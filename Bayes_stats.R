library(nimble)
library(dplyr)
library(coda)

# 1. Data preparation ────────────────────────────────────────────────
d <- read.csv("all_scores.csv") |>
  mutate(
    event_date = as.Date(event_date),
    archer     = as.factor(archer),
    bowstyle   = as.factor(bowstyle),
    event      = as.factor(event_date)
  )

# Drop no-shows
d <- d |> filter(score > 0)

# Example: focus on Recurve (largest group)
d_sub <- d |> filter(bowstyle == "Recurve")

# Indices (1-based for NIMBLE)
archer_idx   <- as.integer(d_sub$archer)
event_idx    <- as.integer(d_sub$event)
N_obs        <- nrow(d_sub)
N_archer     <- nlevels(d_sub$archer)
N_event      <- nlevels(d_sub$event)

# Constants & data list
constants <- list(
  N_obs     = N_obs,
  N_archer  = N_archer,
  N_event   = N_event,
  archer    = archer_idx,
  event     = event_idx
)

data <- list(
  score = d_sub$score
)

# 2. NIMBLE model ─────────────────────────────────────────────────────
code <- nimbleCode({
  
  # Hyperpriors ────────────────────────────────────────
  mu        ~ dnorm(480, sd = 80)          # central tendency (Recurve-ish)
  tau_arch  ~ T(dnorm(0, sd = 80), 0, )    # between-archer SD
  tau_event ~ T(dnorm(0, sd = 40), 0, )    # between-event SD
  sigma     ~ T(dnorm(0, sd = 50), 0, )    # residual
  
  # Varying effects ────────────────────────────────────
  for (i in 1:N_archer) {
    z_arch[i]   ~ dnorm(0, 1)              # standard normal
    theta[i]  <- mu + tau_arch * z_arch[i]
  }
  
  for (t in 1:N_event) {
    z_event[t]  ~ dnorm(0, 1)
    alpha[t]  <- tau_event * z_event[t]
  }
  
  # Likelihood ─────────────────────────────────────────
  for (i in 1:N_obs) {
    score[i] ~ dt(mu    = theta[archer[i]] + alpha[event[i]],
                  tau   = 1 / sigma^2,
                  df    = 4)               # nu=4 → reasonably heavy tails
  }
  
  # Derived / monitoring (optional)
  log_lik[1:N_obs] <- score[1:N_obs] - score[1:N_obs]   # placeholder if needed
})

# 3. Build & compile ──────────────────────────────────────────────────
modelInfo <- list(code = code, constants = constants, data = data)

nimMod <- nimbleModel(code, constants, data)

Cmod <- compileNimble(nimMod)

# 4. Configure & build MCMC ───────────────────────────────────────────
monitors <- c("mu", "tau_arch", "tau_event", "sigma",
              "theta", "alpha")   # theta = archer abilities

mcmcConf <- configureMCMC(nimMod, monitors = monitors,
                          enableWAIC = FALSE, print = TRUE)

mcmcBuild <- buildMCMC(mcmcConf)
Cmcmc <- compileNimble(mcmcBuild, project = nimMod)

# 5. Run MCMC
set.seed(123)
Cmcmc$run(niter = 25000, nburnin = 5000, thin = 10, progress = TRUE)

samples <- as.matrix(Cmcmc$mvSamples)

# Quick check
summary(samples[, c("mu", "tau_arch", "tau_event", "sigma")])

# 6. Posterior predictive simulation for next tournament ───────────────

predict_next_nimble <- function(samples, nsim = 1000, M = 35, p_known = 0.75,
                                badge_thresholds = c(Pink=330, Green=395, White=450,
                                                     Black=495, Blue=525, Red=550,
                                                     Gold=570, Purple=585)) {
  
  nsamples <- nrow(samples)
  idx <- sample(1:nsamples, nsim, replace = TRUE)  # with replacement ok
  
  results <- matrix(NA, nsim, length(badge_thresholds) + 4)
  colnames(results) <- c("mean_score", "max_score", "sd_score", "n_above_500",
                         names(badge_thresholds))
  
  for (s in 1:nsim) {
    row <- samples[idx[s], ]
    
    mu_s        <- row["mu"]
    tau_arch_s  <- row["tau_arch"]
    tau_event_s <- row["tau_event"]
    sigma_s     <- row["sigma"]
    
    # New tournament effect
    alpha_new <- rnorm(1, 0, tau_event_s)
    
    # Approximate participant pool
    n_known <- rbinom(1, M, p_known)
    n_new   <- M - n_known
    
    # Known archers ≈ draw from fitted posterior random effects
    theta_known <- rnorm(n_known, 0, tau_arch_s)   # centered
    
    # Completely new archers from population
    theta_new   <- rnorm(n_new, 0, tau_arch_s)
    
    theta_all <- c(theta_known, theta_new)
    
    # Generate scores (Student-t)
    y <- mu_s + theta_all + alpha_new + sigma_s * rt(length(theta_all), df = 4)
    
    # Clip to realistic range (rarely triggered)
    y <- pmax(0, pmin(600, round(y)))
    
    # Summaries
    results[s, "mean_score"]    <- mean(y)
    results[s, "max_score"]     <- max(y)
    results[s, "sd_score"]      <- sd(y)
    results[s, "n_above_500"]   <- sum(y >= 500)
    
    # Badge claims (at least one archer reaches threshold)
    for (b in seq_along(badge_thresholds)) {
      results[s, names(badge_thresholds)[b]] <- as.integer(any(y >= badge_thresholds[b]))
    }
  }
  
  return(results)
}

# Run prediction
pred <- predict_next_nimble(samples, nsim = 2000, M = 35, p_known = 0.75)

# Summarise
round(colMeans(pred) * 100, 1)[5:ncol(pred)]     # % of sims where badge claimed
apply(pred[,5:ncol(pred)], 2, quantile, probs = c(0.05,0.5,0.95))

# Visuals
hist(pred[,"max_score"], breaks = 40, main = "Predicted max score (next event)")
