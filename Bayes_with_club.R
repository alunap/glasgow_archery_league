library(nimble)
library(dplyr)
library(tidyr)

# 1. Data preparation (example for one bowstyle)
d <- read.csv("all_scores.csv") |>
  mutate(event = factor(event_date),
         archer = factor(archer),
         bowstyle = factor(bowstyle),
         club = factor(club)) |>
  filter(score > 0)

# Pick one bowstyle to model (repeat for "Barebow", "Compound", "Traditional")
sty <- "Recurve"           # Largest group; adjust as needed
d_sub <- d |> filter(bowstyle == sty)

# Archer-level attributes (club fixed per archer)
archer_info <- d_sub |>
  group_by(archer) |>
  summarise(club = first(club), .groups = "drop")

# Indices
archer_idx <- as.integer(d_sub$archer)
event_idx  <- as.integer(d_sub$event)
club_archer <- as.integer(factor(archer_info$club))

N_obs    <- nrow(d_sub)
N_archer <- nlevels(d_sub$archer)
N_event  <- nlevels(d_sub$event)
N_club   <- nlevels(factor(archer_info$club))

# Participation frequencies (for weighted sampling in prediction)
archer_freq <- table(d_sub$archer) / N_obs   # Normalize to probs
club_freq   <- table(club_archer) / N_archer # For new archers

# Constants and data list
constants <- list(
  N_obs       = N_obs,
  N_archer    = N_archer,
  N_event     = N_event,
  N_club      = N_club,
  archer      = archer_idx,
  event       = event_idx,
  club_archer = club_archer
)

data <- list(y = d_sub$score)

# 2. NIMBLE model code
archery_code <- nimbleCode({
  
  # Grand mean (location) ────────────────
  mu ~ dnorm(mean = 480, sd = 120)         # Adjust prior mean per bowstyle if needed (e.g., 550 for Compound)
  
  # Variance components ──────────────────
  tau_archer ~ T(dt(0, pow(0.02, -0.5), 1), 0, )   # Half-t for archer SD
  tau_event  ~ T(dt(0, pow(0.04, -0.5), 1), 0, )   # Smaller for tournament
  tau_club   ~ T(dt(0, pow(0.03, -0.5), 1), 0, )   # New: for club SD
  sigma      ~ T(dt(0, pow(0.04, -0.5), 1), 0, )   # Residual
  
  nu ~ dunif(2, 30)                          # Student-t df for robustness
  
  # Random effects ───────────────────────
  for (k in 1:N_club) {
    z_club[k] ~ dnorm(0, 1)
    beta_club[k] <- tau_club * z_club[k]     # Club effects (centered at 0)
  }
  
  for (j in 1:N_archer) {
    z_archer[j] ~ dnorm(0, 1)
    mu_archer[j] <- mu + beta_club[club_archer[j]] + tau_archer * z_archer[j]
  }
  
  for (t in 1:N_event) {
    z_event[t] ~ dnorm(0, 1)
    alpha_event[t] <- tau_event * z_event[t]
  }
  
  # Likelihood ───────────────────────────
  for (i in 1:N_obs) {
    mu_i[i] <- mu_archer[archer[i]] + alpha_event[event[i]]
    y[i] ~ T(dt(mu = mu_i[i], tau = 1/(sigma*sigma), df = nu), 0, 600)  # Truncated to [0,600]
  }
  
  # Derived quantities (optional: variance proportions)
  var_club   <- pow(tau_club, 2)
  var_archer <- pow(tau_archer, 2)
  var_event  <- pow(tau_event, 2)
  var_resid  <- pow(sigma, 2)
  prop_club  <- var_club / (var_club + var_archer + var_event + var_resid)
})

# 3. Build & compile the model
archery_model <- nimbleModel(
  archery_code,
  constants  = constants,
  data       = data,
  inits      = list(
    mu         = mean(d_sub$score, na.rm=TRUE),
    tau_archer = sd(d_sub$score)*0.5,
    tau_event  = sd(d_sub$score)*0.2,
    tau_club   = sd(d_sub$score)*0.3,
    sigma      = sd(d_sub$score)*0.3,
    nu         = 8,
    z_archer   = rep(0, N_archer),
    z_event    = rep(0, N_event),
    z_club     = rep(0, N_club)
  )
)

C_archery <- compileNimble(archery_model)

# 4. Configure & run MCMC
mcmc_conf <- configureMCMC(
  archery_model,
  monitors = c("mu", "tau_archer", "tau_event", "tau_club", "sigma", "nu",
               "beta_club", "mu_archer", "alpha_event", "prop_club"),
  thin = 1, useConjugacy = TRUE
)

mcmc_build <- buildMCMC(mcmc_conf)
C_mcmc <- compileNimble(mcmc_build, project = archery_model)

# Run (increase iter/burnin for convergence; check with coda::gelman.diag)
C_mcmc$run(niter = 30000, nburnin = 6000, progressBar = TRUE)

# Extract samples
samples <- as.matrix(C_mcmc$mvSamples)

# Diagnostics (use coda for traces, ESS, Rhat)
library(coda)
mcmc_samples <- mcmc(samples)
summary(mcmc_samples)
plot(mcmc_samples[, c("mu", "tau_club", "sigma", "nu")])

# 5. Posterior predictive simulation ───────────────────────────────

# Function to simulate one future tournament (per bowstyle)
simulate_next_tournament <- function(samples, M = 35, p_known = 0.75, nsim = 1000,
                                     archer_freq, club_freq, N_archer, N_club) {
  
  nsamples <- nrow(samples)
  pred_scores <- matrix(NA, nsim, M)
  
  for (s in 1:nsim) {
    idx <- sample(1:nsamples, 1)
    draw <- samples[idx, ]
    
    # New tournament effect
    alpha_new <- rnorm(1, 0, draw["tau_event"])
    
    # Participants: known vs new
    n_known <- rbinom(1, M, p_known)
    n_new   <- M - n_known
    
    # Sample known archers (weighted by freq, no replacement for realism)
    known_ids <- sample(1:N_archer, n_known, replace = FALSE, prob = archer_freq)
    
    # Their means (include club/archer effects)
    theta_known <- draw[paste0("mu_archer[", known_ids, "]")]
    
    # New archers: sample clubs, then draw means
    new_clubs <- sample(1:N_club, n_new, replace = TRUE, prob = club_freq)
    theta_new <- sapply(new_clubs, function(k) {
      draw["mu"] + draw[paste0("beta_club[", k, "]")] + rnorm(1, 0, draw["tau_archer"])
    })
    
    theta_all <- c(theta_known, theta_new)
    
    # Generate scores
    y_pred <- rt(length(theta_all), df = draw["nu"]) * draw["sigma"] + theta_all + alpha_new
    
    # Bounds enforced (though truncation in likelihood minimizes issues)
    y_pred <- pmax(0, pmin(600, round(y_pred)))
    
    pred_scores[s, ] <- y_pred
  }
  
  pred_scores
}

# Example: simulate 2000 predictive tournaments of size 35 (adjust M to style-specific avg, e.g., ~23 for Recurve)
pred_scores <- simulate_next_tournament(samples, M = 35, p_known = 0.75, nsim = 2000,
                                        archer_freq = archer_freq, club_freq = club_freq,
                                        N_archer = N_archer, N_club = N_club)

# Summarise score spread (posterior predictive)
quantile(as.vector(pred_scores), probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), na.rm = TRUE)
hist(as.vector(pred_scores), breaks = 50, main = paste("Predicted Scores:", sty))

# Badge probabilities (load badges.csv and filter)
badges <- read.csv("badges.csv") |> filter(bowstyle == sty)
thr <- setNames(badges$minimum, badges$badge)

prob_badge_claimed <- sapply(thr, function(t) {
  mean(apply(pred_scores, 1, function(row) any(row >= t, na.rm = TRUE)))
})

round(prob_badge_claimed * 100, 1)   # % chance each badge is claimed (at least one archer achieves it)
