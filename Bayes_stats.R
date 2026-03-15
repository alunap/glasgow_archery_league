library(nimble)
library(dplyr)
library(tidyr)

# 1. Data preparation (example for one bowstyle)
d <- read.csv("all_scores.csv") |>
  mutate(event = factor(event_date),
         archer = factor(archer),
         bowstyle = factor(bowstyle)) |>
  filter(score > 0)
# Pick one bowstyle to model (repeat for others)
sty <- "Recurve"           # change to "Barebow", "Compound", etc.
d_sub <- d |> filter(bowstyle == sty)

# Create integer indices
archer_idx <- as.integer(d_sub$archer)
event_idx  <- as.integer(d_sub$event)

N_obs    <- nrow(d_sub)
N_archer <- nlevels(d_sub$archer)
N_event  <- nlevels(d_sub$event)

# Constants and data list
constants <- list(
  N_obs    = N_obs,
  N_archer = N_archer,
  N_event  = N_event,
  archer   = archer_idx,
  event    = event_idx,
  y        = d_sub$score
)

data <- list(y = d_sub$score)   # will be overwritten in model

# 2. NIMBLE model code
archery_code <- nimbleCode({
  
  # Grand mean (location) ────────────────
  mu ~ dnorm(mean = 480, sd = 120)         # informative for most styles
  
  # Variance components ──────────────────
  tau_archer ~ T(dt(0, pow(0.02, -0.5), 1), 0, )   # half-t ≈ half-Cauchy
  tau_event  ~ T(dt(0, pow(0.04, -0.5), 1), 0, )   # smaller tournament effect
  sigma      ~ T(dt(0, pow(0.04, -0.5), 1), 0, )   # residual
  
  nu ~ dunif(2, 30)                          # student-t df (robustness)
  
  # Random effects ───────────────────────
  for (j in 1:N_archer) {
    z_archer[j] ~ dnorm(0, 1)
    mu_archer[j] <- mu + tau_archer * z_archer[j]
  }
  
  for (t in 1:N_event) {
    z_event[t] ~ dnorm(0, 1)
    alpha_event[t] <- tau_event * z_event[t]
  }
  
  # Likelihood ───────────────────────────
  for (i in 1:N_obs) {
    mu_i[i] <- mu_archer[archer[i]] + alpha_event[event[i]]
    y[i] ~ dt(mu = mu_i[i], tau = 1/(sigma*sigma), df = nu)
    
    # Soft truncation via post-calc (or use truncated student if needed)
    # For strict [0,600] bounds, one can add a custom truncated_dstudent
  }
  
  # Derived / monitored quantities
  for (i in 1:N_obs) {
    resid[i] <- (y[i] - mu_i[i]) / sigma
  }
})

# 3. Build & compile the model
archery_model <- nimbleModel(
  archery_code,
  constants  = constants,
  data       = data,
  inits      = list(
    mu         = mean(d_sub$score, na.rm=TRUE),
    tau_archer = sd(d_sub$score)*0.8,
    tau_event  = sd(d_sub$score)*0.3,
    sigma      = sd(d_sub$score)*0.4,
    nu         = 8,
    z_archer   = rep(0, N_archer),
    z_event    = rep(0, N_event)
  )
)

C_archery <- compileNimble(archery_model)

# 4. Configure & run MCMC
mcmc_conf <- configureMCMC(
  archery_model,
  monitors = c("mu", "tau_archer", "tau_event", "sigma", "nu",
               "mu_archer", "alpha_event"),
  thin = 1, useConjugacy = TRUE
)

mcmc_build <- buildMCMC(mcmc_conf)
C_mcmc <- compileNimble(mcmc_build, project = archery_model)

# Run (adjust niter / nburnin as needed)
C_mcmc$run(niter = 25000, nburnin = 5000, progressBar = TRUE)

# Extract samples
samples <- as.matrix(C_mcmc$mvSamples)

# Quick diagnostics
library(coda)
plot(mcmc(samples[, c("mu", "tau_archer", "sigma")]))

# 5. Posterior predictive simulation ───────────────────────────────

# Function to simulate one future tournament
simulate_next_tournament <- function(samples, M = 35, p_known = 0.75, nsim = 1000) {
  
  nsamples <- nrow(samples)
  pred_scores <- matrix(NA, nsim, M)
  
  for (s in 1:nsim) {
    idx <- sample(1:nsamples, 1)
    draw <- samples[idx, ]
    
    # New tournament effect
    alpha_new <- rnorm(1, 0, draw["tau_event"])
    
    # Approximate participant pool
    n_known <- rbinom(1, M, p_known)
    n_new   <- M - n_known
    
    # Known archers: sample from fitted posterior random effects
    theta_known <- rnorm(n_known, 0, draw["tau_archer"])
    
    # New archers: from population
    theta_new   <- rnorm(n_new, 0, draw["tau_archer"])
    
    theta_all   <- c(theta_known, theta_new)
    
    # Generate scores
    mu_pred <- draw["mu"] + theta_all + alpha_new
    y_pred  <- rt(length(mu_pred), df = draw["nu"]) * draw["sigma"] + mu_pred
    
    # Enforce bounds (soft truncation)
    y_pred <- pmax(0, pmin(600, round(y_pred)))
    
    pred_scores[s, 1:length(y_pred)] <- y_pred
  }
  
  pred_scores
}

# Example: simulate 2000 predictive tournaments of size 35
pred_recurve <- simulate_next_tournament(samples, M = 35, p_known = 0.75, nsim = 2000)

# Summarise score spread
quantile(pred_recurve, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), na.rm=TRUE)

# Badge probabilities (load badges.csv and filter for bowstyle)
badges <- read.csv("badges.csv") |> filter(bowstyle == "Recurve")
thr <- setNames(badges$minimum, badges$badge)

prob_badge_claimed <- sapply(thr, function(t) {
  mean(apply(pred_recurve, 1, function(row) any(row >= t, na.rm=TRUE)))
})

round(prob_badge_claimed * 100, 1)   # in %
