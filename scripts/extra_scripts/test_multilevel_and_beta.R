# setwd("~/programming/r-projects/water_conservation/water_conserve/VWCI_May2016/PNAS_Paper")
source('scripts/load_data.R', chdir = T)
# source('scripts/fit_model.R')

library(tidyverse, quietly = TRUE)
library(stringr, quietly = TRUE)
library(broom, quietly = TRUE)

library(rstan, quietly = TRUE)
library(shinystan, quietly = TRUE)
library(loo, quietly = TRUE)
library(jgmcmc, quietly = TRUE)

options(mc.cores = parallel::detectCores())

invlogit <- arm::invlogit

rstan_options(auto_write = TRUE)

data_dir <- 'data'

n_req <- 31
n_reb <- 21
n_other <- 16
n_billing <- 6
n_drought <- 5

n_actions <- 79 # reduce to 77 if using desal

desal_msas <- c('Brownsville, TX', 'Cape Coral, FL', 'El Paso, TX', 'Jacksonville, FL',
                  'Laredo, TX', 'Oxnard, CA', 'San Diego, CA', 'Tampa, FL')

std_data <- read_rds(file.path(data_dir, 'standardized_data.Rds'))

parse_target <- function(target = c('vwci', 'req', 'reb'), k_actions = NA) {
  target = match.arg(target)

  if (is.na(k_actions) || ! is.numeric(k_actions)) {
    k_actions <- c(vwci = n_actions, req = n_req, reb = n_reb)[target]
  }
  if (target %in% c('reb', 'req'))
    target <- paste0(target, 'total')

  list(target = target, k_actions = k_actions)
}

msa_vars <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
              'aridity','surface.water')

state_vars <- c('state.pvi', 'state.rpi',
                'state.aridity', 'state.surface.water')

ml_vars <- list(msa_vars = msa_vars, state_vars = state_vars)

sl_vars <- list(msa_vars = msa_vars)

fit_model <- function(df, vars, target = c('vwci', 'req', 'reb'), k_actions = NA,
                      mu_phi = 40, sigma_phi = 15,
                      multilevel = TRUE,
                      beta = FALSE, random_alpha = FALSE,
                      remove_desal = FALSE) {
  target <- match.arg(target)
  targact <- parse_target(target, k_actions)
  target <- targact$target
  k_actions <- targact$k_actions
  rm(targact)


  msa_data <- df$msa_data %>%
    mutate(state = factor(state), state.index = as.integer(state))
  if (remove_desal && target == 'vwci') {
    msa_data <- msa_data %>% mutate(vwci = ifelse(msa %in% desal_msas, vwci - 1, vwci))
    k_actions = k_actions - 2
  }

  if (multilevel) {
    state_data <- df$state_data %>% filter(state %in% levels(msa_data$state)) %>%
      mutate(state = factor(state), state.index = as.integer(state)) %>%
      arrange(state.index)

    if (random_alpha) {
      f_msa <- as.formula(paste0('~', paste(vars$msa_vars, collapse = ' + '), ' - 1'))
      f_state <- as.formula(paste0('~', paste(vars$state_vars, collapse = ' + '), '-  1'))
    } else {
      f_msa <- as.formula(paste0('~', paste(vars$msa_vars, collapse = ' + '), ' - 1'))
      f_state <- as.formula(paste0('~', paste(vars$state_vars, collapse = ' + ')))
    }

    mm_msa <- model.matrix(f_msa, msa_data)
    mm_state <- model.matrix(f_state, state_data)

    group <- msa_data$state.index
  } else {
    f_msa <- as.formula(paste0('~', paste(vars$msa_vars, collapse = ' + ')))
    mm_msa <- model.matrix(f_msa, msa_data)
  }

  y <- msa_data[,target] %>% unlist()

  if (multilevel) {
    if (beta) {
      if (random_alpha) {
        model <- stan_model('scripts/water_conserve_4_ml_vary_intercept_beta.stan',
                            model_name = "Basic hierarchical beta-binomial regression with varying intercepts and random alpha")
      } else {
        model <- stan_model('scripts/water_conserve_3_ml_vary_intercept_beta.stan',
                            model_name = "Basic hierarchical beta-binomial regression with varying intercepts")
      }
    } else {
      model <- stan_model('scripts/water_conserve_3_ml_vary_intercept.stan',
                          model_name = "Basic hierarchical binomial logistic regression with varying intercepts")
    }

    #  model_b <- stan_model('scripts/water_conserve_3b_ml_vary_intercept.stan',
    #                      model_name = "Basic hierarchical regression with varying intercepts")


    stan_data <- list(N = nrow(mm_msa), K = ncol(mm_msa),
                      M = nrow(mm_state), J = ncol(mm_state),
                      x = mm_msa, w = mm_state, y = y,
                      group = group,
                      K_ACTIONS = k_actions) # there are 78 possible actions, maximum VWCI is 55.

  } else {
    if (beta) {
      model <- stan_model('scripts/water_conserve_3_sl_beta.stan',
                          model_name = "Basic single-level beta-binomial regression")

    } else {
      model <- stan_model('scripts/water_conserve_3_sl.stan',
                          model_name = "Basic single-level binomial logistic regression")
    }

    stan_data <- list(N = nrow(mm_msa), K = ncol(mm_msa),
                      x = mm_msa, y = y,
                      K_ACTIONS = k_actions) # there are 78 possible actions, maximum VWCI is 55.
  }

  if (beta) {
    stan_data <- c(stan_data, mu_phi = mu_phi, sigma_phi = sigma_phi)
  }

  sfit <- sampling(model, stan_data,
                   chains = 4, iter = 2000)
  invisible(sfit)
}

compare_models <- function(data.dir = data_dir) {
  processed_data <- process_data(data.dir, scale_factor = 2.0, pooled = FALSE, save_data = FALSE)

  sl_data <- standardize_msa_data_pooled(processed_data$filtered_data$msa_data, scale_factor = 2.0)
  ml_data <- processed_data$standardized_data

  sfit_sl_beta  <- fit_model(sl_data, sl_vars, 'vwci', beta = TRUE, remove_desal = FALSE, multilevel = FALSE)
  sfit_ml_beta  <- fit_model(ml_data, ml_vars, 'vwci', beta = TRUE, remove_desal = FALSE, multilevel = TRUE)
  sfit_ml_binom <- fit_model(ml_data, ml_vars, 'vwci', beta = FALSE, remove_desal = FALSE, multilevel = TRUE)

  print(rethinking::compare(sfit_sl_beta, sfit_ml_beta, sfit_ml_binom))
  invisible(list(sl_data = sl_data, ml_data = ml_data, sfit_sl_beta = sfit_sl_beta,
                 sfit_ml_beta = sfit_ml_beta, sfit_ml_binom = sfit_ml_binom))
}
