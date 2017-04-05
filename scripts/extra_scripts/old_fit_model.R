library(hellno)
library(tidyverse)
library(stringr)

library(rstan)
library(shinystan)
library(loo)
library(ggmcmc)

options(mc.cores = parallel::detectCores())

invlogit <- arm::invlogit

rstan_options(auto_write = TRUE)

all_vars <- c('pvi', 'log.pop', 'pop.growth', 'drought', 'aridity',
                  'surface.water', 'rpp', 'rpi', 'CA', 'TX', 'FL')

default_vars <- all_vars %>% discard(~.x %in% c('rpi','CA','TX','FL'))

fit_model_vary_intercept <- function(df, vars = default_vars, states = c('CA','FL','TX'),
                                     k_actions = 79, script_dir = 'scripts') {
  model <- stan_model(file.path(script_dir,'water_conserve_2_ml_vary_intercept.stan'),
                                model_name = "Multilevel: Vary intercept")

  df$group <- as.character(df$state)
  df$group[! df$group %in% states] <- 'Other'
  df$group <- ordered(df$group, levels = c(states, 'Other'))

  stan_data <- list(
    J = length(vars),
    N = nrow(df),
    M = nlevels(df$group),
    K_ACTIONS = k_actions,
    x = select_(df, .dots = vars),
    group = as.integer(df$group),
    y = df$vwci
  )
  fit = sampling(model, data = stan_data, chains = 4, iter = 2000)
  invisible(fit)
}

fit_model_vary_slope_intercept <- function(df, vars = default_vars, states = c('CA','FL','TX'),
                                           k_actions = 79, script_dir = 'scripts') {
  model <- stan_model(file.path(script_dir,'water_conserve_2_ml_vary_slope_intercept.stan'),
                                model_name = "Multilevel: Vary slopes and intercept")

  df$group <- as.character(df$state)
  df$group[! df$group %in% states] <- 'Other'
  df$group <- ordered(df$group, levels = c(states, 'Other'))

  stan_data <- list(
    J = length(vars),
    N = nrow(df),
    M = nlevels(df$group),
    K_ACTIONS = k_actions,
    x = select_(df, .dots = vars),
    group = as.integer(df$group),
    y = df$vwci
  )
  fit = sampling(model, data = stan_data, chains = 4, iter = 2000)
  invisible(fit)
}

fit_model_vary_slope_intercept_corr <- function(df, vars = default_vars, states = c('CA','FL','TX'),
                                           k_actions = 79, script_dir = 'scripts',
                                           accept = 0.90, max_treedepth = 12) {
  model <- stan_model(file.path(script_dir,'water_conserve_2_ml_vary_slope_intercept_corr.stan'),
                      model_name = "Multilevel: Vary slopes and intercept")

  df$group <- as.character(df$state)
  df$group[! df$group %in% states] <- 'Other'
  df$group <- ordered(df$group, levels = c(states, 'Other'))
  df$unit <- 1

  stan_data <- list(
    N = nrow(df),
    K = length(vars) + 1,
    J = nlevels(df$group),
    L = 1,
    K_ACTIONS = k_actions,
    x = select_(df, .dots = c('unit',vars)),
    u = matrix(1, nrow = nlevels(df$group), ncol = 1),
    group = as.integer(df$group),
    y = df$vwci
  )
  fit = sampling(model, data = stan_data, chains = 4, iter = 2000,
                 control = list(adapt_delta = accept, max_treedepth = max_treedepth))
  invisible(fit)
}


calc_residuals_vary_slope_intercept <- function(vwci, sfit, vars, states = c('CA','FL','TX'), k_actions = 79) {
  vwci$group <- vwci$state
  vwci$group[ !vwci$group  %in% states ] <- 'Other'
  vwci$group <- ordered(vwci$group, levels = c(states, 'Other'))
  params <- summary(sfit)$summary %>% as_data_frame()
  params$name = rownames(params)
  params <- params %>% select(name, mean) %>% filter(name != 'lp__')
  for(i in 1:nlevels(vwci$group)) {
    st <- levels(vwci$group)[i]
    params$name[params$name == paste0('alpha[',i,']')] <- paste0('alpha[', st, ']')
    for(j in 1:length(vars)) {
      params$name[params$name == paste0('beta[',j,',',i,']')] <- paste0('beta[', vars[j], ',', st, ']')
    }
  }
  vwci$predicted <- 0
  for (i in seq_len(nrow(vwci))) {
    xr <- vwci[i,]
    grp <- xr$group
    mu <- params$mean[params$name == paste0('alpha[',grp,']')]
    for (v in vars) {
      x <- select_(xr,v) %>% unlist()
      beta <- params$mean[params$name == paste0('beta[',v,',',grp,']')]
      mu <- mu + x * beta
    }
    theta <- invlogit(mu)
    vwci$predicted[i] <- theta * k_actions
  }
  vwci <- vwci %>% mutate(residual = vwci - predicted)
  invisible(vwci)
}

calc_residuals_vary_intercept <- function(vwci, sfit, vars, states = c('CA','FL','TX'), k_actions = 79) {
  vwci$group <- vwci$state
  vwci$group[ !vwci$group  %in% states ] <- 'Other'
  vwci$group <- ordered(vwci$group, levels = c(states, 'Other'))
  params <- summary(sfit)$summary %>% as_data_frame()
  params$name = rownames(params)
  params <- params %>% select(name, mean) %>% filter(name != 'lp__')
  for(i in 1:nlevels(vwci$group)) {
    st <- levels(vwci$group)[i]
    params$name[params$name == paste0('alpha[',i,']')] <- paste0('alpha[', st, ']')
    for(j in 1:length(vars)) {
      params$name[params$name == paste0('beta[',j,',',i,']')] <- paste0('beta[', vars[j], ',', st, ']')
    }
  }
  vwci$predicted <- 0
  for (i in seq_len(nrow(vwci))) {
    xr <- vwci[i,]
    grp <- xr$group
    mu <- params$mean[params$name == paste0('alpha[',grp,']')]
    for (v in vars) {
      x <- select_(xr,v) %>% unlist()
      beta <- params$mean[params$name == paste0('beta[',v,']')]
      mu <- mu + x * beta
    }
    theta <- invlogit(mu)
    vwci$predicted[i] <- theta * k_actions
  }
  vwci <- vwci %>% mutate(residual = vwci - predicted)
  invisible(vwci)
}

make_ggs_var_intercept <- function(sfit, vars, states = c('CA','FL','TX'), family = NA) {
  grps <- ordered(c(states, 'Other'), levels = c(states, 'Other'))
  g <- ggs(sfit, family = family)
  xlate <- data_frame(Parameter = levels(g$Parameter), Label = levels(g$Parameter),
                      stringsAsFactors = FALSE)
  for(i in seq_along(grps)) {
    xlate$Label[xlate$Label == paste0('alpha[', i, ']')] <-
      paste0('alpha[',as.character(grps[i]), ']')
    xlate$Label[xlate$Label == paste0('alpha_raw[', i, ']')] <-
      paste0('alpha[',as.character(grps[i]), '.raw]')
  }
  for(j in seq_along(vars)) {
    xlate$Label[xlate$Label == paste0('beta[', j, ']')] <- paste0('beta[',vars[j], ']')
  }
  g <- suppressWarnings(ggs(sfit, family = family, par_labels = xlate))
  invisible(g)
}

make_ggs_var_slope_intercept <- function(sfit, vars, states = c('CA','FL','TX'), family = NA) {
  grps <- ordered(c(states, 'Other'), levels = c(states, 'Other'))
  g <- ggs(sfit, family = family)
  xlate <- data_frame(Parameter = levels(g$Parameter), Label = levels(g$Parameter),
                      stringsAsFactors = FALSE)
  for(i in seq_along(grps)) {
    xlate$Label[xlate$Label == paste0('alpha[', i, ']')] <-
      paste0('alpha[',as.character(grps[i]), ']')
    xlate$Label[xlate$Label == paste0('alpha_raw[', i, ']')] <-
      paste0('alpha[',as.character(grps[i]), '.raw]')
    for(j in seq_along(vars)) {
      xlate$Label[xlate$Label == paste0('beta[', j, ',', i, ']')] <-
        paste0('beta[', vars[j], '.', as.character(grps[i]), ']')
      xlate$Label[xlate$Label == paste0('beta_raw[', j, ',', i, ']')] <-
        paste0('beta[', vars[j], '.', as.character(grps[i]), '.raw]')
      xlate$Label[xlate$Label == paste0('mu_beta[', j, ']')] <-
        paste0('mu[beta.', vars[j], ']')
    }
  }
  g <- suppressWarnings(ggs(sfit, family = family, par_labels = xlate))
  invisible(g)
}

make_ggs_var_slope_intercept_corr <- function(sfit, vars, states = c('CA','FL','TX'), family = NA) {
  grps <- ordered(c(states, 'Other'), levels = c(states, 'Other'))
  vars <- c('const', vars)
  g <- ggs(sfit, family = family)
  xlate <- data_frame(Parameter = levels(g$Parameter), Label = levels(g$Parameter),
                      stringsAsFactors = FALSE)
  for(i in seq_along(grps)) {
    for(j in seq_along(vars)) {
      xlate$Label[xlate$Label == paste0('beta[', i, ',', j, ']')] <-
        paste0('beta[', vars[j], '.', as.character(grps[i]), ']')
    }
  }
  xlate$Label <- str_replace_all(xlate$Label, 'beta\\[const.', 'alpha\\[')
  g <- suppressWarnings(ggs(sfit, family = family, par_labels = xlate))
  invisible(g)
}
