suppressPackageStartupMessages(library(hellno, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
suppressPackageStartupMessages(library(broom, quietly = TRUE))
suppressPackageStartupMessages(library(RColorBrewer, quietly = TRUE))

suppressPackageStartupMessages(library(rstan, quietly = TRUE))
# suppressPackageStartupMessages(library(shinystan, quietly = TRUE))
# suppressPackageStartupMessages(library(loo, quietly = TRUE))
library(jgmcmc)

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
                'Laredo, TX', 'Oxnard, CA', 'SanDiego, CA', 'Tampa, FL')

std_data <- read_rds(file.path(data_dir, 'standardized_data.Rds'))

msa_vars_1 <- c('pvi', 'rpp', 'log.pop', 'pop.growth',
                'precip', 'temp','surface.water')

state_vars_1 <- c('state.pvi', 'state.rpp',
                  'state.precip', 'state.temp',
                  'state.surface.water')

vars_1 <- list(msa_vars = msa_vars_1, state_vars = state_vars_1)

msa_vars_2 <- c('pvi', 'rpi', 'log.pop', 'pop.growth',
                'aridity','surface.water')

state_vars_2 <- c('state.pvi', 'state.rpi',
                  'state.aridity', 'state.surface.water')

vars_2 <- list(msa_vars = msa_vars_2, state_vars = state_vars_2)

parse_target <- function(target = c('vwci', 'req', 'reb'), k_actions = NA) {
  target = match.arg(target)

  if (is.na(k_actions) || ! is.numeric(k_actions)) {
    k_actions <- c(vwci = n_actions, req = n_req, reb = n_reb)[target]
  }
  if (target %in% c('reb', 'req'))
    target <- paste0(target, 'total')

  list(target = target, k_actions = k_actions)
}

index_states <- function(msa_data) {
  msa_data %>% mutate(state = factor(state), state.index = as.integer(state)) %>%
    invisible()
}


fit_model <- function(df, vars, target = c('vwci', 'req', 'reb'), k_actions = NA,
                      mu_phi = 40, sigma_phi = 15, sigma_sigma_delta = 0.5,
                      multilevel = TRUE,
                      beta = FALSE, random_alpha = FALSE,
                      remove_desal = FALSE,
                      seed = NULL) {
  target <- match.arg(target)
  targact <- parse_target(target, k_actions)
  target <- targact$target
  k_actions <- targact$k_actions
  rm(targact)

  # message("Running fit_model: vars = ", str_c(vars, collapse = ", "))

  msa_data <- index_states(df$msa_data)

  if (remove_desal && target == 'vwci') {
    # message("removing desal")
    msa_data <- msa_data %>% mutate(vwci = ifelse(msa %in% desal_msas, vwci - 1, vwci))
    k_actions = k_actions - 2
  }

  if (multilevel) {
    state_data <- msa_data %>% dplyr::select(state, state.index) %>% distinct() %>%
      mutate(state = as.character(state)) %>%
      left_join(df$state_data, by = 'state') %>% arrange(state.index)

    if (random_alpha) {
      f_msa <- as.formula(paste0('~', paste(vars$msa_vars, collapse = ' + '), ' - 1'))
      f_state <- as.formula(paste0('~', paste(vars$state_vars, collapse = ' + '), ' - 1'))
    } else {
      f_msa <- as.formula(paste0('~', paste(vars$msa_vars, collapse = ' + '), ' - 1'))
      f_state <- as.formula(paste0('~', paste(vars$state_vars, collapse = ' + ')))
    }

    # message("f_msa = ", f_msa, ", f_state = ", f_state)

    mm_msa <- model.matrix(f_msa, msa_data)
    mm_state <- model.matrix(f_state, state_data)

    #message("msa dimensions = (", str_c(dim(mm_msa), collapse = ', '),
    #        "), state dimensions = (", str_c(dim(mm_state), collapse = ", "), ")")

    group <- msa_data$state.index
  } else {
    f_msa <- as.formula(paste0('~', paste(vars$msa_vars, collapse = ' + ')))
    mm_msa <- model.matrix(f_msa, msa_data)
  }

  y <- msa_data[,target] %>% unlist()

  if (multilevel) {
    if (beta) {
      if (random_alpha) {
        mdl <- list(model_source = 'scripts/water_conserve_ml_vary_intercept_beta_alpha.stan',
                    model_name = "Basic hierarchical beta-binomial regression with varying intercepts and random alpha")
      } else {
        mdl <- list(model_source = 'scripts/water_conserve_ml_vary_intercept_beta.stan',
                    model_name = "Basic hierarchical beta-binomial regression with varying intercepts")
      }
    } else {
      if (random_alpha) {
        mdl <- list(model_source = 'scripts/water_conserve_ml_vary_intercept_alpha.stan',
                    model_name = "Basic hierarchical binomial regression with varying intercepts and random alpha")
      } else {
        mdl <- list(model_source = 'scripts/water_conserve_ml_vary_intercept.stan',
                    model_name = "Basic hierarchical binomial regression with varying intercepts")
      }
    }
    stan_data <- list(N = nrow(mm_msa), K = ncol(mm_msa),
                      M = nrow(mm_state), J = ncol(mm_state),
                      xx = mm_msa, w = mm_state, y = y,
                      group = group,
                      K_ACTIONS = k_actions) # there are 78 possible actions, maximum VWCI is 55.

  } else {
    if (beta) {
      mdl <- list(model_source = 'scripts/water_conserve_sl_beta.stan',
                  model_name = "Basic single-level beta-binomial logistic regression")
    } else {
      mdl <- list(model_source = 'scripts/water_conserve_sl.stan',
                  model_name = "Basic single-level binomial logistic regression")
    }
    stan_data <- list(N = nrow(mm_msa), K = ncol(mm_msa),
                      xx = mm_msa, y = y,
                      K_ACTIONS = k_actions) # there are 78 possible actions, maximum VWCI is 55.
  }

  if (beta) {
    stan_data <- c(stan_data, mu_phi = mu_phi, sigma_phi = sigma_phi)
  }
  if (random_alpha) {
    stan_data <- c(stan_data, sig_sig_delta = sigma_sigma_delta)
  }

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  # message("Preparing to sample from model ", mdl$model_source)
  # message("Variables: ", str_c(names(stan_data), collapse = ", "))

  model <- stan_model(mdl$model_source, model_name = mdl$model_name)

  stan_env <- new.env(parent = emptyenv())
  stan_env$model <- force(model)
  stan_env$stan_data <- force(stan_data)
  stan_env$seed <- force(seed)
  attach(stan_env)


  sfit <- sampling(force(model), force(stan_data),
                   chains = 4, iter = 2000,
                   seed = seed)
  detach(stan_env)
  invisible(sfit)
}

calc_residuals <- function(df, sfit, vars,
                           target = c('vwci', 'req', 'reb'),
                           k_actions = NA,
                           random_alpha = FALSE,
                           remove_desal = FALSE) {
  targact <- parse_target(target, k_actions)
  target <- targact$target
  k_actions <- targact$k_actions
  rm(targact)

  msa_vars <- vars$msa_vars
  state_vars <- vars$state_vars

  msa_data <- df$msa_data %>%
    mutate(state = factor(state), state.index = as.integer(state))
  state_data <- df$state_data %>% dplyr::filter(state %in% levels(msa_data$state)) %>%
    mutate(state = factor(state), state.index = as.integer(state)) %>%
    arrange(state.index)

  if (remove_desal && target == 'vwci') {
    msa_data <- msa_data %>% mutate(vwci = ifelse(msa %in% desal_msas, vwci - 1, vwci))
    k_actions = k_actions - 2
  }

  params <- sfit %>% tidy(conf.int = FALSE, estimate.method = "median") %>%
    dplyr::select(name = term, median = estimate)
  if (random_alpha) {
    params <- params %>%
      dplyr::filter(name != 'lp__' & ! str_detect(name, '^alpha\\[|^delta_raw'))
  } else {
    params <- params %>%
      dplyr::filter(name != 'lp__' & ! str_detect(name, '^alpha')) %>%
      mutate(name = ifelse(name == 'gamma[1]', 'alpha', name))
  }
  if (random_alpha) {
    index_offset = 0
  } else {
    index_offset = 1
  }
  for(i in seq_along(state_vars)) {
    params <- params %>%
      mutate(name = ifelse(name == paste0('gamma[', i + index_offset, ']'),
                           paste0('gamma[', state_vars[i], ']'), name))
  }
  if (random_alpha) {
    mu_0 <- params$median[params$name == 'alpha_0'] %>%
      rep(nrow(state_data)) %>%
      setNames(state_data$state.name)
    for (i in state_data$state.index) {
      delta <- params$median[params$name == paste0('delta[',i,']')]
      mu_0[i] <- mu_0[i] + delta
    }
  } else {
    mu_0 <- params$median[params$name == 'alpha']
  }
  for(i in seq_along(msa_vars)) {
    params <- params %>%
      mutate(name = ifelse(name == paste0('beta[', i, ']'),
                           paste0('beta[', msa_vars[i], ']'), name))
  }
  msa_data$predicted <- 0
  for (i in seq(nrow(msa_data))) {
    xr <- msa_data[i,]
    grp <- xr$state.index
    wr <- state_data[grp,]
    if (random_alpha) {
      mu <- mu_0[grp]
    } else {
      mu <- mu_0
    }
    for (v in state_vars) {
      w <- dplyr::select_(wr,v) %>% unlist()
      gamma <- params$median[params$name == paste0('gamma[',v,']')]
      mu <- mu + w * gamma
    }
    for (v in msa_vars) {
      x <- dplyr::select_(xr,v) %>% unlist()
      beta <- params$median[params$name == paste0('beta[',v,']')]
      mu <- mu + x * beta
    }
    theta <- invlogit(mu)
    msa_data$predicted[i] <- theta * k_actions
  }
  dots = setNames(list(substitute(y - predicted, list(y = as.name(target)))), list('residual'))
  msa_data <- msa_data %>% mutate_(.dots = dots)
  invisible(list(residuals = msa_data, target = target))
}

make_ggs_var_intercept <- function(sfit, vars, family = 'gamma|beta', abs_rank = FALSE, random_alpha = FALSE) {
  msa_vars <- vars$msa_vars
  state_vars <- vars$state_vars
  if (random_alpha) {
    family <- str_c(family, '|alpha_0')
  }
  g <- ggs(sfit, family = family)
  xlate <- data_frame(Parameter = levels(g$Parameter), Label = levels(g$Parameter),
                      stringsAsFactors = FALSE)
  if (random_alpha) {
    index_offset = 0
  } else {
    index_offset = 1
    xlate$Label[xlate$Parameter == 'gamma[1]'] <- 'alpha'
  }

  for(i in seq_along(state_vars)) {
    xlate$Label[xlate$Parameter == paste0('gamma[', i + index_offset, ']')] <-
      paste0('gamma[plain("',str_replace_all(state_vars[i], c('^state\\.' = '', '\\.' = ' ',
                                                              'pvi' = 'PVI', 'rpi' = 'RPI', 'rpp' = 'RPP')), '")]')
  }
  for(j in seq_along(msa_vars)) {
    xlate$Label[xlate$Parameter == paste0('beta[', j, ']')] <-
      paste0('beta[plain("',str_replace_all(msa_vars[j], c('\\.' = ' ',
                                                           'pvi' = 'PVI', 'rpi' = 'RPI', 'rpp' = 'RPP')), '")]')
  }
  g <- suppressWarnings(ggs(sfit, family = family, par_labels = xlate))
  g_attr <- attributes(g)
  par_list <- g %>% group_by(Parameter) %>% dplyr::summarize(value = mean(value)) %>%
    ungroup() %>%
    mutate( value = if(abs_rank) abs(value) else value,
            Parameter = as.character(Parameter),
            par = str_replace_all(Parameter, '\\[.*$','')) %>%
    arrange(par, value) %>%
    dplyr::select(Parameter) %>% unlist() %>% unname()
  g <- g %>% mutate(Parameter = ordered(Parameter, levels = par_list))
  attributes(g) <- g_attr
  invisible(g)
}

make_city_ggs_var_intercept <- function(sfit, vars, family = 'delta\\[', abs_rank = FALSE, random_alpha = FALSE, states = NULL) {
  msa_vars <- vars$msa_vars
  state_vars <- vars$state_vars
  if (random_alpha) {
    family <- str_c(family, '|alpha_0')
  }
  g <- ggs(sfit, family = family)
  xlate <- data_frame(Parameter = levels(g$Parameter), Label = levels(g$Parameter),
                      stringsAsFactors = FALSE)
  if (random_alpha) {
    index_offset = 0
  } else {
    index_offset = 1
    xlate$Label[xlate$Parameter == 'gamma[1]'] <- 'alpha'
  }

  if (!is.null(states) && 'delta[1]' %in% xlate$Parameter) {
    message("Translating states")
    for(i in seq(nrow(states))) {
      xlate$Label[xlate$Parameter == paste0('delta[', states$state.index[i], ']')] <-
        paste0('delta[', as.character(states$state[i]),']')
    }
  }

  for(i in seq_along(state_vars)) {
    xlate$Label[xlate$Parameter == paste0('gamma[', i + index_offset, ']')] <-
      paste0('gamma[',str_replace(state_vars[i], '^state\\.',''), ']')
  }
  for(j in seq_along(msa_vars)) {
    xlate$Label[xlate$Parameter == paste0('beta[', j, ']')] <-
      paste0('beta[',msa_vars[j], ']')
  }
  g <- suppressWarnings(ggs(sfit, family = family, par_labels = xlate))
  g_attr <- attributes(g)
  par_list <- g %>% group_by(Parameter) %>% dplyr::summarize(value = mean(value)) %>%
    ungroup() %>%
    mutate( value = if(abs_rank) abs(value) else value,
            Parameter = as.character(Parameter),
            par = str_replace_all(Parameter, '\\[.*$','')) %>%
    arrange(par, value) %>%
    dplyr::select(Parameter) %>% unlist() %>% unname()
  g <- g %>% mutate(Parameter = ordered(Parameter, levels = par_list))
  attributes(g) <- g_attr
  invisible(g)
}


make_cat_plot <- function(g, family = 'beta|gamma', target = 'vwci',
                          thick_ci = c(0.17, 0.83), thin_ci = c(0.025, 0.975),
                          thick_size = 1.5, thin_size = 0.5, point_size = 3,
                          line = 0, linesize = 0.5, linetype = 'solid',
                          family_name = "Family", title = NA,
                          scale_beta_by_actions = FALSE,
                          color = FALSE
) {
  if (target == 'vwci') {
    target_name <- 'VWCI'
    k_actions = n_actions
  } else if (target == 'reqtotal') {
    target_name <- 'Requirements'
    k_actions = n_req
  } else if (target == 'rebtotal') {
    target_name <- 'Rebates'
    k_actions = n_reb
  } else {
    target_name <- 'Unknown'
    warning("Unknown target variable ", target)
  }
  families <- data_frame(
    name = c('Intercept', 'MSA-level', 'State-level'),
    pattern = c('^alpha.*$', '^beta.*$', '^gamma.*$')
  )
  if (scale_beta_by_actions)  {
    g <- g %>% mutate(value = value * k_actions / 4)
  } else {
    g <- g %>% mutate(value = value / 4)
  }
  g <- g %>% mutate(
    family = str_replace_all(Parameter, setNames(families$name, families$pattern)) %>%
      ordered(levels = c('State-level', 'MSA-level', 'Intercept')),
    Parameter = ordered(Parameter, levels = levels(Parameter),
                        labels = str_replace_all(levels(Parameter), fixed('['), ' * minute['))
  )
  y_list <- g %>% dplyr::select(Parameter) %>% distinct() %>%
    arrange(Parameter) %>% as_data_frame() %>%
    dplyr::filter(str_detect(Parameter, family)) %>%
    mutate(value = seq_along(Parameter), Parameter = as.character(Parameter)) %>%
    as.data.frame()
  if (!is.null(title)) {
    if (is.na(title)) {
      title <- paste("Regression coefficients for", target_name)
    } else if (title == FALSE) {
      title <- NULL
    }
  }
  p <- ggs_caterpillar(g, X = y_list, family = family, greek = T,
                       thick_ci = thick_ci, thick_size = thick_size,
                       thin_ci = thin_ci, thin_size = thin_size,
                       point_size = point_size,
                       line = line, linesize = linesize, linetype = linetype) +
    aes(color = family) +
    scale_color_brewer(palette = 'Dark2', name = family_name, guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks = y_list$value,
                       labels = purrr::map(y_list$Parameter, ~parse(text = .x)) %>% unlist()) +
    labs(x = "Scaled coefficient", y = NULL, title = title) +
    theme(plot.title = element_text(hjust = 1),
          panel.grid.minor.x = element_blank())
  p
}

make_scatter_plot <- function(residuals, color = F, pt.alpha = 0.5) {
  res <- residuals$residuals %>% mutate(state_2 = ifelse(state %in% c('CA','FL','TX'),
                                                         as.character(state), 'Other') %>%
                                          ordered(levels = c('CA','FL','TX','Other'))) %>%
    rename_(.dots = setNames(list(residuals$target), list('actual')))
  if (residuals$target == 'vwci') {
    target <- 'VWCI'
    k_actions <- n_actions
  } else if (residuals$target == 'reqtotal') {
    target <- 'Requirements'
    k_actions <- n_req
  } else if (residuals$target == 'rebtotal') {
    target <- 'Rebates'
    k_actions <- n_reb
  } else {
    target <- 'Unknown'
    k_actions <- NA
    warning("Unknown target variable ", residuals$target)
  }
  if (color) {
    p <- ggplot(res, aes(x = predicted, y = actual, color = state_2)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_point(alpha = pt.alpha) +
      coord_fixed(xlim = c(0,k_actions), ylim = c(0,k_actions), expand = 0) +
      scale_color_manual(values = set_names(c(brewer.pal(3, "Dark2"), "gray50"), c('CA', 'FL', 'TX', 'Other')),
                         name = "State") +
      labs(x = paste("Predicted",target), y = paste("Actual",target),
           title = paste("Predicted vs. Actual", target))
  } else {
    p <- ggplot(res, aes(x = predicted, y = actual)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_point(color = "dark blue", alpha = pt.alpha) +
      coord_fixed(xlim = c(0,k_actions), ylim = c(0,k_actions), expand = 0) +
      labs(x = paste("Predicted",target), y = paste("Actual",target),
           title = paste("Predicted vs. Actual", target))

  }
  p
}

calc_post_pred_residuals <- function(sfit, df, target = 'vwci') {
  y <- df$msa_data %>% mutate(state = factor(state), state.index = as.integer(state)) %>%
    dplyr::select_(~state, .dots=list(y = target)) %>% mutate(index = seq_along(y))
  y_pred <- rstan::extract(sfit, pars='y_pred', permuted=TRUE, inc_warmup=FALSE)[[1]] %>%
    as_data_frame() %>% gather(key = index, value = y_pred) %>%
    mutate(index = as.numeric(str_replace_all(index, '^V',''))) %>%
    left_join(y, by = 'index') %>%
    group_by(index) %>% mutate(residual = y - y_pred, scale = sd(residual)) %>% ungroup()
  invisible(y_pred)
}

make_post_pred_scatter_plot <- function(sfit, df, target = c('vwci', 'rebtotal', 'reqtotal'),
                                        type = c('box','range'), range.scale = 2) {
  target = match.arg(target)
  type = match.arg(type)
  y_pred <- calc_post_pred_residuals(sfit, df, target)  %>%
    arrange(y) %>%
    mutate(index = ordered(index, levels = unique(index)) %>% as.numeric()) %>%
    mutate(state = recode_factor(state, CA = 'CA', FL = 'FL',
                                 TX = 'TX', .default = "Other"))
  if (type == 'box') {
    g = expression(jgplot2::geom_boxplot(outlier.colour = 'dark blue', outlier.size = 0.1, outlier.alpha = 0.1))
  } else {
    y_pred <- y_pred %>% group_by(index) %>%
      dplyr::summarize(state = first(state), mean_residual = mean(residual),
                       sd_residual = sd(residual),
                       scale = mean(scale)) %>% rename(residual = mean_residual) %>%
      ungroup()
    g <- substitute(geom_pointrange(aes_(y = ~residual / scale,
                                         ymin = ~(residual - sd_residual * range.scale) / scale,
                                         ymax = ~(residual + sd_residual * range.scale) /scale)), list(range.scale = range.scale))
  }
  ggplot(y_pred, aes(x = index, y = residual / scale, group = index,
                     color = state)) +
    # jgplot2::geom_boxplot(outlier.colour = 'dark blue', outlier.size = 0.1, outlier.alpha = 0.1) +
    eval(g) +
    geom_hline(yintercept = 0) +
    scale_color_brewer(type = 'qual', palette = 'Dark2') +
    theme_bw()
}

make_model_name <- function(var_index, dep_var, multilevel, beta, random_alpha = FALSE) {
  dep_var <- str_to_lower(dep_var)
  str_c('_', var_index, ifelse(multilevel, '_ml', '_sl'), ifelse(beta, '_beta', ''),
        ifelse(multilevel & random_alpha, '_alpha', ''), ifelse(dep_var %in% c('', 'vwci'), '', str_c('_', dep_var)))
}

process_models <- function(std_data, vars, data.dir = data_dir,
                           abs_rank = TRUE,
                           vars_2 = TRUE,
                           beta = TRUE,
                           multilevel = TRUE,
                           random_alpha = TRUE,
                           remove_desal = FALSE,
                           mu_phi_vwci = 50, sigma_phi_vwci = 20,
                           mu_phi_rr = 15, sigma_phi_rr = 15,
                           sigma_sigma_delta = 0.5,
                           seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  seeds <- sample.int(.Machine$integer.max, 100, replace = TRUE)

  seed_list <- list()
  i <- 1
  for (v2 in c(TRUE, FALSE)) {
    v = ifelse(v2, 2, 1)
    for (ml in c(TRUE, FALSE)) {
      for (ra in unique(ml & c(TRUE, FALSE))) {
        for (b in c(TRUE, FALSE)) {
          for (dv in c('VWCI', 'REQ', 'REB')) {
            seed_list[make_model_name(v, dv, ml, b, ra)] = seeds[i]
            i <- i + 1
          }
        }
      }
    }
  }

  model_fits <- list()
  model_ggs <- list()
  model_res <- list()

  for (v2 in vars_2) {
    vi = ifelse(v2, 2, 1)            # independent variables index (1 or 2)
    if (v2) vv <- vars$vars_2 else vv <- vars$vars_1
    # message('vv = ', map(vv, ~str_c("name = ", names(.x), "(", str_c(.x, collapse = ", "), ")", collapse = "; ")))
    for (ml in multilevel) {
      for (b in beta) {
        for (ra in unique(ml & random_alpha)) {
          for (dv in c('vwci', 'req', 'reb')) {
            m_name <- make_model_name(vi, dv, ml, b, ra)
            if (dv == "vwci") {
              mu_phi = mu_phi_vwci
              sigma_phi = sigma_phi_vwci
            } else {
              mu_phi = mu_phi_rr
              sigma_phi = sigma_phi_rr
            }
            message("Fitting model ", m_name, " with vars = ", paste0('vars_', vi), ', target = ', dv,
                    ', beta = ', b, ', multilevel = ', ml, ', random alpha = ', ra)
            # message('vv = ', map(vv, ~str_c("name = ", names(.x), "(", str_c(.x, collapse = ", "), ")", collapse = "; ")))
            sfit <- fit_model(df = std_data, vars = vv, target = dv,
                              beta = b,
                              multilevel = ml, random_alpha = ra,
                              remove_desal = remove_desal,
                              mu_phi = mu_phi, sigma_phi = sigma_phi,
                              sigma_sigma_delta = sigma_sigma_delta,
                              seed = seeds[m_name])
            message('saving model sfit', m_name, ' classs = ', class(sfit))
            model_fits <- c(model_fits, setNames(list(sfit), str_c('sfit', m_name)))
            if (ml) {
              g <- make_ggs_var_intercept(sfit, vv, abs_rank = abs_rank,
                                          random_alpha = ra)
              message('saving ggs ggs', m_name, ' class = ', class(g))
              model_ggs <- c(model_ggs, setNames(list(g), str_c('ggs', m_name)))
              res <- calc_residuals(std_data, sfit, vv, target = dv, random_alpha = ra,
                                    remove_desal = remove_desal)
              message('saving residuals res', m_name, ' classs = ', class(res))
              model_res <- c(model_res, setNames(list(res), str_c('res', m_name)))
            }
            saveRDS(list(model_fits = model_fits, model_ggs = model_ggs, model_res = model_res), file = file.path(data.dir, 'model_fit_checkpoint.rds'))
          }

        }
      }
    }
  }

  result <- list(fits = model_fits, ggs = model_ggs, res = model_res)
  write_rds(result, path = file.path(data.dir, 'model_fits.Rds'), compress = 'xz')
  invisible(result)
}

summarize_fit <- function(sfit, vars, std_data, random_alpha, beta, multilevel) {
  pars <- c('beta')
  if (multilevel) pars <- c(pars, 'gamma')
  if (random_alpha) pars <- c(pars, 'alpha_0', 'delta')
  if (beta) pars <- c(pars, 'phi')

  states <- std_data$msa_data %>% index_states() %>% dplyr::select(state, state.index) %>%
    distinct() %>% arrange(state)

  beta_replacements <- setNames(
    str_c('$\\\\beta_{\\\\text{',
          str_replace_all(vars$msa_vars, c('^state\\.'='', '\\.'=' ',
                                           'pvi' = 'PVI', 'rpi' = 'RPI', 'rpp' = 'RPP')),
          '}}$'),
    str_c('^beta\\[', seq_along(vars$msa_vars), '\\]$')
  )

  gamma_replacements <- setNames(
    str_c('$\\\\gamma_{\\\\text{',
          str_replace_all(vars$state_vars, c('^state\\.'='', '\\.'=' ',
                                             'pvi' = 'PVI', 'rpi' = 'RPI', 'rpp' = 'RPP')),
          '}}$'),
    str_c('^gamma\\[', seq_along(vars$state_vars), '\\]$')
  )


  delta_replacements <- setNames(
    str_c('$\\\\delta_{\\\\text{',
          states$state,
          '}}$'),
    str_c('^delta\\[', states$state.index, '\\]$')

  )

  replacements <- c('^alpha_0$' = '$\\\\alpha_0$', '^phi$' = '$\\\\phi$',
                    gamma_replacements, beta_replacements, delta_replacements)

  s <- summary(sfit, pars = pars)$summary %>%
    as.data.frame() %>% rownames_to_column("coefficient") %>% as_tibble() %>%
    mutate(coefficient = str_replace_all(coefficient, replacements) %>%
             ordered(levels = str_replace_all(replacements, fixed('\\\\'), '\\'))) %>%
    arrange(coefficient) %>%
    select(-n_eff) %>%
    rename("std.~err." = se_mean, 'std.~dev.' = sd, '$\\hat R$\\rule{0pt}{2.5ex}' = Rhat) %>%
    set_names(names(.) %>% str_replace_all(fixed('%'), '\\%'))
}
