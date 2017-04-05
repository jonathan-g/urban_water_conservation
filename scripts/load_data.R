suppressMessages(library(hellno, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
library(readxl)
library(stringr)

data_dir = 'data'
script_dir = 'scripts'

load_data <- function(data.dir = data_dir) {
  vwci <- read_csv(file.path(data.dir, 'VWCI_city_coordinates.csv'))

  aux_vars <- read_rds(file.path(data.dir, 'msa_predictors.Rds')) %>%
    mutate(city = as.character(city) %>% str_trim()) %>%
    dplyr::select(-city.state, -city, -state, -lat, -lon, -msa.name)

  df <- vwci %>% left_join(aux_vars, by = 'msa.fips')
  invisible(df)
}

filter_continental <- function(msa_data, state_data) {
  msa_data <- msa_data %>% dplyr::filter(! state %in% c('HI','AK'))
  state_data <- state_data %>% dplyr::filter(! state %in% c('HI','AK'))
  invisible(list(msa_data = msa_data, state_data = state_data))
}

#
# Standardize MSA data and state data both to mean and sd of completely pooled  MSA's
#
standardize_msa_data_pooled <- function(msa_data, scale_factor = 2.0) {
  indices <- c('msa.fips','city','state','city.state', 'msa.name',
               'lon','lat',
               'vwci', 'reqtotal', 'rebtotal')
  df  <- msa_data %>% dplyr::select(-one_of(indices))
  means <- df %>% summarize_each(funs(mean(.)))
  sds <- df %>% summarize_each(funs(sd(.)))
  msa_data <- msa_data %>%
    mutate_each(funs((. - mean(.)) / (scale_factor * sd(.))), -one_of(indices))

  invisible(list(means = means, sds = sds, msa_data = msa_data))
}

#
# Standardize MSA data and state data both to mean and sd of completely pooled  MSA's
#
standardize_data_pooled <- function(msa_data, state_data, scale_factor = 2.0) {
  indices <- c('msa.fips','city','state', 'city.state', 'msa.name','lon','lat',
               'vwci', 'reqtotal', 'rebtotal')
  df  <- msa_data %>% dplyr::select(-one_of(indices))
  means <- df %>% summarize_each(funs(mean(.)))
  sds <- df %>% summarize_each(funs(sd(.)))
  msa_data <- msa_data %>%
    mutate_each(funs((. - mean(.)) / (scale_factor * sd(.))), -one_of(indices))

  state_indices <- c('state.fips','state','state.name')
  cols <- c(state_indices, names(df)) %>% intersect(names(state_data))
  state_data <- state_data %>% dplyr::select(one_of(cols))
  data_cols <- setdiff(names(state_data), state_indices)
  for(n in data_cols) {
    state_data[,n] <- (state_data[,n] - means[[n]]) / (scale_factor * sds[[n]])
  }

  varlist <- names(state_data) %>% discard(~.x %in% c('state', 'state.name'))
  dots <- varlist %>% {setNames(as.list(.), str_c('state', ., sep='.'))}
  state_data <- state_data %>% rename_(.dots = dots)

  varlist <- c('pvi', 'rpp', 'rpi', 'affordability', 'precip', 'temp', 'aridity',
               'surface.water')
  f <- purrr::map(varlist, function(x) {
    substitute(.x - .y, list(.x = as.name(x),
                             .y = as.name(str_c('state', x, sep = '.'))))
  })

  msa_data <- msa_data %>% mutate(state = as.character(state)) %>%
    left_join(state_data, by = 'state') %>%
    mutate_(.dots = setNames(f, varlist)) %>%
    dplyr::select(-starts_with('state.'))

  invisible(list(means = means, sds = sds, msa_data = msa_data, state_data = state_data))
}

#
# Standardize State data and then scale MSA's to represent difference from state averages.
#
standardize_data_by_state <- function(msa_data, state_data, scale_factor = 2.0) {
  msa_indices <- c('msa.fips', 'city', 'state', 'city.state',
                   'msa.fips', 'msa.name',
                   'lon','lat',
               'vwci', 'reqtotal', 'rebtotal')

  state_indices <- c('state.fips','state','state.name')

  df  <- state_data %>% dplyr::select(-one_of(state_indices))
  means <- df %>% summarize_each(funs(mean(.)))
  sds <- df %>% summarize_each(funs(sd(.)))

  # scale state_data to zero mean, sd = 1 / scale_factor
  state_data <- state_data %>%
    mutate_each(funs((. - mean(.)) / (scale_factor * sd(.))), -one_of(state_indices))

  msa_cols <- c(msa_indices, names(df)) %>% intersect(names(msa_data))
  # data_cols is the data columns that exist in both state_data and msa_data
  data_cols <- setdiff(msa_cols, msa_indices)

  msa_df <- msa_data %>% dplyr::select(-one_of(msa_cols))
  msa_means <- msa_df %>% summarize_each(funs(mean(.))) %>% bind_cols(means, .)
  msa_sds <- msa_df %>% summarize_each(funs(sd(.))) %>% bind_cols(sds, .)

  # scale to zero mean, sd = 1 / scale_factor  fpr columns that don't exist in state_data
  msa_data <- msa_data %>%
    mutate_each(funs((. - mean(.)) / (scale_factor * sd(.))), -one_of(msa_cols))

  # for columns that do exist in state_data, scale these the same way the state
  # variables are scaled (subtract state mean, divide by scale_factor * state sd)
  for (n in intersect(names(means), names(msa_data))) {
    msa_data[,n] <- (msa_data[,n] - means[[n]]) / (scale_factor * sds[[n]])
  }

  varlist <- names(state_data) %>% discard(~.x %in% c('state', 'state.name'))
  dots <- varlist %>% {setNames(as.list(.), str_c('state', ., sep='.'))}
  state_data <- state_data %>% rename_(.dots = dots)

  varlist <- c('pvi', 'rpp', 'rpi', 'affordability', 'precip', 'temp', 'aridity',
               'surface.water')
  f <- purrr::map(varlist, function(x) {
    substitute(.x - .y, list(.x = as.name(x),
                             .y = as.name(str_c('state', x, sep = '.'))))
  })

  msa_data <- msa_data %>% mutate(state = as.character(state)) %>%
    left_join(state_data, by = 'state') %>%
    mutate_(.dots = setNames(f, varlist)) %>%
    dplyr::select(-starts_with('state.'))

  invisible(list(means = msa_means, sds = msa_sds, msa_data = msa_data, state_data = state_data))
}




process_data <- function(data.dir = data_dir, scale_factor = 2.0, pooled = FALSE,
                         save_data = TRUE, si_data_dir = 'si_scripts/data/') {
  msa_data <- load_data(data.dir)
  if (!dir.exists(si_data_dir)) dir.create(si_data_dir, recursive = )
  state_data <- read_rds(file.path(data.dir, 'state_predictors.Rds'))

  if (save_data) {
    msa_data %>% dplyr::select(msa.fips, msa.name, city.state, city, state, lat, lon,
                               vwci, reqtotal, rebtotal,
                               precip, temp, aridity, pvi,
                               pop, log.pop, pop.growth,
                               surface.water,
                               rpp, rpi) %>%
      write_csv(path = file.path(si_data_dir, 'msa_data.csv'))

    state_data %>% dplyr::select(state.fips, state, state.name,
                                 precip, temp, aridity, pvi,
                                 surface.water,
                                 rpp, rpi) %>%
      write_csv(path = file.path(si_data_dir, 'state_covariates.csv'))
  }

  filtered_data <- filter_continental(msa_data, state_data)
  if (pooled) {
    std_data <- standardize_data_pooled(filtered_data$msa_data,
                                        filtered_data$state_data,
                                        scale_factor = scale_factor)
  } else {
    std_data <- standardize_data_by_state(filtered_data$msa_data,
                                          filtered_data$state_data,
                                          scale_factor = scale_factor)
  }
  if (save_data) {
    if(pooled) {
      base_name <-  '_pooled_data'
    } else {
      base_name <-  '_data'
    }
    write_rds(filtered_data, path = file.path(data.dir, paste0('filtered', base_name, '.Rds')), compress = 'xz')
    write_rds(std_data, path = file.path(data.dir, paste0('standardized', base_name, '.Rds')), compress = 'xz')
  }
  invisible(list(msa_data = msa_data, state_data = state_data,
                 filtered_data = filtered_data, standardized_data = std_data))
}
