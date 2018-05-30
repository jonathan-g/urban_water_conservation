options(tidyverse.quiet = TRUE)
library(pacman)
p_load(tidyverse)
p_load(readxl)

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
# Standardize MSA data to mean and sd of completely pooled  MSA's
#
standardize_msa_data_pooled <- function(msa_data, scale_factor = 2.0) {
  indices <- c('msa.fips','city','state','city.state', 'msa.name',
               'lon','lat',
               'vwci', 'reqtotal', 'rebtotal')
  df  <- msa_data %>% dplyr::select(-one_of(indices))
  means <- df %>% summarize_all(funs(mean(.)))
  sds <- df %>% summarize_all(funs(sd(.)))
  msa_data <- msa_data %>%
    mutate_at(vars(-one_of(indices)),funs((. - mean(.)) / (scale_factor * sd(.))))

  invisible(list(means = means, sds = sds, msa_data = msa_data))
}

#
# Standardize MSA data to mean and sd of completely pooled  MSA's
# Standardize state data to mean and sd of states.
#
standardize_data_pooled <- function(msa_data, state_data, scale_factor = 2.0) {
  indices <- c('msa.fips','city','state', 'city.state', 'msa.name','lon','lat',
               'vwci', 'reqtotal', 'rebtotal')

  df  <- msa_data %>% dplyr::select(-one_of(indices))
  means <- df %>% summarize_all(funs(mean(.)))
  sds <- df %>% summarize_all(funs(sd(.)))

  msa_data <- msa_data %>%
    mutate_at(vars(-one_of(indices)),funs((. - mean(.)) / (scale_factor * sd(.))))

  state_indices <- c('state.fips','state','state.name')

  state_df  <- state_data %>% dplyr::select(-one_of(state_indices))
  state_means <- state_df %>% summarize_all(funs(mean(.)))
  state_sds <- state_df %>% summarize_all(funs(sd(.)))

  state_data <- state_data %>%
    mutate_at(vars(-one_of(state_indices)), funs((. - mean(.)) / (scale_factor * sd(.))))

  varlist <- names(state_data) %>% discard(~.x %in% c('state', 'state.name', 'state.fips'))
  dots <- varlist %>% {setNames(as.list(.), str_c('state', ., sep='.'))}
  state_data <- state_data %>% rename_(.dots = dots)

  msa_data$pvi.aridity <- msa_data$pvi * msa_data$aridity
  state_data$state.pvi.aridity <- state_data$state.pvi * state_data$state.aridity

  pvi_state_aridity <- state_data %>% dplyr::select(state, state.aridity) %>%
    right_join(msa_data, by = "state") %>%
    transmute(msa.fips, pvi.state.aridity = pvi * state.aridity)

  msa_data <- msa_data %>% left_join(pvi_state_aridity, by = "msa.fips")

  invisible(list(means = means, sds = sds, state_means = state_means, state_sds = state_sds,
                 msa_data = msa_data, state_data = state_data))
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
  means <- df %>% summarize_all(funs(mean(.)))
  sds <- df %>% summarize_all(funs(sd(.)))

  # scale state_data to zero mean, sd = 1 / scale_factor
  state_data <- state_data %>%
    mutate_at(vars(-one_of(state_indices)),funs((. - mean(.)) / (scale_factor * sd(.))))

  msa_cols <- c(msa_indices, names(df)) %>% intersect(names(msa_data))
  # data_cols is the data columns that exist in both state_data and msa_data
  data_cols <- setdiff(msa_cols, msa_indices)

  msa_df <- msa_data %>% dplyr::select(-one_of(msa_cols))
  msa_means <- msa_df %>% summarize_all(funs(mean(.))) %>% bind_cols(means, .)
  msa_sds <- msa_df %>% summarize_all(funs(sd(.))) %>% bind_cols(sds, .)

  # scale to zero mean, sd = 1 / scale_factor  for columns that don't exist in state_data
  msa_data <- msa_data %>%
    mutate_at(vars(-one_of(msa_cols)),funs((. - mean(.)) / (scale_factor * sd(.))))

  # for columns that do exist in state_data, scale these the same way the state
  # variables are scaled (subtract state mean, divide by scale_factor * state sd)
  for (n in intersect(names(means), names(msa_data))) {
    msa_data[,n] <- (msa_data[,n] - means[[n]]) / (scale_factor * sds[[n]])
  }

  varlist <- names(state_data) %>% discard(~.x %in% c('state', 'state.name', 'state.fips'))
  dots <- varlist %>% {setNames(as.list(.), str_c('state', ., sep='.'))}
  state_data <- state_data %>% rename_(.dots = dots)

  varlist <- c('pvi', 'rpp', 'rpi', 'affordability', 'gini',
               'precip', 'temp', 'aridity',
               'precip_70', 'temp_70', 'aridity_70',
               'precip_85', 'temp_85', 'aridity_85',
               'precip_95', 'temp_95', 'aridity_95',
               'precip_05', 'temp_05', 'aridity_05',
               'surface.water')
  f <- purrr::map(varlist, function(x) {
    substitute(.x - .y, list(.x = as.name(x),
                             .y = as.name(str_c('state', x, sep = '.'))))
  })

  msa_data <- msa_data %>% mutate(state = as.character(state)) %>%
    left_join(state_data, by = 'state') %>%
    mutate_(.dots = setNames(f, varlist)) %>%
    dplyr::select(-starts_with('state.'))

  msa_data$pvi.aridity <- msa_data$pvi * msa_data$aridity
  state_data$state.pvi.aridity <- state_data$state.pvi * state_data$state.aridity

  pvi_state_aridity <- state_data %>% dplyr::select(state, state.aridity) %>%
    right_join(msa_data, by = "state") %>%
    transmute(msa.fips, pvi.state.aridity = pvi * state.aridity)

  msa_data <- msa_data %>% left_join(pvi_state_aridity, by = "msa.fips")

  invisible(list(means = msa_means, sds = msa_sds, state_means = means, state_sds = sds,
                 msa_data = msa_data, state_data = state_data))
}




process_data <- function(data.dir = data_dir, scale_factor = 2.0, pooled = FALSE, save_data = TRUE) {
  msa_data <- load_data(data.dir)
  # if (!dir.exists(si_data_dir)) dir.create(si_data_dir, recursive = TRUE)
  state_data <- read_rds(file.path(data.dir, 'state_predictors.Rds'))

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
