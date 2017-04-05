suppressMessages(library(hellno, quietly = TRUE))
suppressMessages(library(dplyr, quietly = TRUE))
library(tidyr)
library(stringr)
library(readxl)
library(readr)

climate_data_dir <- 'data'
# bea_year <- '2014'

process_city_climate <- function(data.dir = climate_data_dir, bea_year = 2014) {
  bea_year <- as.character(bea_year)

  city_aridity <- read_rds(file.path(data.dir, 'cities_aridity.Rds'))

  city_rpp <- read_excel(file.path(data.dir, 'regional_price_parity.xls'), 1, skip = 5) %>%
    dplyr::select_(.dots = setNames(list(~GeoFips, as.name(bea_year)), list('msa.fips','rpp'))) %>%
    mutate(msa.fips = as.integer(msa.fips)) %>%
    dplyr::filter(! is.na(msa.fips))
  city_rpi <- read_excel(file.path(data.dir, 'real_personal_income.xls'), 1, skip = 5) %>%
    dplyr::select_(.dots = setNames(list(~GeoFips, as.name(bea_year)), list('msa.fips','rpi'))) %>%
    mutate(msa.fips = as.integer(msa.fips)) %>%
    dplyr::filter(! is.na(msa.fips))
  city_data <- read_csv(file.path(data.dir, 'VWCI_explanatory_variables.csv')) %>%
    dplyr::select(msa.fips = msa.fips, pvi, pop, pop.growth, surface.water) %>%
    distinct()
  city_data <- city_data %>% left_join(city_rpp, by = 'msa.fips') %>%
    left_join(city_rpi, by = 'msa.fips') %>%
    dplyr::filter(str_detect(msa.fips, '^[0-9]') & as.numeric(msa.fips) > 0) %>%
    mutate(affordability = rpi / rpp, log.pop = log10(pop))

  city_names <- city_aridity %>% dplyr::select(msa.fips, city.state, city, state, msa.name, lon, lat) %>%
    distinct()

  climate_data <- city_aridity %>%
    dplyr::filter(year >= 1970 & year <= 2014) %>%
    group_by(msa.fips, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp)) %>%
    mutate(aridity = precip / (33 + temp)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_each(funs(mean(., na.rm = T)), precip, temp, aridity) %>%
    ungroup() %>%
    left_join(city_names, ., by = 'msa.fips')

  city_data <- left_join(climate_data, city_data, by = 'msa.fips')
  write_rds(city_data, path = file.path(data.dir, 'msa_predictors.Rds'), compress = 'xz')
  invisible(city_data)
}
