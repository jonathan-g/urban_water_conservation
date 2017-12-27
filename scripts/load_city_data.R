library(pacman)
options(tidyverse.quiet = TRUE)
p_load(tidyverse)
p_load(readxl)
p_load(janitor)


climate_data_dir <- 'data'
# bea_year <- '2014'

process_city_climate <- function(data.dir = climate_data_dir, bea_year = 2014) {
  bea_year <- as.character(bea_year)

  city_aridity <- read_rds(file.path(data.dir, 'cities_aridity.Rds'))

  city_rpp <- read_excel(file.path(data.dir, 'regional_price_parity.xls'), 1, skip = 5,
                         col_types = c("text", "text", rep("numeric", 7))) %>%
    dplyr::select(msa.fips = GeoFips, rpp = matches(fixed(as.character(bea_year)))) %>%
    dplyr::filter(str_detect(msa.fips, "^[0-9]+$")) %>%
    mutate(msa.fips = as.integer(msa.fips))

  city_rpi <- read_excel(file.path(data.dir, 'real_personal_income.xls'), 1, skip = 5) %>%
    dplyr::select(msa.fips = GeoFips, rpi = matches(as.character(bea_year))) %>%
    dplyr::filter(str_detect(msa.fips, "^[0-9]+$")) %>%
    mutate(msa.fips = as.integer(msa.fips))

  gini_header <- suppressMessages(
    read_csv(file.path('data','gini', 'ACS_14_1YR_B19083_with_ann.csv'), n_max = 1)
  ) %>% names()

  gini <- suppressMessages(
    read_csv(file.path('data','gini', 'ACS_14_1YR_B19083_with_ann.csv'), skip = 2, col_names = gini_header,
             col_types = cols(col_character(), col_integer(), col_character(), col_double(), col_double()))
  )%>%
    clean_names() %>%
    rename(geoid = geo_id, fips = geo_id2, place = geo_display_label, gini = hd01_vd01, gini.se = hd02_vd01) %>%
    mutate(category = ifelse(str_detect(geoid, "^[0-9]+US[0-9]+$"), "State", "MSA") %>% factor())

  city_gini <- gini %>% dplyr::filter(category == "MSA")

  city_data <- read_csv(file.path(data.dir, 'VWCI_explanatory_variables.csv')) %>%
    dplyr::select(msa.fips, pvi, pop, pop.growth, pop.dens, pop.dens.growth, area, surface.water) %>%
    distinct()
  city_data <- city_data %>% left_join(city_rpp, by = 'msa.fips') %>%
    left_join(city_rpi, by = 'msa.fips') %>%
    left_join(city_gini %>% dplyr::select(msa.fips = fips, gini), by = "msa.fips") %>%
    dplyr::filter(str_detect(msa.fips, '^[0-9]') & as.numeric(msa.fips) > 0) %>%
    mutate(affordability = rpi / rpp, log.pop = log10(pop), log.pop.dens = log10(pop.dens))

  city_names <- city_aridity %>% dplyr::select(msa.fips, city.state, city, state, msa.name, lon, lat) %>%
    distinct()

  climate_data <- city_aridity %>%
    dplyr::filter(year >= 1970 & year <= 2014) %>%
    group_by(msa.fips, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp)) %>%
    mutate(aridity = precip / (33 + temp)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_at(vars(precip, temp, aridity), funs(mean(., na.rm = T))) %>%
    rename(precip_70 = precip, temp_70 = temp, aridity_70 = aridity) %>%
    ungroup() %>%
    left_join(city_names, ., by = 'msa.fips')

  climate_data <- city_aridity %>%
    dplyr::filter(year >= 1985 & year <= 2014) %>%
    group_by(msa.fips, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp)) %>%
    mutate(aridity = precip / (33 + temp)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_at(vars(precip, temp, aridity), funs(mean(., na.rm = T))) %>%
    rename(precip_85 = precip, temp_85 = temp, aridity_85 = aridity) %>%
    ungroup() %>%
    left_join(climate_data, ., by = 'msa.fips')

  climate_data <- city_aridity %>%
    dplyr::filter(year >= 1995 & year <= 2014) %>%
    group_by(msa.fips, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp)) %>%
    mutate(aridity = precip / (33 + temp)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_at(vars(precip, temp, aridity), funs(mean(., na.rm = T))) %>%
    rename(precip_95 = precip, temp_95 = temp, aridity_95 = aridity) %>%
    ungroup() %>%
    left_join(climate_data, ., by = 'msa.fips')


  climate_data <- city_aridity %>%
    dplyr::filter(year >= 2005 & year <= 2014) %>%
    group_by(msa.fips, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp)) %>%
    mutate(aridity = precip / (33 + temp)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_at(vars(precip, temp, aridity), funs(mean(., na.rm = T))) %>%
    rename(precip_05 = precip, temp_05 = temp, aridity_05 = aridity) %>%
    ungroup() %>%
    left_join(climate_data, ., by = 'msa.fips') %>%
    mutate(temp = temp_85, precip = precip_85, aridity = aridity_85)

  city_data <- left_join(climate_data, city_data, by = 'msa.fips')
  write_rds(city_data, path = file.path(data.dir, 'msa_predictors.Rds'), compress = 'xz')
  invisible(city_data)
}
