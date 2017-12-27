library(pacman)
options(tidyverse.quiet = TRUE)
p_load(tidyverse)
p_load(readxl)
p_load(janitor)
# p_load(rnoaa)

process_state_data <- function(climate_data_dir = 'data', bea_year = '2014') {

  # noaa_api_key <- 'JfhGIOixTAkLKSTysDSVqgthhEznxNAu'

  widths <- fwf_widths(c(3, 1, 2, 4, rep(7,12)),
                       col_names = c('code', 'div', 'var', 'year', str_to_lower(month.abb)))

  div_widths <- fwf_widths(c(2, 2, 2, 4, rep(7,12)),
                           col_names = c('code', 'div', 'var', 'year', str_to_lower(month.abb)))

  climate_lookup <- data_frame(state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                                         "DE", "DC", "FL", "GA", "HI", "ID", "IL",
                                         "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                         "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                         "NV", "NH", "NJ", "NM", "NY", "NC", "ND",
                                         "OH", "OK", "OR", "PA", "RI", "SC", "SD",
                                         "TN", "TX", "UT", "VT", "VA", "WA", "WV",
                                         "WI", "WY"),
                               code = c('001', '050', '002', '003', '004', '005', '006',
                                        '007', '051', '008', '009', '999', '010', '011',
                                        '012', '013', '014', '015', '016', '017', '018',
                                        '019', '020', '021', '022', '023', '024', '025',
                                        '026', '027', '028', '029', '030', '031', '032',
                                        '033', '034', '035', '036', '037', '038', '039',
                                        '040', '041', '042', '043', '044', '045', '046',
                                        '047', '048')
  )

  temp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpcst-v1.0.0-20161004"
  prcp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpnst-v1.0.0-20161004"
  pdsi_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsist-v1.0.0-20161004"

  div_temp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpcdv-v1.0.0-20161004"
  div_prcp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpndv-v1.0.0-20161004"
  div_pdsi_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsidv-v1.0.0-20161004"

  temp_file <- "climdiv-tmpcst-v1.0.0-20161004"
  prcp_file <- "climdiv-pcpnst-v1.0.0-20161004"
  pdsi_file <- "climdiv-pdsist-v1.0.0-20161004"

  div_temp_file <- "climdiv-tmpcdv-v1.0.0-20161004"
  div_prcp_file <- "climdiv-pcpndv-v1.0.0-20161004"
  div_pdsi_file <- "climdiv-pdsidv-v1.0.0-20161004"


  download_climate_data <- function(dest_dir = climate_data_dir) {
    for (x in list(c(url = temp_url, file = temp_file),
                   c(url = prcp_url, file = prcp_file),
                   c(url = pdsi_url, file = pdsi_file),
                   c(url = div_temp_url, file = div_temp_file),
                   c(url = div_prcp_url, file = div_prcp_file),
                   c(url = div_pdsi_url, file = div_pdsi_file))) {
      dest = file.path(dest_dir, x['file'])
      if (! file.exists(dest)) download.file(x['url'], dest)
    }
  }

  download_climate_data()

  temp_data <- read_fwf(file.path(climate_data_dir, temp_file), widths, na = c("-99.90")) %>%
    gather(key = month, value = value, -code, -div, -var, -year) %>%
    mutate(month = ordered(month, levels = str_to_lower(month.abb))) %>%
    inner_join(climate_lookup, by = 'code') %>%
    arrange(state, year, month)

  div_temp_data <- read_fwf(file.path(climate_data_dir, div_temp_file), div_widths, na = c("-99.90")) %>%
    dplyr::filter(code == '18' & div == '04') %>%
    mutate(code = '051', div = '0') %>%
    gather(key = month, value = value, -code, -div, -var, -year) %>%
    mutate(month = ordered(month, levels = str_to_lower(month.abb))) %>%
    inner_join(climate_lookup, by = 'code') %>%
    arrange(state, year, month)

  temp_data <- temp_data %>% rbind(div_temp_data)

  precip_data <- read_fwf(file.path(climate_data_dir, prcp_file), widths, na = c("-9.99")) %>%
    gather(key = month, value = value, -code, -div, -var, -year) %>%
    mutate(month = ordered(month, levels = str_to_lower(month.abb))) %>%
    inner_join(climate_lookup, by = 'code') %>%
    arrange(state, year, month)

  div_precip_data <- read_fwf(file.path(climate_data_dir, div_prcp_file), div_widths, na = c("-9.99")) %>%
    dplyr::filter(code == '18' & div == '04') %>%
    mutate(code = '051', div = '0') %>%
    gather(key = month, value = value, -code, -div, -var, -year) %>%
    mutate(month = ordered(month, levels = str_to_lower(month.abb))) %>%
    inner_join(climate_lookup, by = 'code') %>%
    arrange(state, year, month)

  precip_data <- precip_data %>% rbind(div_precip_data)

  pdsi_data <- read_fwf(file.path(climate_data_dir, pdsi_file), widths, na = c("-99.99")) %>%
    gather(key = month, value = value, -code, -div, -var, -year) %>%
    mutate(month = ordered(month, levels = str_to_lower(month.abb)), div = as.character(div)) %>%
    inner_join(climate_lookup, by = 'code') %>%
    arrange(state, year, month)

  div_pdsi_data <- read_fwf(file.path(climate_data_dir, div_pdsi_file), div_widths, na = c("-99.99")) %>%
    dplyr::filter(code == '18' & div == '04') %>%
    mutate(code = '051', div = '0') %>%
    gather(key = month, value = value, -code, -div, -var, -year) %>%
    mutate(month = ordered(month, levels = str_to_lower(month.abb))) %>%
    inner_join(climate_lookup, by = 'code') %>%
    arrange(state, year, month)

  pdsi_data <- pdsi_data %>% bind_rows(div_pdsi_data)


  state_surface_water <- read_csv(file.path(climate_data_dir, 'state_2010_surface_water.csv')) %>%
    dplyr::select(state = state_short, surface.water = surfwater)
  state_rpp <- read_excel(file.path(climate_data_dir, 'regional_price_parity_by_state.xls'), 1, skip = 5) %>%
    dplyr:::select(state.fips = GeoFips, state.name = GeoName, rpp = matches(as.character(bea_year)))
  state_rpi <- read_excel(file.path(climate_data_dir, 'real_personal_income_by_state.xls'), 1, skip = 5) %>%
    dplyr::select(state.fips = GeoFips, state.name = GeoName, rpi = matches(as.character(bea_year)))
  state_pvi <- read_csv(file.path(climate_data_dir, 'pvi_by_state.csv')) %>%
    dplyr::select(state.fips, state = state.abb, pvi) %>%
    mutate(state.fips = str_c(state.fips, '000'))
  state_data <- full_join(state_pvi, state_rpp, by = 'state.fips') %>%
    full_join(state_rpi, by = c('state.fips', 'state.name')) %>%
    full_join(state_surface_water, by = 'state') %>%
    dplyr::filter(str_detect(state.fips, '^[0-9]') & as.numeric(state.fips) > 0) %>%
    mutate(affordability = rpi / rpp)

  climate_data <- bind_rows(temp_data, precip_data, pdsi_data) %>%
    dplyr::filter(! is.na(state)) %>%
    mutate(var = factor(var, levels = c('01','02','05'), labels = c('precip', 'temp', 'pdsi'))) %>%
    spread(key = var, value = value) %>%
    dplyr::filter(year >= 1970 & year <= 2014 & div == 0) %>%
    group_by(state, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp), pdsi = mean(pdsi)) %>%
    mutate(precip = precip * 25.4, temp = (temp - 32) * 5 / 9, aridity = precip / (33 + temp)) %>%
    group_by(state) %>%
    dplyr::summarize_at(vars(precip, temp, pdsi, aridity),funs(mean(., na.rm = T))) %>%
    rename(precip_70 = precip, temp_70 = temp, pdsi_70 = pdsi, aridity_70 = aridity) %>%
    ungroup()

  climate_data <- bind_rows(temp_data, precip_data, pdsi_data) %>%
    dplyr::filter(! is.na(state)) %>%
    mutate(var = factor(var, levels = c('01','02','05'), labels = c('precip', 'temp', 'pdsi'))) %>%
    spread(key = var, value = value) %>%
    dplyr::filter(year >= 1985 & year <= 2014 & div == 0) %>%
    group_by(state, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp), pdsi = mean(pdsi)) %>%
    mutate(precip = precip * 25.4, temp = (temp - 32) * 5 / 9, aridity = precip / (33 + temp)) %>%
    group_by(state) %>%
    dplyr::summarize_at(vars(precip, temp, pdsi, aridity),funs(mean(., na.rm = T))) %>%
    rename(precip_85 = precip, temp_85 = temp, pdsi_85 = pdsi, aridity_85 = aridity) %>%
    ungroup() %>%
    left_join(climate_data, ., by = "state")

  climate_data <- bind_rows(temp_data, precip_data, pdsi_data) %>%
    dplyr::filter(! is.na(state)) %>%
    mutate(var = factor(var, levels = c('01','02','05'), labels = c('precip', 'temp', 'pdsi'))) %>%
    spread(key = var, value = value) %>%
    dplyr::filter(year >= 1995 & year <= 2014 & div == 0) %>%
    group_by(state, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp), pdsi = mean(pdsi)) %>%
    mutate(precip = precip * 25.4, temp = (temp - 32) * 5 / 9, aridity = precip / (33 + temp)) %>%
    group_by(state) %>%
    dplyr::summarize_at(vars(precip, temp, pdsi, aridity),funs(mean(., na.rm = T))) %>%
    rename(precip_95 = precip, temp_95 = temp, pdsi_95 = pdsi, aridity_95 = aridity) %>%
    ungroup() %>%
    left_join(climate_data, ., by = "state")

  climate_data <- bind_rows(temp_data, precip_data, pdsi_data) %>%
    dplyr::filter(! is.na(state)) %>%
    mutate(var = factor(var, levels = c('01','02','05'), labels = c('precip', 'temp', 'pdsi'))) %>%
    spread(key = var, value = value) %>%
    dplyr::filter(year >= 2005 & year <= 2014 & div == 0) %>%
    group_by(state, year) %>%
    dplyr::summarize(precip = sum(precip), temp = mean(temp), pdsi = mean(pdsi)) %>%
    mutate(precip = precip * 25.4, temp = (temp - 32) * 5 / 9, aridity = precip / (33 + temp)) %>%
    group_by(state) %>%
    dplyr::summarize_at(vars(precip, temp, pdsi, aridity),funs(mean(., na.rm = T))) %>%
    rename(precip_05 = precip, temp_05 = temp, pdsi_05 = pdsi, aridity_05 = aridity) %>%
    ungroup() %>%
    left_join(climate_data, ., by = "state") %>%
    mutate(temp = temp_85, precip = precip_85, aridity = aridity_85, pdsi = pdsi_85)


  gini_header <- read_csv(file.path('data','gini', 'ACS_14_1YR_B19083_with_ann.csv'), n_max = 1) %>% names()
  gini <- read_csv(file.path('data','gini', 'ACS_14_1YR_B19083_with_ann.csv'), skip = 2, col_names = gini_header,
                   col_types = cols(col_character(), col_integer(), col_character(), col_double(), col_double())) %>%
    clean_names() %>%
    rename(geoid = geo_id, fips = geo_id2, place = geo_display_label, gini = hd01_vd01, gini.se = hd02_vd01) %>%
    mutate(category = ifelse(str_detect(geoid, "^[0-9]+US[0-9]+$"), "State", "MSA") %>% factor())

  state_gini <- gini %>% dplyr::filter(category == "State") %>%
    mutate(state.fips = sprintf("%02d000", fips))

  state_data <- left_join(state_data, climate_data, by = 'state') %>%
    left_join(dplyr::select(state_gini, state.fips, gini), by = "state.fips")

  indices <- names(state_data)
  new_indices <- c('state.fips', 'state', 'state.name', 'pvi', 'rpp', 'rpi',
                   'affordability',
                   'precip', 'temp', 'aridity', 'pdsi')
  new_indices <- c(new_indices, setdiff(indices, new_indices))
  state_data <- dplyr::select_(state_data, .dots = new_indices)

  write_rds(state_data, path = file.path(climate_data_dir, 'state_predictors.Rds'), compress = 'xz')

  invisible(state_data)
}
