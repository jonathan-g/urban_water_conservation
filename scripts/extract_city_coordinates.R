library(pacman)

p_load(tidyverse)
p_load(stringr)
p_load_gh('dkahle/ggmap')
p_load(readxl)

if (file.exists(file.path(data_dir, 'proprietary', 'google_maps_api.csv'))) {
  api_key <- read_csv(file.path(data_dir, 'proprietary', 'google_maps_api.csv'))$api_key
  register_google(key = api_key, account_type = 'standard', second_limit = 50, day_limit = 20000)
}


extract_city_coordinates <- function(data.dir = data_dir, recalc_coords = TRUE) {
  ancillary <- read_csv(file.path(data.dir, 'VWCI_explanatory_variables.csv')) %>%
    dplyr::select(place = city, msa.fips, msa.name) %>%
    mutate(place = str_trim(place), place = str_replace_all(place, fixed("ManchesterNY"), "ManchesterNH"))

  new_vwci <- read_excel(file.path(data.dir, 'VWCI_DATA_10_15_16.xlsx'), skip = 1, col_names = TRUE) %>%
    set_names(str_to_lower(names(.))) %>%
    rename(place = city) %>%
    mutate(place = str_trim(place)) %>%
    left_join(ancillary, by = 'place')

  reqreb <- new_vwci %>% dplyr::select(-vwci, -msa.fips, -msa.name) %>%
    gather(-place, key = index, value = value) %>%
    dplyr::filter(str_detect(index, '^[cr]re[qb]')) %>%
    mutate(req = str_detect(index, '^[cr]req') & value, reb = str_detect(index, '^[cr]reb') & value) %>%
    group_by(place) %>% summarize(reqtotal = sum(req), rebtotal = sum(reb)) %>%
    ungroup()

  new_vwci <- new_vwci %>% left_join(reqreb, by = 'place')

  if (FALSE) {
    other_vwci <- read_excel('data/VWCI_DATA_10-06-2016_COMPLETE_CORRECTED.xlsx', col_names = TRUE) %>%
      set_names(str_to_lower(names(.))) %>%
      dplyr::select(place = city, vwci, reqtotal, rebtotal)

    compare <- new_vwci %>% dplyr::select(place, vwci, reqtotal, rebtotal) %>%
      full_join(other_vwci %>% dplyr::select(place, vwci, reqtotal, rebtotal),
                by = 'place', suffix = c('.new','.other'))
  }

  new_vwci <- new_vwci %>%
    dplyr::select(place, msa.fips, msa.name, vwci, reqtotal, rebtotal) %>%
    mutate(
      place = str_trim(place),
           state = str_extract(place, '[A-Z]{2}$'),
           city = str_replace_all(place, '^(.*)[A-Z]{2}$','\\1') %>%
             str_replace_all('([a-z])([A-Z])', '\\1 \\2') %>%
             str_replace_all('(Mc) +([A-Z])', '\\1\\2'),
           city.state = str_c(city, state, sep = ', ')
    )

  if (recalc_coords || ! file.exists(file.path(data.dir, 'VWCI_city_coordinates.csv'))) {
    new_vwci <- new_vwci %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate_geocode(city.state, output = 'latlon', source = 'google')
  } else {
    old_coords <- read_csv(file.path(data.dir, 'VWCI_city_coordinates.csv')) %>%
      dplyr::select(msa.fips, lat, lon)
      new_vwci <- new_vwci %>% left_join(old_coords, by = 'msa.fips')
  }
  new_vwci <- new_vwci %>%
    dplyr::select(msa.fips, msa.name, city.state, city, state, lat, lon, vwci, reqtotal, rebtotal)

  write_csv(new_vwci, path = file.path(data.dir, 'VWCI_city_coordinates.csv'))

  # gcdf <- geocode(new_vwci$city.state, output = 'latlon', source = 'google')
  invisible(new_vwci)
}
