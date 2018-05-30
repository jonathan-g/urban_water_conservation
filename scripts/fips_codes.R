library(pacman)
p_load(tidyverse)
p_load(readxl)
p_load(maps)
p_load(janitor)

data_dir = 'data'
script_dir = 'scripts'

get_fips <- function(data.dir = data_dir) {
  AK_HI <- data_frame(state.fips = c("02", "15"),
                      state.abb = c("AK", "HI"),
                      state.name = c('alaska', 'hawaii'))

  state_fips <- maps::state.fips %>%
    mutate(state_fips = str_pad(fips, 2, pad = "0"), abb = as.character(abb),
           state.name = as.character(polyname) %>% str_extract("^[^:]+")) %>%
    dplyr::select(state.fips = state_fips, state.abb = abb, state.name) %>%
    bind_rows(AK_HI) %>%
    distinct()

  cnty_fips <- maps::county.fips %>%
    mutate(state = str_extract(polyname, '^.*?(?=,)'),
           county = str_extract(polyname, '[^,]*$') %>%
             str_replace_all("[[:punct:][:space:]]+",""),
           cnty.fips = str_pad(fips, 5, pad = "0")) %>%
    dplyr::select(state, county, cnty.fips)

  cnty2msa <- read_excel(file.path(data.dir, "cbsa_msa_csa_2015.xls"), skip = 2, col_names = T) %>%
    dplyr::select(msa.fips = `CBSA Code`, msa.name = `CBSA Title`, type = `Metropolitan/Micropolitan Statistical Area`,
                  county.name = `County/County Equivalent`, state.name = `State Name`,
                  state.fips = `FIPS State Code`, county.fips = `FIPS County Code`) %>%
    mutate(cnty.fips = str_c(state.fips, county.fips) %>% str_pad(5, pad = '0')) %>%
    dplyr::select(-state.fips, -county.fips)

  city2msa <- read_excel(file.path(data.dir, "cbsa_msa_2015.xls"), skip = 2, col_names = T) %>%
    set_names(c("cbsa.fips", "cbsa.name", "type", "city", "state.fips", "place.fips")) %>%
    dplyr::filter(type == "Metropolitan Statistical Area") %>%
    mutate(city = str_replace_all(city, '^([^-]+)-.*$','\\1')) %>%
    left_join(state_fips, by = "state.fips") %>%
    dplyr::select(msa.fips = cbsa.fips, msa.name = cbsa.name, city, state.abb, state.name) %>%
    distinct()

  invisible(list(AK_HI = AK_HI, state_fips = state_fips, cnty_fips = cnty_fips,
                 cnty2msa = cnty2msa, city2msa = city2msa))
}
