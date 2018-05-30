library(pacman)
p_load(tidyverse)
# p_load(RCurl)
p_load(readxl)
p_load(stringr)
p_load(magrittr)
p_load(lazyeval)
p_load(lubridate)
p_load(maps)
p_load(janitor)
p_load_gh('dkahle/ggmap')
# p_load(ggmap)


data_dir = 'data'
script_dir = 'scripts'

source(file.path(script_dir, "fips_codes.R"))

build_explanatory_variables <- function(data.dir = data_dir, save = FALSE, pop_target_year = 2014, script.dir = script_dir) {
  url1 <- "https://www.census.gov/population/metro/files/lists/2015/List1.xls"
  url2 <- "https://www.census.gov/population/metro/files/lists/2015/List2.xls"

  if (!file.exists(file.path(data.dir, "cbsa_msa_csa_2015.xls")))
    download.file(url1, file.path(data.dir, "cbsa_msa_csa_2015.xls"), method = "curl")
  if (!file.exists(file.path(data.dir, "cbsa_msa_2015.xls")))
    download.file(url2, file.path(data.dir, "cbsa_msa_2015.xls"), method = "curl")

  vwci <- read_excel(file.path(data.dir, "VWCI_DATA_10_15_16.xlsx"), skip = 1, col_names = T) %>%
    set_names(str_to_lower(colnames(.))) %>%
    dplyr::rename(place = city)

  fips_codes <- get_fips()

  state_fips <- fips_codes$state_fips
  cnty2msa <- fips_codes$cnty2msa
  city2msa <- fips_codes$city2msa

  vwci_msa <- vwci %>%
    mutate(state.abb = place %>% str_trim() %>%
             str_extract('[A-Z]{2}$'),
           city = place %>% str_trim() %>%
             str_replace_all('[A-Z]{2}$',"") %>%
             str_replace_all('([a-z])([A-Z])', '\\1 \\2') %>%
             str_replace_all('(Mc) ([A-Z])', '\\1\\2'),
           city.state = str_c(city, state.abb, sep = ", ")) %>%
    left_join(city2msa, by = c("city", "state.abb"))

  vwci_no_msa <- vwci_msa %>%
    dplyr::filter(is.na(msa.fips))

  vwci_msa_fill <- data_frame(
    city.state = c("Boise, ID", "Honolulu, HI", "Indianapolis, IN",
                 "Louisville, KY", "Port St Luci, FL", "St Louis, MO"),
    name.fill = c("Boise City, ID", "Urban Honolulu, HI",
                  "Indianapolis-Carmel-Anderson, IN",
                  "Louisville/Jefferson Country, KY-IN",
                  "Port St. Lucie, FL",
                  "St. Louis, MO-IL"),
    fips.fill = c("14260", "46520", "26900", "31140", "38940", "41180"))

  vwci_msa <- vwci_msa %>%
    left_join(vwci_msa_fill, by = "city.state") %>%
    mutate(msa.name = ifelse(is.na(msa.name), name.fill, msa.name),
           msa.fips = ifelse(is.na(msa.fips), fips.fill, msa.fips)) %>%
    dplyr::select(-name.fill, -fips.fill)

  msa_pvi_data_file <- "pvi_by_msa.csv"
  state_pvi_data_file <- "pvi_by_state.csv"

  pvi <- read_csv(file.path(data.dir, msa_pvi_data_file))
  state_pvi <- read_csv(file.path(data.dir, state_pvi_data_file))

  wu_data_file = 'wudata_out_NWIS_2010.csv'

  if (! file.exists(file.path(data.dir, wu_data_file))) {
    source(file.path(script.dir, 'NWIS_WUD.R'), chdir = T)
    save_wd <- getwd()
    setwd(data.dir)
    wudata.pull("all")
    wu_out_file <- str_c('wudata_out_NWIS', today(), ".csv")
    file.rename(wu_out_file, wu_data_file)
    setwd(save_wd)
  }

  wudata <- read_csv(file.path(data.dir, wu_data_file), na = c('','-', 'na')) %>%
    dplyr::select(cnty.fips = cntyFIPS, state = state_name, county = county_nm, year,
                  gw = Public.Supply.total.self.supplied.withdrawals..groundwater..in.Mgal.d,
                  sw = Public.Supply.total.self.supplied.withdrawals..surface.water..in.Mgal.d,
                  tw = Public.Supply.total.self.supplied.withdrawals..total..in.Mgal.d) %>%
    dplyr::filter(year == 2010) %>%
    mutate(cnty.fips = str_pad(cnty.fips, 5, pad = '0')) %>%
    left_join(cnty2msa %>% dplyr::select(msa.fips, msa.name, cnty.fips), by = 'cnty.fips') %>%
    group_by(msa.fips, msa.name) %>%
    dplyr::summarize(sw = sum(sw), gw = sum(gw), tw = sum(tw)) %>%
    ungroup() %>%
    mutate(surface.water = sw / tw) %>%
    dplyr::select(msa.fips, surface.water)

  pop_target = as.name(str_c('pop', pop_target_year, sep ='.'))
  n_years = pop_target_year - 2010

  population <- suppressWarnings(read_csv(file.path(data.dir, 'PEP_2015_GCTPEPANNR.US24PR.csv'), skip=1)) %>%
    set_names(names(.) %>% str_replace_all('[^[:alnum:]]+','.')) %>%
    dplyr::select(msa.fips = Target.Geo.Id2, msa.name = Geography.2, census.2010 = April.1.2010.Census,
                  base.2010 = April.1.2010.Estimates.Base, pop.2010 = Population.Estimate.as.of.July.1.2010,
                  pop.2011 = Population.Estimate.as.of.July.1.2011, pop.2012 = Population.Estimate.as.of.July.1.2012,
                  pop.2013 = Population.Estimate.as.of.July.1.2013, pop.2014 = Population.Estimate.as.of.July.1.2014,
                  pop.2015 = Population.Estimate.as.of.July.1.2015) %>%
    mutate_(.dots = list(pop = pop_target, pop.growth = interp(~log(x / pop.2010) / n, x =as.name(pop_target), n = n_years)))

  pop_density <- read_excel(file.path(data.dir, "population_density", "cbsa-report-chapter-3-data.xlsx"), range = "A6:K952",
                            col_names = c("msa.fips", "msa.name", "category", "pop.2000", "pop.2010", "area",
                                          "pop.dens.2000", "pop.dens.2010", "pop.w.dens.2000", "pop.w.dens.2010",
                                          "delta.pop.w.dens")) %>%
    mutate(pop.dens.growth = log(pop.w.dens.2010 / pop.w.dens.2000) / 10,
           msa.fips = str_replace_all(msa.fips, fixed("31100"), "31080") %>%
             str_replace_all(fixed("42060"), "42200") %>%
             str_replace_all(fixed("26180"), "46520")) %>%
    dplyr::select(msa.fips, area, pop.dens = pop.w.dens.2010, pop.dens.growth)

  explanatory_variables <- population %>% dplyr::select(msa.fips, msa.name, pop, pop.growth) %>%
    mutate(msa.fips = as.character(msa.fips) %>% str_pad(5, pad = '0')) %>%
    left_join(wudata, by = 'msa.fips') %>% left_join(pvi, by = 'msa.fips') %>%
    left_join(vwci_msa %>% dplyr::select(msa.fips, city = place), by = 'msa.fips') %>%
    left_join(pop_density, by = "msa.fips") %>%
    dplyr::filter(! is.na(city))

  if (save) {
    write_csv(explanatory_variables, path = file.path(data.dir, 'VWCI_explanatory_variables.csv'))
    write_rds(explanatory_variables, path = file.path(data.dir, 'VWCI_explanatory_variables.Rds'), compress = 'xz')
  }

  invisible(explanatory_variables)
}
