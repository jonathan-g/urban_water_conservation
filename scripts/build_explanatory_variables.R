library(pacman)
p_load(hellno)
p_load(tidyverse)
# p_load(RCurl)
p_load(readxl)
p_load(stringr)
p_load(magrittr)
p_load(lazyeval)
p_load(lubridate)
p_load(maps)
p_load_gh('dkahle/ggmap')


data_dir = 'data'
script_dir = 'scripts'

if (file.exists(file.path(data_dir, 'proprietary', 'google_maps_api.csv'))) {
  api_key <- read_csv(file.path(data_dir, 'proprietary', 'google_maps_api.csv'))$api_key
  register_google(key = api_key, account_type = 'standard', second_limit = 50, day_limit = 20000)
}

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

  elections <- data_frame()
  for (y in c(2008, 2012)) {
    e <- data_frame()
    for (s in c('al_fl', 'ga_ky', 'la_ne', 'nv_nm', 'ny_sd', 'tn_wy')) {
      fn = str_c('cq_election_', y, '_', s, '.csv')
      e <- e %>% bind_rows(read_csv(file.path(data.dir, 'proprietary', fn), skip = 2, na = c('','NA','N/A')))
    }
    e <- e %>% mutate(year = y)
    elections <- elections %>% bind_rows(e)
  }

  state_elections <- elections %>%
    dplyr::filter((is.na(Office) & ! is.na(State)) | Office == 'CensusPopAll') %>%
    dplyr::select(which(! is.na(.[1,]))) %>%
    set_names(.[1,] %>% str_replace_all('([a-z])([A-Z])', '\\1.\\2') %>% str_to_lower()) %>%
    dplyr::rename(year = `2008`) %>%
    dplyr::filter(is.na(census.pop.all)) %>%
    dplyr::select(year, state = area.all, total.votes.all, rep.votes.all, dem.votes.all, third.votes.all, other.votes.all) %>%
    set_names(str_replace_all(names(.), '\\.all$','')) %>%
    mutate_each(funs(ifelse(is.na(.), 0, . %>% str_replace_all(',','') %>% as.numeric())), total.votes:other.votes) %>%
    mutate(dem.share = dem.votes / (dem.votes + rep.votes))

  national_elections <- state_elections %>% dplyr::select(-state) %>%
    group_by(year) %>% dplyr::summarize_all(sum) %>% ungroup() %>%
    mutate(dem.share = dem.votes / (dem.votes + rep.votes))

  state_elections <- state_elections %>%
    left_join(national_elections, by = c('year'), suffix = c('.state','.natl')) %>%
    set_names(str_replace_all(names(.), "\\.state$","")) %>%
    mutate(pvi = (dem.share - dem.share.natl) * 100,
           state = str_to_lower(state))

  msa_elections <- elections %>%
    dplyr::filter(!is.na(Office) & Office == 'President') %>%
    set_names(str_replace_all(names(.), '([a-z])([A-Z])', '\\1.\\2') %>% str_to_lower()) %>%
    dplyr::select(year, state, county = area, area.type, total.votes, rep.votes, dem.votes, third.votes, other.votes) %>%
    mutate_each(funs(ifelse(is.na(.), 0, . %>% str_replace_all(',','') %>% as.numeric())), total.votes:other.votes)

  msa_elections <- msa_elections %>%
    left_join(national_elections, by = c('year'), suffix = c('.msa','.natl')) %>%
    set_names(str_replace_all(names(.), "\\.msa$","")) %>%
    mutate(county = str_replace_all(county, "[[:punct:][:space:]]+","") %>%
             str_to_lower(),
           state = str_to_lower(state))

  msa_elections <- msa_elections %>%
    left_join(cnty_fips, by = c("county", "state"))

  anchorage <- msa_elections %>% dplyr::filter(state == 'alaska') %>%
    mutate(district = county %>% str_extract('[[:digit:]]+$') %>% as.integer()) %>%
    dplyr::filter(district >= 11 & district <= 27) %>%
    group_by(year, state) %>% dplyr::summarise_each(funs(sum(.)), ends_with(fixed('.votes'))) %>%
    ungroup() %>% mutate(cnty.fips = '02020') %>%
    left_join(national_elections, by = c('year'), suffix = c('.msa','.natl')) %>%
    set_names(str_replace_all(names(.), "\\.msa$","")) %>%
    left_join(
      cnty2msa %>% dplyr::select(msa.fips, msa.name, type, cnty.fips),
      by = 'cnty.fips'
      )

  honolulu <- msa_elections %>% dplyr::filter(state == 'hawaii') %>%
    dplyr::filter(county == 'honolulu') %>%
    mutate(cnty.fips = '15003') %>%
    set_names(str_replace_all(names(.), "\\.msa$","")) %>%
    left_join(
      cnty2msa %>% dplyr::select(msa.fips, msa.name, type, cnty.fips),
      by = 'cnty.fips'
    )

  no_fips <- msa_elections %>%
    dplyr::filter(!(state %in% c('alaska', 'hawaii')) & is.na(cnty.fips) &
                    ! county %in% c('votesnotreportedbycounty')) %>%
    mutate(county = ifelse(area.type %in% c('Absentee', 'Ward') & state == 'district of columbia',
                           'washington', county),
           city.state = str_c(county, state, sep = ", "))

  no_fips_geocodes <- no_fips %>% dplyr::select(city.state) %>% distinct() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate_geocode(city.state, "more")

  no_fips <- no_fips %>% left_join(no_fips_geocodes, by = 'city.state') %>%
    mutate(administrative_area_level_2 =
             ifelse(county == 'franklincity' & state == 'virginia', 'Accomack',
                    ifelse(county == 'galax' & state == 'virginia', 'Carroll',
                           as.character(administrative_area_level_2))),
           geocode_county = str_replace_all(administrative_area_level_2,
                                            c('District of Columbia' = 'Washington',
                                              'Okaloosa' = 'Okaloosa Main',
                                              'St. Martin Parish' = 'St. Martin North',
                                              'Currituck' = 'Currituck Main',
                                              'Galveston' = 'Galveston Main',
                                              'Accomack' = 'Accomack Main',
                                              'Chesapeake' = 'Norfolk',
                                              'Colonial Heights' = 'Dinwiddie',
                                              'San Juan' = 'San Juan Lopez Island')) %>%
             str_replace_all(("(County|Parish) *$"), "") %>%
             str_replace_all("[[:punct:][:space:]]+", "") %>%
             str_to_lower()) %>%
    dplyr::rename(no.county = county, county = geocode_county) %>%
    dplyr::select(-cnty.fips)
  # dplyr::select(state, no.county = county, county = geocode_county, area.type)

  no_fips <- no_fips %>%
    left_join(cnty_fips, by = c("county", "state"))

  found_fips <- no_fips %>% dplyr::filter(! is.na(cnty.fips)) %>% distinct() %>%
    dplyr::select(one_of(names(msa_elections)))
  no_fips <- no_fips %>% dplyr::filter(is.na(cnty.fips)) %>% distinct()


  no_fips <- cnty2msa %>%
    dplyr::filter(str_detect(county.name, ' city *$')) %>%
    mutate(city = str_replace_all(county.name, ' +city *$', '') %>%
             str_replace_all('[[:space:]]+','') %>% str_to_lower(),
           state = str_replace_all(state.name, '[[:space:]]+','') %>% str_to_lower())  %>%
    dplyr::select(city, state, county = county.name, msa.name, cnty.fips) %>%
    right_join(
      no_fips %>% dplyr::select(-cnty.fips, -county),
      by = c('city' = 'no.county', 'state' = 'state'), suffix = c('.msa', '.nf')) %>%
    dplyr::select(one_of(names(msa_elections)))

  msa_elections <- msa_elections %>% bind_rows(found_fips, no_fips)

  msa_elections <- cnty2msa %>%
    dplyr::select(msa.fips, msa.name, type, cnty.fips) %>%
    right_join(msa_elections, by = 'cnty.fips') %>%
    bind_rows(anchorage, honolulu)

  pvi <- msa_elections %>%
    dplyr::filter(! is.na(msa.fips)) %>%
    group_by(year, msa.fips) %>%
    dplyr::summarize_each(funs(sum(.)), dem.votes, rep.votes, total.votes) %>%
    ungroup() %>%
    left_join(national_elections, by = 'year', suffix = c('.msa', '.natl')) %>%
    set_names(names(.) %>% str_replace_all('\\.msa$','')) %>%
    mutate(dem.share = dem.votes / (dem.votes + rep.votes),
           dem.share.natl = dem.votes.natl / (dem.votes.natl + rep.votes.natl)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_each(funs(mean(.)), dem.share, dem.share.natl) %>%
    ungroup() %>%
    mutate(pvi = 100 * (dem.share - dem.share.natl))

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

  explanatory_variables <- population %>% dplyr::select(msa.fips, msa.name, pop, pop.growth) %>%
    mutate(msa.fips = as.character(msa.fips) %>% str_pad(5, pad = '0')) %>%
    left_join(wudata, by = 'msa.fips') %>% left_join(pvi, by = 'msa.fips') %>%
    left_join(vwci_msa %>% dplyr::select(msa.fips, city = place), by = 'msa.fips') %>%
    dplyr::filter(! is.na(city))

  state_pvi <- state_elections %>% dplyr::select(year, state, pvi) %>%
    group_by(state) %>% dplyr::summarize(pvi = mean(pvi)) %>% ungroup() %>%
    left_join( state_fips, by = c("state" = "state.name")) %>%
    dplyr::select(state.fips, state.abb, state, pvi) %>%
    arrange(state.abb)

  if (save) {
    write_csv(state_pvi, path = file.path(data.dir, 'pvi_by_state.csv'))
    write_csv(explanatory_variables, path = file.path(data.dir, 'VWCI_explanatory_variables.csv'))
    write_rds(explanatory_variables, path = file.path(data.dir, 'VWCI_explanatory_variables.Rds'), compress = 'xz')
  }

  invisible(explanatory_variables)
}
