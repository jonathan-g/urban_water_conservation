library(pacman)
p_load(tidyverse)
p_load(readxl)
p_load(magrittr)
p_load(lazyeval)
p_load(lubridate)
p_load(maps)
p_load(janitor)
p_load_gh('dkahle/ggmap')
# p_load(ggmap)


data_dir = 'data'
script_dir = 'scripts'

#
# Google Maps geolocation API is limited to 2500 requests per day. If you run into this limit and want to
# replicate calculating PVI from the
#
if (file.exists(file.path(data_dir, 'proprietary', 'google_maps_api.csv'))) {
  api_key <- read_csv(file.path(data_dir, 'proprietary', 'google_maps_api.csv'))$api_key[1]
  register_google(key = api_key, account_type = 'standard', second_limit = 50, day_limit = 20000)
}

source(file.path(script_dir, "fips_codes.R"))

calculate_pvi <- function(data.dir = data_dir, save = TRUE, script.dir = script_dir) {
  fips_codes <- get_fips()

  cnty_fips <- fips_codes$cnty_fips
  state_fips <- fips_codes$state_fips
  cnty2msa <- fips_codes$cnty2msa

  elections <- data_frame()
  for (y in c(2008, 2012)) {
    e <- data_frame()
    #
    # I downloaded the election files from CQ Press as
    # "2008_al_fl.csv" and "2012_al_fl.csv" for Alabama through Florida
    # "2008_ga_ky.csv" and "2012_ga_ky.csv" for Georgia through Kentucky
    # "2008_la_ne.csv" and "2012_la_ne.csv" for Louisiana through Nebraska
    # "2008_nv_nm.csv" and "2012_nv_nm.csv" for Nevada through New Mexico
    # "2008_ny_sd.csv" and "2012_ny_sd.csv" for New York through South Dakota
    # "2008_tn_wy.csv" and "2012_tn_wy.csv" for Tennessee through Wyoming
    #
    # The files are all stored in data/proprietary/
    #
    for (s in c('al_fl', 'ga_ky', 'la_ne', 'nv_nm', 'ny_sd', 'tn_wy')) {
      fn = str_c('cq_election_', y, '_', s, '.csv')
      if (!file.exists(file.path(data.dir, 'proprietary', fn))) {
        stop("ERROR: To compute PVI, you need to obtain state- and county-level vote-share data for presidential elections from CQ Press, at http://library.cqpress.com/elections/login.php?requested=%2Felections%2Fdownload-data.php")
      }
      x <- suppressWarnings(suppressMessages(
        read_csv(file.path(data.dir, 'proprietary', fn), skip = 2, na = c('','NA','N/A'))
      ))
      e <- e %>% bind_rows(x)
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
    mutate_at(vars(total.votes:other.votes),funs(ifelse(is.na(.), 0, . %>% str_replace_all(',','') %>% as.numeric()))) %>%
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
    mutate_at(vars(total.votes:other.votes),funs(ifelse(is.na(.), 0, . %>% str_replace_all(',','') %>% as.numeric())))

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
    group_by(year, state) %>% dplyr::summarise_at(vars(ends_with(fixed('.votes'))),funs(sum(.))) %>%
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
    as_tibble() %>%
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
    dplyr::summarize_at(vars(dem.votes, rep.votes, total.votes),funs(sum(.))) %>%
    ungroup() %>%
    left_join(national_elections, by = 'year', suffix = c('.msa', '.natl')) %>%
    set_names(names(.) %>% str_replace_all('\\.msa$','')) %>%
    mutate(dem.share = dem.votes / (dem.votes + rep.votes),
           dem.share.natl = dem.votes.natl / (dem.votes.natl + rep.votes.natl)) %>%
    group_by(msa.fips) %>%
    dplyr::summarize_at(vars(dem.share, dem.share.natl),funs(mean(.))) %>%
    ungroup() %>%
    mutate(pvi = 100 * (dem.share - dem.share.natl))

  state_pvi <- state_elections %>% dplyr::select(year, state, pvi) %>%
    group_by(state) %>% dplyr::summarize(pvi = mean(pvi)) %>% ungroup() %>%
    left_join( state_fips, by = c("state" = "state.name")) %>%
    dplyr::select(state.fips, state.abb, state, pvi) %>%
    arrange(state.abb)

  if (save) {
    write_csv(pvi, path = file.path(data.dir, "pvi_by_msa.csv"))
    write_csv(state_pvi, path = file.path(data.dir, 'pvi_by_state.csv'))
  }

  invisible(pvi)
}
