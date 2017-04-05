library(pacman)
p_load(tidyverse, readxl, readr, stringr)

vwci <- read_excel(file.path(data_dir, 'VWCI_DATA_10-06-2016_COMPLETE_CORRECTED.xlsx')) %>%
  dplyr::filter(!str_detect(CITY, 'OaklandCA') & ! str_detect(CITY, '(AK|HI)$'))

x <- vwci %>% gather(-(CITY:STATE), key = action, value = value) %>%
  rename(city = CITY, state = STATE) %>%
  dplyr::filter(! str_detect(action, '(PER|RATIO)$')) %>%
  mutate(action = action %>% str_replace_all('^(.*)(TOTAL)$','TOT\\1') %>% str_replace_all('VWCI','TOTVWCI'),
         category = action %>% str_replace_all('^([RC]RE[QB]|[RC]OTH|[RC]BS|O|D|TOT).*$','\\1') %>% ordered(),
         acat = str_sub(category, 1, 1) %>% ordered(),
         bcat = ifelse(str_detect(category, '^TOT'), as.character(category),
                       ifelse(str_length(category) <= 1, as.character(category), str_sub(category, 2)))) %>%
  arrange(bcat, acat, action)


detailed.totals <- x %>% group_by(city, state, category) %>%
  summarize(total = sum(value)) %>% ungroup()

broad.totals <- x %>% group_by(city, state, bcat) %>%
  summarize(total = sum(value)) %>% ungroup()

detailed.totals <- x %>% group_by(city, state, category, acat, bcat) %>%
  summarize(total = sum(value)) %>% ungroup() %>% arrange(city, bcat) %>%
  dplyr::filter(str_detect(city, '^Port'))
broad.totals <- x %>% group_by(city, state, bcat) %>%
  summarize(total = sum(value)) %>% ungroup() %>% arrange(city, bcat) %>%
  dplyr::filter(str_detect(city, '^Port'))
