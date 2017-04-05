
library(pacman)
pacman::p_load(dplyr,magrittr,stringr,readxl)

# source custom function for downloading USGS water use data
source("https://raw.githubusercontent.com/scworland-usgs/usgs-water-use/master/NWIS_WUD.R")

# download water use data (takes 5-10 mins)
wudata.pull("all")

# load and clean water use data
wudata <- read.csv("wudata_out_NWIS2016-10-26.csv", stringsAsFactors = F, na.strings = "-") %>%
  # select columns, note: the csv has horrible column names, hence the mess below
  select(cnty_fips=cntyFIPS,state=state_name,county=county_nm,year,
         gw=Public.Supply.total.self.supplied.withdrawals..groundwater..in.Mgal.d,
         sw=Public.Supply.total.self.supplied.withdrawals..surface.water..in.Mgal.d,
         tw=Public.Supply.total.self.supplied.withdrawals..total..in.Mgal.d) %>%
  filter(year==2010) %>% # subset for only the year of interest
  mutate(cnty_fips = str_pad(cnty_fips,5, pad = "0"), # add leading zero
         surface.water = sw/tw) # calculate fraction surface water for each county

# url for census cnty and MSA file
url1 <- "https://www.census.gov/population/metro/files/lists/2015/List1.xls"

# download census file
download.file(url1, "cbsa_msa_csa_2015.xls", method = "curl")

# load cnty2msa file
cnty2msa <- read_excel("cbsa_msa_csa_2015.xls", skip = 2, col_names = T)[,c(1,4,5,10,11)] %>%
  set_colnames(c("msa_fips","msa_name","type","state_fips","cnty_fips")) %>%
  filter(type=="Metropolitan Statistical Area") %>% # filter for MSAs
  mutate(cnty_fips = paste0(state_fips,cnty_fips) %>% # build full FIPS codes
           str_pad(5, pad = "0")) %>% # add leading zero
  select(msa_fips,msa_name,cnty_fips)

# aggregate wu data into MSAs
wudata_msa <- wudata %>%
  left_join(cnty2msa,by="cnty_fips") %>%
  group_by(msa_fips) %>%
  dplyr::summarize(sw_sum = sum(sw),
                   tw_sum= sum(tw),
                   surface.water = sw_sum/tw_sum)