
library(pacman)
pacman::p_load(dplyr,magrittr,stringr,readxl,maps)

#------------------------------------------------
# Download data
#------------------------------------------------

## census urls
url1 <- "https://www.census.gov/population/metro/files/lists/2015/List1.xls"
url2 <- "https://www.census.gov/population/metro/files/lists/2015/List2.xls"

## download files
download.file(url1, "cbsa_msa_csa_2015.xls", method = "curl")
download.file(url2, "city_msa_2015.xls", method = "curl")

## load files into R
cnty2msa <- read_excel("cbsa_msa_csa_2015.xls", skip = 2, col_names = T)
city2msa <- read_excel("city_msa_2015.xls", skip = 2, col_names = T)

#------------------------------------------------
# Link VWCI to MSAs
#------------------------------------------------

## load vwci file with location, coordinates, and vwci score
vwci <- read.csv("vwci_october_2016.csv", colClasses="character")

## manually add AK and HI to state fips
AK_HI <- data.frame(state_fips=c("02","15"), 
                    state_abb=c("AK","HI"))

## load state fips from "maps" package
state.fips <- state.fips %>% 
  mutate(state_fips = str_pad(fips, 2, pad = "0")) %>%
  select(state_fips,state_abb = abb) %>%
  bind_rows(AK_HI)

## load and clean city2msa file before joining to vwci
city2msa <- read_excel("city_msa_2015.xls", skip = 2, col_names = T) %>%
  set_colnames(c("cbsa_fips","cbsa_name","type","city","state_fips","place_fips")) %>%
  filter(type=="Metropolitan Statistical Area") %>% # select only MSAs
  mutate(city2 = str_extract(city, '^.*?(?=-)'), # some cities are combined with hyphen
         city = ifelse(!is.na(city2),city2,city)) %>% # replace with city before hyphen
  left_join(state.fips,by="state_fips") %>% # add state abb
  select(msa_fips = cbsa_fips,msa_name = cbsa_name,city,state_abb) %>%
  distinct() # remove duplicated cities

## join msa data to vwci file
vwci_msa <- vwci %>%
  mutate(state_abb = str_extract(location, '[A-Z]{2}$') %>% # extract state
           str_trim(),  # remove any trailing white space
         city = str_replace_all(location, '.{4}$',"") %>% # extract city
           str_trim()) %>% # remove any trailing white space
  left_join(city2msa, by=c("city","state_abb")) # join

## select only cities without MSA
vwci_no_msa <- vwci_msa %>%
  filter(is.na(msa_fips))

# manually fill in MSA columns
vwci_msa_fill <- data.frame(location=vwci_no_msa$location,
                            name.fill=c("Boise City, ID",
                                        "Urban Honolulu, HI",
                                        "Indianapolis-Carmel-Anderson, IN",
                                        "Louisville/Jefferson County, KY-IN",
                                        "Port St. Lucie, FL",
                                        "St. Louis, MO-IL"),
                            fips.fill=c("14260",
                                        "46520",
                                        "26900",
                                        "31140",
                                        "38940",
                                        "41180"),
                            stringsAsFactors=FALSE)


# add missing MSAs using join and ifelse
vwci_msa %<>%
  left_join(vwci_msa_fill, by="location") %>%
  mutate(msa_name=ifelse(is.na(msa_name), name.fill, msa_name)) %>%
  mutate(msa_fips=ifelse(is.na(msa_fips), fips.fill, msa_fips)) %>%
  select(-c(name.fill,fips.fill))

#------------------------------------------------
# Add County FIPS and MSA FIPS to pres_data
#------------------------------------------------

## load the county fips file from "maps" package
## and prepare for merging with pres_data file
cnty.fips <- county.fips %>% # data from "maps" package
  mutate(state = str_extract(polyname, '^.*?(?=,)'), # extract state
         county = str_extract(polyname, '[^,]*$') %>% # extract county
           str_replace_all("[[:punct:]]","") %>% # remove punctuation
           str_replace_all(fixed(" "), ""), # remove all spaces
         cnty_fips = str_pad(fips, 5, pad = "0")) %>% # add leading zero
  select(state,county,cnty_fips)

## Load pres_data file, clean, and merge
pres_data <- read.csv("presidential_votes_2008_2012.csv", stringsAsFactors=F) %>%
  mutate(county = str_replace_all(county, "[[:punct:]]","") %>% # remove punctuation
           str_replace_all(fixed(" "), ""), # remove white spaces
         state = str_to_lower(state)) %>% # make state name lower case
  filter(county != "votesnotreportedbycounty") %>% # remove flag
  left_join(cnty.fips,by=c("county","state")) # join by county and state

## find which counties did not merge and try using mutate_geocode
no_fips <- pres_data %>%
  filter(!(state %in% c("alaska","hawaii"))) %>% 
  filter(is.na(cnty_fips)) %>% # select only counties w/out FIPS
  distinct(state,county) %>% # because there are two years
  mutate(location = paste(county,state,sep=",")) %>% 
  mutate_geocode(location ,"more") %>% # geo_code
  mutate(geocode_county = administrative_area_level_2 %>% # grab county
           str_replace_all(fixed("County"), "") %>% # remove word "County"
           str_replace_all("[[:punct:]]","") %>% # remove punctuation
           str_replace_all(fixed(" "), "") %>% # remove all spaces
           str_to_lower()) %>% # make lower case
  select(state,no.county=county,county=geocode_county) %>%
  left_join(cnty.fips, by=c("county","state")) # join again

## find index of counties still missing fips
x <- which(is.na(no_fips$cnty_fips))

## manually add fips codes
no_fips$cnty_fips[x] <- c("12091","22099","37053","48167",
                          "51001","51059","51710","51041",
                          "51143","51059","51175","51035",
                          "51165","51149","51031","51153",
                          "51053","51199","51710","51121",
                          "51095","53055")
## select relevant columns
no_fips %<>% select(state,county=no.county,cnty_fips_match=cnty_fips)

## join and use ifelse to add missing FIPS and aggregate by county FIPS 
## because we will have some duplicate counties when we assigned FIPS 
## to the cities in VA
pres_data_fips <- pres_data %>%
  left_join(no_fips,by=c("county","state")) %>%
  mutate(cnty_fips = ifelse(is.na(cnty_fips), cnty_fips_match,cnty_fips)) %>%
  select(-cnty_fips_match) %>%
  group_by(cnty_fips, year, state) %>%
  dplyr::summarize(total_votes=sum(total_votes),
                   rep_prop = mean(rep_prop),
                   dem_prop = mean(dem_prop)) %>%
  ungroup()

## load cnty2msa file
# load cnty2msa file
cnty2msa <- read_excel("cbsa_msa_csa_2015.xls", skip = 2, col_names = T)[,c(1,4,5,10,11)] %>%
  set_colnames(c("msa_fips","msa_name","type","state_fips","cnty_fips")) %>%
  filter(type=="Metropolitan Statistical Area") %>% # filter for MSAs
  mutate(cnty_fips = paste0(state_fips,cnty_fips) %>% # build full FIPS codes
           str_pad(5, pad = "0")) %>% # add leading zero
  select(msa_fips,msa_name,cnty_fips)

## Add MSA fips
pres_data_msa <- pres_data_fips %>%
  left_join(cnty2msa,by="cnty_fips")

#------------------------------------------------
# Cooks PVI
#------------------------------------------------

# 2008 rep and dem head to head
pres2008 <- pres_data_msa %>%
  filter(!is.na(cnty_fips)) %>%
  filter(year==2008) %>%
  mutate(dem08 = dem_prop*total_votes, 
         rep08 = rep_prop*total_votes, 
         rep_dem08 = dem08 + rep08,
         dem2008 = dem08/rep_dem08) %>%
  select(cnty_fips,dem08,rep_dem08)

# 2012 rep and dem head to head
pres2012 <- pres_data_msa %>%
  filter(!is.na(cnty_fips)) %>%
  filter(year==2012) %>%
  mutate(dem12 = dem_prop*total_votes, 
         rep12 = rep_prop*total_votes, 
         rep_dem12 = dem12 + rep12,
         dem2012 = dem12/rep_dem12) %>%
  select(cnty_fips,msa_fips,msa_name,dem12,rep_dem12)

# calculate national share
dn08 <- sum(pres2008$dem08)/sum(pres2008$rep_dem08)
dn12 <- sum(pres2012$dem12)/sum(pres2012$rep_dem12)
mn_nat_dem = (dn08 + dn12)/2

# calculate pvi for counties
cnty_pvi <- pres2008 %>%
  left_join(pres2012, by="cnty_fips") %>%
  mutate(pdem08 = dem08/rep_dem08,
         pdem12 = dem12/rep_dem12,
         mn_dem = (pdem08 + pdem12)/2,
         cnty.pvi = (mn_dem - mn_nat_dem)*100)

## calculate PVI for each MSA
msa_pvi <- pres2008 %>%
  left_join(pres2012, by="cnty_fips") %>%
  group_by(msa_fips) %>%
  dplyr::summarize(dem08 = sum(dem08),
                   dem12 = sum(dem12),
                   rep_dem08 = sum(rep_dem08),
                   rep_dem12 = sum(rep_dem12)) %>%
  mutate(pdem08 = dem08/rep_dem08,
         pdem12 = dem12/rep_dem12,
         mn_dem = (pdem08 + pdem12)/2,
         msa.pvi = (mn_dem - mn_nat_dem)*100) %>%
  select(msa_fips, msa.pvi)

## Link to VWCI database
vwci_msa_pvi <- vwci_msa %>%
  left_join(msa_pvi, by="msa_fips") %>%
  select(location,msa_name,msa_fips,lat,lon,pvi=msa.pvi)