library(pacman)

p_load(hellno)
p_load(tidyverse)
p_load(ncdf4)
p_load(raster)
p_load(geosphere)
p_load(reshape2)
p_load(zoo)
p_load(lubridate)
p_load(magrittr)

# p_load(RColorBrewer)
# p_load_gh('dkahle/ggmap')

data_dir = 'data'


process_city_aridity <- function(data.dir = data_dir) {
  cities <- read_csv(file.path(data.dir, "VWCI_city_coordinates.csv")) # %>% dplyr::filter(city.state != 'Oakland, CA')

  if (!file.exists(file.path(data.dir, 'air.mon.mean.v401.nc'))) {
    download.file("ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/air.mon.mean.v401.nc",
                  file.path(data.dir, "air.mon.mean.v401.nc"))
  }
  if (!file.exists(file.path(data.dir, 'precip.mon.total.v401.nc'))) {
    download.file("ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/precip.mon.total.v401.nc",
                  file.path(data.dir, "precip.mon.total.v401.nc"))
  }

  # open temp and precip netcdf files
  temp_full <- nc_open(file.path(data.dir, 'air.mon.mean.v401.nc'))
  precip_full <- nc_open(file.path(data.dir, 'precip.mon.total.v401.nc'))

  # load data for just north america from Jan 1970- Dec 2014
  start = c(375,20,841) # where to start counting (lon, lat, time)
  count = c(300,150,540) # how far to count after start vector
  temp <- ncvar_get(temp_full, "air", start=start, count=count)

  precip <- ncvar_get(precip_full, "precip", start=start, count=count)
  precip <- precip * 10 # convert from cm to mm

  # extract time
  time <- ncvar_get(temp_full,"time",841,540) # Jan 1970- Dec 2014
  tunits <- ncatt_get(temp_full,"time","units")

  ## convert hours to date-time
  dt <- as.difftime(time,units="hours")
  date <- (as.POSIXct('1900-1-1 0:0:0') + dt) %>%
    strptime(., "%Y-%m-%d") %>%
    format(., "%b_%d_%Y")

  # extract lon and lat
  lon <- ncvar_get(temp_full,"lon", 375, 300) - 360 # convert to degrees west
  lat <- ncvar_get(temp_full,"lat", 20, 150)

  # only keep coordinates where there is a measurement (ie. land).
  # this ensures that the closest coordinate will be associated
  # with a value. Some coastal cities have lat and lon closer to
  # a value in the ocean which returns NA from temp and precip
  coords <- expand.grid(lon, lat) %>% rename(lon=Var1, lat=Var2)
  temp.true <- complete.cases(melt(temp[,,1]))
  coords <- coords[temp.true,] # temp or precip would work

  ## find grid index for cities and save in cities df
  cities$lon.i <- NA
  cities$lat.i <- NA
  for (i in 1:nrow(cities)) {
    # use gdist function from Imap package to find grid index for cities
    hold <- which.min(distGeo(coords, cities[i,c('lon','lat')]))
    cities$lon.i[i] <- which(lon == coords$lon[[hold]])
    cities$lat.i[i] <- which(lat == coords$lat[[hold]])
  }

  # extract monthly temp and precip data for cities using index
  cities.temp <- matrix(data=NA, nrow=nrow(cities),ncol=length(time))
  cities.precip <- cities.temp
  for (i in 1:nrow(cities)){
    cities.temp[i,] <- temp[cities$lon.i[i],cities$lat.i[i],]
    cities.precip[i,] <- precip[cities$lon.i[i],cities$lat.i[i],]
  }

  # format temp
  cities.temp %<>%
    as_data_frame() %>%
    set_names(date) %>%
    bind_cols(dplyr::select(cities, msa.fips, msa.name, city.state, city, state, lon, lat)) %>%
    gather(key = date, value = temp, -(msa.fips:lat)) %>%
    mutate(date = as_date(mdy(date)), month = month(date), year = year(date)) %>%
    dplyr::select(msa.fips:lat, month, year, temp)

  # format precip
  cities.precip %<>%
    as_data_frame() %>%
    set_names(date) %>%
    bind_cols(dplyr::select(cities, msa.fips, msa.name, city.state, city, state, lon, lat)) %>%
    gather(key = date, value = precip, -(msa.fips:lat)) %>%
    mutate(date = as_date(mdy(date)), month = month(date), year = year(date)) %>%
    dplyr::select(msa.fips:lat, month, year, precip)

  cities.aridity <- full_join(cities.temp, cities.precip,
                              by = intersect(names(cities.temp), names(cities.precip))) %>%
    mutate(aridity = precip / (temp + 33.0))

  #  cities.temp %<>% mutate(temp = temp * 9. / 5. + 32.0) # convert from C to F
  #  cities.precip %<>% mutate(precip = precip / 25.4) # convert from mm to inches

  # export to csv
  write.csv(cities.temp, file = file.path(data.dir, "udel_vwci_temp_1970_2014.csv"))
  write.csv(cities.precip, file = file.path(data.dir, "udel_vwci_precip_1970_2014.csv"))

  # write_feather(cities.aridity, file.path(data.dir, 'udel_vwci_aridity_1970_2014.feather'))
  write_rds(cities.aridity, path = file.path(data.dir, 'cities_aridity.Rds'), compress = 'xz')
  invisible(cities.aridity)
}

verification_plot <- function(aridity) {
  state = map_data('state')
  m1 <- ggplot() + coord_fixed(1.3) + theme_bw() + ggtitle("Koppen Aridity Index")
  m1 <- m1 + geom_polygon(data=state,aes(long,lat, group=group),
                          color = "black", fill= "white",size=1)
  m1 <- m1 + geom_point(data=cities.aridity %>% group_by(msa.fips) %>%
                          dplyr::summarize(lat = mean(lat), lon = mean(lon), aridity = mean(aridity)) %>%
                          dplyr::filter(lon > -130),
                        aes(lon,lat,fill=aridity),
                        shape=21,color="black",size=3)
  m1 <- m1 + scale_fill_gradientn(colours = (brewer.pal(n=11,name = 'RdYlBu')))
  print(m1)
  invisible(m1)
}
