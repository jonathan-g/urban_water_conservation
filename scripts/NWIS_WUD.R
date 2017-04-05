

wudata.pull <- function(states){

# build the urls
state <- cbind(c('DC', state.abb), c('District of Columbia', state.name)) # add DC
state.ab <- state[order(state[,2]),1] # sort by state name and not abbreviation
urlleft <- 'http://waterdata.usgs.gov/'
urlright1 <- '/nwis/water_use?format=rdb&rdb_compression=value&wu_area=County&wu_year=ALL'
urlright2 <- '&wu_county=ALL&wu_category=ALL&wu_county_nms=--ALL%2BCounties--&wu_category_nms=--ALL%2BCategories--'
urls <- paste(urlleft,tolower(state.ab),urlright1,urlright2,sep="")

urlfile <- data.frame(urls,state.ab,stringsAsFactors=FALSE)

# subset for the provided states
if (identical(states,'all')){urlfile <- urlfile[,1]
} else {
rowindex <- grep(paste(states,collapse="|"), urlfile[,2])
urlfile <- urlfile[rowindex,1]
}

# read in first state as a starter
start <- data.frame(read.table(url(urlfile[1]), header=T, sep = "\t", na.strings = "-"))
start <- start[-1,]

if(length(urlfile) == 1){wudata.out = start
} else {

# for loop to add each state
pb <- txtProgressBar(min = 0, max = length(urlfile)-1, style = 3)

for (i in 2:length(urlfile)) {
  X <- read.table(url(urlfile[i]), header=T, sep ="\t", na.strings ="-")
  X <- X[-1,]
  wudata.out <- rbind(start,X)
  start <- wudata.out
  setTxtProgressBar(pb, i-1)
}
}

# add fips codes
cntyFIPS <- paste0(wudata.out$state_cd,wudata.out$county_cd)
wudata.out <- cbind(cntyFIPS,wudata.out)

# write the file to a csv add leading zeros to FIPS in excel
fname = paste(paste('wudata_out_NWIS', Sys.Date(), sep=""), "csv", sep = ".")
write.csv(wudata.out, file = fname, row.names=F, na="-")
}

# example for select states
# wudata.pull(c('TN','GA','SD','TX','CA'))

# example for all states
# wudata.pull('all')





