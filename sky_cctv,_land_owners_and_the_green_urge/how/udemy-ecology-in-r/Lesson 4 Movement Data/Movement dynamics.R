##############################################################
################# Movement Dynamics ##########################
##############################################################

# First we will set our working directory, install some new 
# packages, and load other existing ones

setwd("")


install.packages("adehabitatHR")


library(adehabitatHR)
library(sp)
library(ggplot2)
library(dplyr)
library(lubridate)
library(mapview)


# ~~~~~~~~~~~~~~~~~~~ Movement Data Query ~~~~~~~~~~~~~~~~~~~~~~~~

# We will use the example dataset used by Benhamou (2011) as provided
# by the adehabitatHR vignette. The move package allows us to query movement
# data from the MoveBank database, however, not all of the data is available
# as open-source, and many of them have strict useage rights that restrict
# use to exploratory analysis only. 
data(buffalo)

# --------------------------------------------------------------------------


# to make the data resemble raw data we would usually have from a telemetry
# study, we will first extract and select the data we want
dat <- buffalo[["traj"]][[1]]
# if we don't call the dplyer:: package first, we will run an ambiguous error
# this is because one of the other packages we're currently using 
dat <- dplyr::select(dat, x, y, date)

#first, lets change our date column to "datetime"
# since it contains both

# lets change our date to a universal format, readable by R
dat$datetime <- as.POSIXct(dat$date, format = "%Y-%m-%d %hh:%mm:%ss")

# Now we can isolate the date only and add that as the date column
dat$date <- as.Date(dat$datetime, format="%Y-%m-%d")


# our coordinates are currently don't have a projecton, which we need 
# so we will have to see what their epsg is by 
# checking a coordinate pair on https://www.geoplaner.com/
#----------------------------------------------------
dat$x[1]
dat$y[1]

# as we can se from the coordinates
# the projection falls within "zone 32"
# now we can visit https://spatialreference.org/ref/epsg/wgs-84-utm-zone-32n/
# to find that the projection CRS is 32632
#-----------------------------------------------


# now add the CRS projection to the data and create a spatial object
sp.UTM <- SpatialPoints(cbind(dat$x, dat$y), # always remember to put x first and then y after
                        proj4string = CRS("+init=epsg:32632")) 
# now we can reproject our data into lat/long format for plotting
# purposes later
sp.latlong <- spTransform(sp.UTM,
                          CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# now lets check where our points land on the map
mapview(sp.latlong)

# lets add our lat/lon coords to our "dat" data frame
sp.latlong.df <- as.data.frame(sp.latlong)
dat$lon <- sp.latlong.df$coords.x2
dat$lat <- sp.latlong.df$coords.x1


# ~~~~~~~~~~~~~~ Get movement info ~~~~~~~~~~~~~~~~~~~~~~~~

# get the range of dates
daterange <-paste0(min(dat$date),"_to_",
                   max(dat$date))

# check the range
daterange

# number of moves recorded is simply the number of rows 
# in the data frame
numberofmoves <- length(dat$date)

# number of days of recorded movement can be identified in the same
# way, however we will use the "unique()" function to remove duplicate
# dates from the count
numberofdaysmoving <- length(unique(dat$date))



# to find total distance, we use a loops to add all 
# values in the distance moved column, and ignore NA values
# we start the loop from 2, because the first record will not 
# have a corresponding distance
utms <- as.data.frame(sp.UTM)

distances <- NULL
for(i in 2:length(dat$date)){
  c <- sqrt((utms$coords.x1[i] - utms$coords.x1[i - 1])^2 +
              (utms$coords.x2[i] - utms$coords.x2[i - 1])^2)
  distances <- c(distances, c)
}
sum(distances)

# total distance will be recorded in meters
totaldistance <- sum(distances)

# average move distance iis equal to the total distance
# divided by the total number of moves.
avgpermove <- totaldistance/numberofmoves

# store our date in a values object called "Dates"
Dates <- dat$date

# remove any NA vaues (if there are any)
Dates <- na.omit(Dates)

# then get the difference between max and min dates in the dataset this will
# come as a date formatted result expressing the answer in days - not the
# easiest to work with so the as.numeric forces it to be a plain number that is
# easier to manipulate
days <- as.numeric(max(Dates)-min(Dates))

# average move per day is simply the total distance 
# divided by the total number of moves
avgpermove <- totaldistance/numberofmoves
avgperday <- totaldistance/numberofdaysmoving


# Now we can create a list of results we have acquired from our previous
# analaysis.
listofresults <- list("numberofdaysmoving", "numberofmoves", "totaldistance",
                      "avgpermove", "avgperday")

# first we make output, somewhere to store the results.
# we need to be binding things to it repeatedly and we don't want
# to keep redefining the results desination every run of the loop
output <- NULL
# This loops runs every value in the list that we've created
# and  binds it as a new row to the output
for(result in listofresults){
  if(exists(result)){
    resultreal <- get(result)
    resultreal <- cbind(result, resultreal)
    output <- rbind(output, resultreal)
  }
}

# we will convert our output to a data frame
output <- data.frame(output)
# and we will change the column names 
names(output) = c("Summary Type", "Result")
# View output
output

# Now we can write our summary to the current working directory
write.csv(output,"movement_summary.csv")


# ~~~~~~~~~~~~~~~ Plot distance over time ~~~~~~~~~~~~~~~~~~~~~

# Now lets say we want some simple figures showing movement distance
# over our study period. We can make a few graphs to visualize that

# First, we will makea "months" column  to use later in a 
# gridded pot
dat$months <- format(dat$date, format = "%B")

# then we will add our distances that we createdin our 
# pevious analysis to the data frame
# now we will add the distances column. We start at 0 because 
# of the missing row
dat$distmoved <- c(0, distances)
# To plot the progression of distance we need the cumulative distance
# so we will make another column and use the cumsum() 
# function to calculate it
dat$sumMoves <- cumsum(dat$distmoved)


# Now we will make a first plot of overall movemenet x time
ggplot(dat, aes(x = date, y = sumMoves, col = "red"))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  theme(legend.position = "none")

# We can also make the same graph with month by month values
# we add "facet_wrap()" to make the divided panels, and add the 
# argument "scales = "free" to close the empty gaps in data
ggplot(dat, aes(x = date, y = sumMoves, col = "red"))+
  geom_line()+
  geom_point()+
  facet_wrap(~months, scales = "free")+
  theme_minimal()+
  theme(legend.position = "none")

# as we can see, the animal moved more in June than in May
# likely because of the small sample size not capturing 
# as much movement for May


# END