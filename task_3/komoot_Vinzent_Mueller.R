library(data.table)
library(fasttime)

wd = "/home/vinzent/Documents/r_scrips"
setwd(wd)

events = fread("tour-events.csv", sep = ',')

# remove NAs
events = events[complete.cases(events), ]


events[,timestamp := fastPOSIXct(timestamp)]


# aggregate first, last usage, coordinates of first usage
# assume a user's first coords are his home country

users = events[ , .( firstdate = min(timestamp), lastdate = max(timestamp), long = longitude[which.min(timestamp)], lat = latitude[which.min(timestamp)]), by = user]

# year cohort
users[ , firstyear := as.integer(format(firstdate, format = "%Y"))]

str(events)
summary(events)

# get countries

# taken from StackOverflow:
#
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees

library(sp)
library(rworldmap)

coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}


users[ , country := coords2country(users[ , .(long, lat)])]

table(users[, .(country)])

# only a handful of countries have useful data
users = users[country %in% c("Austria", "Germany", "Switzerland", "United States of America"), ]

users[ , country := droplevels(country)]

table(users[, .(country, firstyear)])

 # useful data from 2012, US: from 2015

users = users[firstyear >= 2012 & !(country == "United States of America" & firstyear < 2015)]

# (sloppy) month calculation
users[ , tenureMonths := as.integer(difftime(lastdate, firstdate, units = 'days')/30)]

users[ , ageMonths := as.integer(difftime(max(lastdate), firstdate, units = 'days')/30)]

# fill up all other non-final months of a user
months = data.frame(month = seq(0:max(users[, ageMonths]))-1)

users = data.table(merge(data.frame(users), months, by = NULL))

# crop months that a user cannot have lived through
users = users[ month <= ageMonths,]

# is the user still active on the last recorded date?
users[ , active := ifelse(month <= tenureMonths, 1, 0)]

# table with year & country cohorts
cohorts = users[ , .( nActive = sum(active)), by = .(country, month, firstyear)]

years = users[ , .(total = length(unique(user))), by = .(country, firstyear)]

cohorts = merge(years, cohorts, by = c("firstyear", "country"))

cohorts[ , percentActive := nActive/total]

cohorts[,firstyear := as.factor(firstyear)]

# retention heatmap
library(ggplot2)

ggplot(cohorts, aes(month, firstyear)) + geom_tile(aes(fill = percentActive), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue") + facet_wrap( ~country, scales = "free") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0), limits = rev(levels(cohorts$firstyear))) + theme_bw() 

# table in wide format, can be used further for reports 
cohort_table = reshape(cohorts[, .(country, firstyear, month, percentActive)], idvar = c("country" ,"firstyear"), timevar = "month", direction = "wide")

options(digits=2)
cohort_table[country == "Germany", 1:6]
