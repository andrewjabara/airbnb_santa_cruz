#Using data from insideairbnb.com - data from the Santa Cruz, CA area, last updated October 2015
santacruz = read.csv("documents/airbnb_santacruz.csv")
attach(santacruz)

#thanks to http://www.geodatasource.com/developers/javascript for ideas behind this function
#Note that "West" and "South" need negative signs for lat/lon
distance <- function(lat1, lon1, lat2, lon2) {
    #lat2 <- 36.9643, lon2 <- -122.0189 for Santa Cruz Beach Boardwalk
    radlat1 = pi * lat1/180
    radlat2 = pi * lat2/180
    theta = lon1-lon2
    radtheta = pi * theta/180
    dist = (sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(radtheta))
    dist = acos(dist)
    dist = dist * 180/pi
    dist = dist * 60 * 1.1515
    return(dist)
}

#use distance to regress price on distance, split by type of room (maybe focus on home first)
santacruz$distance<- distance(santacruz$latitude, santacruz$longitude,36.9643,-122.0189)

santacruz$room_type <- factor(santacruz$room_type)
lm <- lm(santacruz$price ~ santacruz$distance + santacruz$room_type)
summary(lm)

#These two regressions indicate somewhat odd findings. 
#For one, the farther a house is away from the Santa Cruz Beach Boardwalk, 
#the more expensive it is to stay per night. 
#For another, you have very big negative coefficients for Private Room or Shared Room categories. 
#The latter is easily explained upon realizing that the third category, renting a full house, 
#should logically be more expensive per night. The former (positive distance variable) may be explained 
#by physically looking through the data, where a lot of houses 
#in the "Santa Cruz" area are actually closer to Capitola Beach.

santacruz$capitola<- distance(santacruz$latitude, santacruz$longitude,36.9716, -121.9516)
lmcap <- lm(santacruz$price ~ santacruz$capitola + santacruz$room_type)
summary(lmcap)

mean(santacruz$latitude)
mean(santacruz$longitude)

#I tried using Capitola Beach as another distance measure, but again the further away a house was from the beach, 
#the more my model predicted it would cost. 
#Upon checking the average location for latitude and longitude, I figured out the reason why: 
#this dataset ranges from Santa Cruz Beach Boardwalk to Capitola Beach, 
#making both anchor points the extremes of their datasets: 
#https://www.google.com/maps/place/36%C2%B059'12.2%22N+121%C2%B059'13.0%22W/@36.9792869,-122.0092925,13.42z/data=!4m5!3m4!1s0x0:0x0!8m2!3d36.9867106!4d-121.9869506

#Separate Capitola from Santa Cruz Beach houses (focus on Santa Cruz Boardwalk first)
#first, see what neighborhoods are in there
unique(neighbourhood)

sconly <- subset(santacruz, santacruz$neighbourhood == "City of Santa Cruz")
sconly$distance<- distance(sconly$latitude, sconly$longitude,36.9643,-122.0189)
lmsconly <- lm(sconly$price ~ sconly$distance + sconly$room_type)
summary(lmsconly)

caponly <- subset(santacruz, santacruz$neighbourhood == "City of Capitola")
caponly$capitola<- distance(caponly$latitude, caponly$longitude,36.9716, -121.9516)
lmcaponly <- lm(caponly$price ~ caponly$capitola + caponly$room_type)
summary(lmcaponly)

#Now the adage of "location, location, location" is correctly displayed in the regression for Capitola
#the further away a house is from Capitola Beach, the less a person should be willing to pay to stay in an Airbnb), 
#and is significant at the 5% level. The sign on the Santa Cruz coefficient is still positive, 
#but it's not significant for the regression.  
