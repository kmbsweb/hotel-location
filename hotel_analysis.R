
#retrive chain hotel data
osaka <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/osakach.csv")
tokyo <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/tokyoch.csv")

#and then...
library(Jtransit)
library(dplyr)

time    <- tokyo$narita_dur
or <- tokyo$hotel_name
dataF <- data.frame(origin=or,time=time)

# result
DD <- covert_m(dataF,2)
tokyo$narita_dur <- DD$Time.sum

write.csv(tokyo,"chtokyo.csv")
