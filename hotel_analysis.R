
#retrive chain hotel data
osaka <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chosaka.csv")
tokyo <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chtokyo.csv")

#and then...
#create index of transit level
osaka$index <- (log(osaka$kanku_dura)+log(osaka$itami_dura))





summary(osaka$index)
summary(osaka$rooms)
summary(tokyo$rooms)
