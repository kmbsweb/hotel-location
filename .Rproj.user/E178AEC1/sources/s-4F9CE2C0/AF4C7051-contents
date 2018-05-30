library(Jtransit)
library(dplyr)


or <- as.character(paste(rep("東京駅", 500)))
des <- as.character(tokyo$住所)

# prepare for the blank data.frame
# ErrorPage <- NULL
Data <- data.frame()

# repetition processing
for (i in seq(or)){
  print(paste0("...", i, "行目を処理しています。"))
  exdata <- transit(or[i],des[i],12,0,0)
  #row bind
  Data <- rbind(Data, exdata)
}

tokyo <- cbind(tokyo,Data)

write.csv(osaka,"chosaka2.csv")
write.csv(tokyo,"chtokyo2.csv")

hist(A$rooms,breaks=seq(0,500,10))

A <- subset(tokyo,rooms<=500)
A <- subset(tokyo,o_year>=1980)
hist(A$o_year,breaks=seq(1980,2020,1))

