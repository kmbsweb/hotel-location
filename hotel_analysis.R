
#retrive chain hotel data
osaka <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chosaka.csv")
tokyo <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chtokyo.csv")

#and then...
#create index of transit level
osaka$index <- (log(osaka$kanku_dura)+log(osaka$itami_dura))
tokyo$index <- (log(tokyo$narita_dur)+log(tokyo$haneda_dur))


###########################
#osaka#####################
###########################

library(fancycut)
#区分け内容を指定:intervalsオプション;[:含める,(:含めない　←ポイント
#ラベルを指定:bucketsオプション
#区分け内容外のラベルを指定:unmatched.bucketオプション
summary(osaka$rooms)
osaka$roomsclass <- fancycut(x = osaka$rooms,
                             intervals = c("[0,128.5)", "[128.5,202.5)",  "[202.5,318.8)","[318.8, 972.0]"),
                             buckets = c("under 128.5", "128.5-202.5", "202.5-318.8","more than 318.8"),
                             unmatched.bucket = "範囲外")
library(ggplot2)
ggplot()+
  geom_point(data=osaka, aes(x=log(lvalue), y=index)) + 
  geom_vline(xintercept=mean(log(osaka$lvalue)),
             linetype=2,colour = "gray")+
  geom_hline(yintercept=mean(osaka$index),
             linetype=2,colour = "gray")+
  stat_density2d(aes(x =log(lvalue), y = index, fill = ..level.., alpha = ..level..),
                 bins = 10, geom = "polygon",data=osaka) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ roomsclass) +
  theme_classic()

###########################
#Tokyo#####################
###########################

summary(tokyo$rooms)
tokyo$roomsclass <- fancycut(x = tokyo$rooms,
                             intervals = c("[0,105.0)", "[105.0,156.0)","[156.0,238.0)","[238.0, 3560.0]"),
                             buckets = c("under 105.0", "105.0-156.0", "156.0-238.0","more than 238.0"),
                             unmatched.bucket = "範囲外")
library(ggplot2)
ggplot()+
  geom_point(data=tokyo, aes(x=log(lvalue), y=index)) + 
  geom_vline(xintercept=mean(log(tokyo$lvalue)),
             linetype=2,colour = "gray")+
  geom_hline(yintercept=mean(tokyo$index),
             linetype=2,colour = "gray")+
  stat_density2d(aes(x =log(lvalue), y = index, fill = ..level.., alpha = ..level..),
                 bins = 10, geom = "polygon",data=tokyo) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ roomsclass) +
  theme_classic()


capital_Label
μ <- mean(osaka$rooms)
σ <- sd(osaka$rooms)
μ+σ

summary(osaka$index)

summary(tokyo$index)
summary(tokyo$rooms)
