library(dplyr)
library(fancycut)
#区分け内容を指定:intervalsオプション;[:含める,(:含めない　←ポイント
#ラベルを指定:bucketsオプション
#区分け内容外のラベルを指定:unmatched.bucketオプション

#retrive chain hotel data
osaka <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chosaka2.csv")
tokyo <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chtokyo2.csv")

#retrive experiemced data
osakaE <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/osaka_expand.csv")
tokyoE <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/tokyo_expand.csv")

osakaE$exclass <- fancycut(x = osakaE$expantion,
                           intervals = c("[0,5)", "[5, 22]"),
                           buckets = c("few experiemced", "experienced"),
                           unmatched.bucket = "except")
tokyoE$exclass <- fancycut(x = tokyoE$expantion,
                           intervals = c("[0,5)", "[5, 52]"),
                           buckets = c("few experiemced", "experienced"),
                           unmatched.bucket = "except")

osaka <- left_join(osaka,osakaE,by="brand")
tokyo <- left_join(tokyo,tokyoE,by="brand")


#attribute class
#open year
summary(osaka$o_year)
osaka$yearclass <- fancycut(x = osaka$o_year,
                             intervals = c("[0,2007)", "[2007,2013)", "[2013, 2017]"),
                             buckets = c("2006年以前", "2007年～2012年", "2013年以降"),
                             unmatched.bucket = "範囲外")
#rooms
#same tokyo,osaka
summary(osaka$rooms)
osaka$roomclass <- fancycut(x = osaka$rooms,
                            intervals = c("[0,224]", "(225,972]"),
                            buckets = c("224以下", "224超"),
                            unmatched.bucket = "範囲外")

#analysis
#land value
summary(osaka$lvalue)
osd <- sd(osaka$lvalue)
omean <- mean(osaka$lvalue) 
osd + omean
omean - osd
############################################
#.......................................#


#osaka st time distance
summary(osaka$osakast_dura)
osaka$otclass <- fancycut(x = osaka$osakast_dura,
                            intervals = c("[0,20]", "(20,40]", "(40,60]","(60,99]"),
                            buckets = c("20分以内", "40分以内", "60分以内","60分越"),
                            unmatched.bucket = "範囲外")

#Kansai air port time distance
summary(osaka$kanku_dura)
osaka$KIXclass <- fancycut(x = osaka$kanku_dura,
                          intervals = c("[0,20]", "(20,40]", "(40,60]","(60,80]","(80,100]","(100,156]"),
                          buckets = c("20分以内", "40分以内", "60分以内","80分以内","100分以内","100分越"),
                          unmatched.bucket = "範囲外")

#Itami air port time distance
summary(osaka$itami_dura)
osaka$ITMclass <- fancycut(x = osaka$itami_dura,
                           intervals = c("[0,20]", "(20,40]", "(40,60]","(60,80]","(80,100]","(100,154]"),
                           buckets = c("20分以内", "40分以内", "60分以内","80分以内","100分以内","100分越"),
                           unmatched.bucket = "範囲外")

write.csv(osaka,"osaka.csv")
