require(ggplot2)
library(sf)
library(jpndistrict)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(fancycut)
library(scico)
library(ggsn)

#データの取得
pref_27 <- jpn_pref(27,district = FALSE)
pref_13 <- jpn_pref(13,district = FALSE)
d <- st_read(".", options = "ENCODING=CP932", stringsAsFactors = FALSE)
rail <- sf::st_transform(d,"+proj=longlat +datum=WGS84 +no_defs")
railO <- st_intersection(rail,pref_27)
railT <- st_intersection(rail,pref_13)


#retrive chain hotel data
osaka <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chosaka2.csv")
tokyo <- read.csv("https://raw.githubusercontent.com/kmbsweb/hotel-location/master/chtokyo2.csv")

osaka$yearclass <- fancycut(x = osaka$o_year,
                            intervals = c("[0,2007)", "[2007,2013)", "[2013, 2017]"),
                            buckets = c("2006年以前", "2007年～2012年", "2013年以降"),
                            unmatched.bucket = "範囲外")

tokyo$yearclass <- fancycut(x = tokyo$o_year,
                            intervals = c("[0,2007)", "[2007,2013)", "[2013, 2017]"),
                            buckets = c("2006年以前", "2007年～2012年", "2013年以降"),
                            unmatched.bucket = "範囲外")

#MAC:family = "HiraKakuPro-W3"
A <- ggplot() +
  geom_sf(data=pref_27, fill = 'white') +
  geom_sf(data=railO, color="gray") +
  geom_point(data=osaka %>% filter(yearclass=="2006年以前") , aes(x=fX, y=fY),color="navy",size=1)+
  geom_point(aes(x=135.2432,y=34.4361),color="red",  size = 1.5)+
  geom_point(aes(x=135.437986,y=34.788762), color="red",  size = 1.5)+
  coord_sf(datum = NA) +
  theme_bw() + xlab("") + ylab("") +
  geom_rect(aes(xmin = 135.46466,xmax = 135.558287,ymin = 34.636234,ymax = 34.762075),
                alpha = 0, colour = "red",size = 1, linetype = 1) 

B <- ggplot() +
  geom_sf(data=pref_27, fill = 'white') +
  geom_sf(data=railO,alpha=0.8) +
  geom_point(data=osaka %>% filter(yearclass=="2006年以前"), aes(x=fX, y=fY),color="navy")+
  theme_map()+
  coord_sf(ylim=c(34.644234,34.73575), xlim=c(135.46466, 135.558287))+
  stat_density2d(aes(x=fX, y=fY, fill = ..level.., alpha = ..level..),
                 bins = 8,h=c(.01,.01),geom = "polygon",data=osaka %>% filter(yearclass=="2006年以前")) +
  scale_fill_scico(palette = 'lajolla',guide="none") +
  theme(legend.position="none")


library(grid)
grid.newpage()
##main mapの描画
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
##navigate mapの描画
v2 <- viewport(width = 0.4, height = 0.4, x = 0.70, y = 0.80)
print(B, vp = v1)
print(A, vp = v2)


##Tokyo
#MAC:family = "HiraKakuPro-W3"
C <- ggplot() +
  geom_sf(data=pref_13, fill = 'white') +
  geom_sf(data=railT, color="gray") +
  geom_point(data=tokyo %>% filter(yearclass=="2006年以前") , aes(x=fX, y=fY),color="navy",size=1)+
  geom_point(aes(x=139.779839,y=35.549393),color="red",  size = 1.5)+
  geom_point(aes(x=140.39285,y=35.771987), color="red",  size = 1.5)+
  coord_sf(ylim=c(35.50,35.9), xlim=c(138.948130, 140.051728 ),datum = NA)+
  theme_bw() +
  xlab("") +
  ylab("") +
  geom_rect(aes(xmin = 139.65,xmax = 139.88,
                ymin = 35.5,ymax = 35.77),
            alpha = 0, colour = "red",
            size = 1, linetype = 1) 

D <- ggplot() +
  geom_sf(data=pref_13, fill = 'white') +
  geom_sf(data=railT,color="gray") +
  geom_point(data=tokyo %>% filter(yearclass=="2006年以前"), aes(x=fX, y=fY),color="navy")+
  theme_map()+
  coord_sf(ylim=c(35.5,35.77), xlim=c(139.65, 139.88),datum = NA)+
  stat_density2d(aes(x=fX, y=fY, fill = ..level.., alpha = ..level..),
                 bins = 8,h=c(.015,.015),geom = "polygon",data=tokyo %>% filter(yearclass=="2006年以前")) +
  scale_fill_scico(palette = 'lajolla',guide="none") +
  theme(legend.position="none")


library(grid)
grid.newpage()
##main mapの描画
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
##navigate mapの描画
v2 <- viewport(width = 0.8, height = 0.23, x = 0.72, y = 0.3)
print(D, vp = v1)
print(C, vp = v2)


#memo
#geom_text_repel(aes(x=135.500011,y=34.733251, label = "新大阪駅"),  size = 2.5)
#geom_text_repel(aes(x=135.496239,y=34.701301, label = "大阪駅"),  size = 2.5)
#geom_text_repel(aes(x=135.502754,y=34.66378, label = "難波"),  size = 2.5)
#geom_text_repel(aes(x=135.516143,y=34.647258, label = "天王寺"),  size = 2.5)
#geom_text_repel(aes(x=135.2432,y=34.3861, label = "関西国際空港"),  size = 2.5)
#geom_text_repel(aes(x=135.437986,y=34.788762, label = "伊丹空港"),  size = 2.5)


