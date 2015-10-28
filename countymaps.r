##### motivates selection - provide context - political data? voteshare -

rm(list=ls())
library(ggplot2)
library(RColorBrewer)
library(maps)
library(rgdal)
library(dplyr)

setwd("~/state-descriptives")
setwd("C:/Users/frankalready/state-descriptives")


returnquant<-function(x){
  l<-9 ### number of quantiles
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
    }
    return(as.factor(quant))
}



dat1<-read.csv("nhgis0014_ds201_20135_2013_county.csv", head=TRUE)

dat1$STATE<-tolower(dat1$STATE)
dat1$COUNTY<-tolower(dat1$COUNTY)
names(dat1)[9]<-"id"
dat1$id<-as.character(strsplit(dat1$id, " county"))
  
names(dat1)[which(names(dat1)=="UEQE001")]<-"tot.pop"
names(dat1)[which(names(dat1)=="UEQE002")]<-"white.pop"
names(dat1)[which(names(dat1)=="UEQE003")]<-"black.pop"
names(dat1)[which(names(dat1)=="UEZE003")]<-"latino.pop"
names(dat1)[which(names(dat1)=="UILE001")]<-"gini"
names(dat1)[which(names(dat1)=="UJAE001")]<-"per.cap.inc"

dat1$lessHS<-with(dat1, 
  UGSE002+UGSE003+UGSE004+UGSE005+UGSE006+UGSE007+UGSE008+UGSE009
  +UGSE010+UGSE011+UGSE012+UGSE013+UGSE014+UGSE015+UGSE016)

dat1$pov2<-with(dat1,
  UG7E002+UG7E003+UG7E004+UG7E005+UG7E006+UG7E007)

dat1$unemp.rt<-with(dat1,
  UJ8E005/UJ8E002)

dat1$pov.rt<-with(dat1,
  dat1$pov2/dat1$tot.pop)

dat1$pctblk<-with(dat1,
  dat1$black.pop/dat1$tot.pop)

dat1$pctlat<-with(dat1,
  dat1$latino.pop/dat1$tot.pop)

states<-c("washington", "california", "texas", "illinois", 
  "minnesota", "missouri", "georgia", "new york")
s.dat<-list()
for(s in 1:length(states)){
  s.dat[[s]]<-dat1[dat1$STATE==states[s],]
 }


county_map <- map_data("county", "washington")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[1]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[1]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[1]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[1]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[1]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[1]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="WApctblk.pdf", plot=map.pctblk)
ggsave(filename="WApctlat.pdf", plot=map.pctlat)
ggsave(filename="WAgini.pdf", plot=map.gini)
ggsave(filename="WApcincome.pdf", plot=map.inc)
ggsave(filename="WAunemp.pdf", plot=map.unemp)
ggsave(filename="WApov.pdf", plot=map.pov)


county_map <- map_data("county", "california")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[2]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[2]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[2]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[2]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[2]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[2]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="CApctblk.pdf", plot=map.pctblk)
ggsave(filename="CApctlat.pdf", plot=map.pctlat)
ggsave(filename="CAgini.pdf", plot=map.gini)
ggsave(filename="CApcincome.pdf", plot=map.inc)
ggsave(filename="CAunemp.pdf", plot=map.unemp)
ggsave(filename="CApov.pdf", plot=map.pov)


county_map <- map_data("county", "texas")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[3]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[3]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[3]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[3]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[3]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[3]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="TXpctblk.pdf", plot=map.pctblk)
ggsave(filename="TXpctlat.pdf", plot=map.pctlat)
ggsave(filename="TXgini.pdf", plot=map.gini)
ggsave(filename="TXpcincome.pdf", plot=map.inc)
ggsave(filename="TXunemp.pdf", plot=map.unemp)
ggsave(filename="TXpov.pdf", plot=map.pov)


county_map <- map_data("county", "illinois")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[4]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[4]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[4]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[4]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[4]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[4]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="ILpctblk.pdf", plot=map.pctblk)
ggsave(filename="ILpctlat.pdf", plot=map.pctlat)
ggsave(filename="ILgini.pdf", plot=map.gini)
ggsave(filename="ILpcincome.pdf", plot=map.inc)
ggsave(filename="ILunemp.pdf", plot=map.unemp)
ggsave(filename="ILpov.pdf", plot=map.pov)


county_map <- map_data("county", "minnesota")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[5]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[5]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[5]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[5]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[5]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[5]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="MNpctblk.pdf", plot=map.pctblk)
ggsave(filename="MNpctlat.pdf", plot=map.pctlat)
ggsave(filename="MNgini.pdf", plot=map.gini)
ggsave(filename="MNpcincome.pdf", plot=map.inc)
ggsave(filename="MNunemp.pdf", plot=map.unemp)
ggsave(filename="MNpov.pdf", plot=map.pov)


county_map <- map_data("county", "missouri")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[6]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[6]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[6]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[6]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[6]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[6]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="MOpctblk.pdf", plot=map.pctblk)
ggsave(filename="MOpctlat.pdf", plot=map.pctlat)
ggsave(filename="MOgini.pdf", plot=map.gini)
ggsave(filename="MOpcincome.pdf", plot=map.inc)
ggsave(filename="MOunemp.pdf", plot=map.unemp)
ggsave(filename="MOpov.pdf", plot=map.pov)


county_map <- map_data("county", "georgia")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[7]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[7]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[7]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[7]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[7]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[7]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="GApctblk.pdf", plot=map.pctblk)
ggsave(filename="GApctlat.pdf", plot=map.pctlat)
ggsave(filename="GAgini.pdf", plot=map.gini)
ggsave(filename="GApcincome.pdf", plot=map.inc)
ggsave(filename="GAunemp.pdf", plot=map.unemp)
ggsave(filename="GApov.pdf", plot=map.pov)

s.dat[[8]]$id[45]<-"st lawrence"
county_map <- map_data("county", "new york")
names(county_map)[5:6] <- c("state", "id")

map.pctblk <- ggplot(s.dat[[8]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctblk)), 
            colour = "black") + ggtitle("Percent Black Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pctlat <- ggplot(s.dat[[8]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pctlat)), 
            colour = "black") + ggtitle("Percent Latino Population in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.gini <- ggplot(s.dat[[8]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(gini)), 
            colour = "black") + ggtitle("Gini in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.inc <- ggplot(s.dat[[8]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(per.cap.inc)), 
            colour = "black") + ggtitle("Per Capita Income in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.pov <- ggplot(s.dat[[8]])+
   geom_map( map = county_map, aes(map_id = id,fill = returnquant(pov.rt)), 
            colour = "black") + ggtitle("Poverty Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

map.unemp <- ggplot(s.dat[[8]]) +
  geom_map( map = county_map, aes(map_id = id,fill = returnquant(unemp.rt)), 
            colour = "black") + ggtitle("Unemployment Rate in Deciles") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
  scale_fill_brewer(palette="Blues", labels=c("Lowest 10%", " ", " ", " ", " ", " ", " ", " ", "Top 10%"),
    name=" ")

ggsave(filename="NYpctblk.pdf", plot=map.pctblk)
ggsave(filename="NYpctlat.pdf", plot=map.pctlat)
ggsave(filename="NYgini.pdf", plot=map.gini)
ggsave(filename="NYpcincome.pdf", plot=map.inc)
ggsave(filename="NYunemp.pdf", plot=map.unemp)
ggsave(filename="NYpov.pdf", plot=map.pov)
