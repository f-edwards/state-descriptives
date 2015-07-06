##### motivates selection - provide context - political data? voteshare -



rm(list=ls())
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(rgdal)
library(dplyr)

setwd("~/Dropbox/LFO/r")

dat1<-read.csv("nhgis0014_ds201_20135_2013_county.csv", head=TRUE)
dat1<-dat1[dat1$STATE=="Washington",]

dat1$STATE<-tolower(dat1$STATE)
dat1$COUNTY<-tolower(dat1$COUNTY)
names(dat1)[9]<-"id"
dat1$id<-as.character(strsplit(dat1$id, " county"))

set.seed(47)
county_map <- map_data("county", "washington")
names(county_map)[5:6] <- c("state", "id")

### MAKE DF vars
names(dat1)[which(names(dat1)=="UEQE001")]<-"tot.pop"
names(dat1)[which(names(dat1)=="UEQE002")]<-"white.pop"
names(dat1)[which(names(dat1)=="UEQE003")]<-"black.pop"
names(dat1)[which(names(dat1)=="UEZE003")]<-"latino.pop"
names(dat1)[which(names(dat1)=="UILE001")]<-"gini"
names(dat1)[which(names(dat1)=="B19301")]<-"per.cap.inc"


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

map.wa.pctblk <- ggplot(dat1)+
   geom_map( map = county_map, aes(map_id = id,fill = pctblk), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)

map.wa.pctlat <- ggplot(dat1)+
   geom_map( map = county_map, aes(map_id = id,fill = pctlat), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)

map.wa.gini <- ggplot(dat1)+
   geom_map( map = county_map, aes(map_id = id,fill = gini), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)

map.wa.inc <- ggplot(dat1)+
   geom_map( map = county_map, aes(map_id = id,fill = per.cap.inc), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)

map.wa.pov <- ggplot(dat1)+
   geom_map( map = county_map, aes(map_id = id,fill = pov.rt), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)

map.wa.unemp <- ggplot(dat1) +
  geom_map( map = county_map, aes(map_id = id,fill = unemp.rt), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank())



county<-map_data("county", "washington")
names(county)[5:6] <- c("state", "id")
county$COUNTY<-paste(county$COUNTY, "county")


dat1<-read.csv("nhgis0014_ds201_20135_2013_county.csv", head=TRUE)
dat1<-dat1[dat1$STATE=="Washington",]

dat1$STATE<-tolower(dat1$STATE)
dat1$COUNTY<-tolower(dat1$COUNTY)
names(dat1)[9]<-"id"

#m<-merge(dat1, county, by="COUNTY")

map1 <- ggplot(dat1) +
  geom_map(map = county, aes(map_id = id))
  	, 
            colour = "black") + coord_map() +
  expand_limits(x = m$long, y = m$lat)


#shp<-readOGR("US_county_2013.shp", "US_county_2013")
#shp<-fortify(shp, region="GISJOIN")
names(dat1)[1]<-"id"
plotData<-merge(shp, dat1, by="id")



p<-ggplot()+
	geom_polygon(data=plotData, 
		aes(x=long, y=lat, group=group, fill=NULL),
		color="black", size=0.25)

#data=dat1, 
		#aes(map_id=GISJOIN, fill=UEZE003),


county_map <- map_data("county", "washington")
names(county_map)[5:6] <- c("state", "id")

wadat<-

countyData <- data.frame(id = unique(county_map$id), value = rnorm(39)) 

map1 <- ggplot(countyData) +
  geom_map( map = county_map, aes(map_id = id,fill = value), 
            colour = "black") + coord_map() +
  expand_limits(x = county_map$long, y = county_map$lat)
