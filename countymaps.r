##### motivates selection - provide context - political data? voteshare -

rm(list=ls())
library(ggplot2)
library(RColorBrewer)
library(maps)
library(rgdal)
library(dplyr)
library(mapproj)
library(xtable)

#setwd("~/state-descriptives")
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

rq<-function(x,l){
  require(gglpot2)
  l<-9 ### number of quantiles
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
    }
    return(as.factor(quant))
}

cleanstate<-function(x){
  x$STATE<-ifelse((x$State=="AL"),"Alabama", x$STATE)
  x$STATE<-ifelse((x$State=="AK"),"Alaska", x$STATE)
  x$STATE<-ifelse((x$State=="AZ"),"Arizona", x$STATE)
  x$STATE<-ifelse((x$State=="AR"),"Arkansas", x$STATE)
  x$STATE<-ifelse((x$State=="CA"),"California", x$STATE)
  x$STATE<-ifelse((x$State=="CO"),"Colorado", x$STATE)
  x$STATE<-ifelse((x$State=="CT"),"Connecticut", x$STATE)
  x$STATE<-ifelse((x$State=="DE"),"Delaware", x$STATE)
  x$STATE<-ifelse((x$State=="DC"),"District of Columbia", x$STATE)
  x$STATE<-ifelse((x$State=="FL"),"Florida", x$STATE)
  x$STATE<-ifelse((x$State=="GA"),"Georgia", x$STATE)
  x$STATE<-ifelse((x$State=="HI"),"Hawaii", x$STATE)
  x$STATE<-ifelse((x$State=="ID"),"Idaho", x$STATE)
  x$STATE<-ifelse((x$State=="IL"),"Illinois", x$STATE)
  x$STATE<-ifelse((x$State=="IN"),"Indiana", x$STATE)
  x$STATE<-ifelse((x$State=="IA"),"Iowa", x$STATE)
  x$STATE<-ifelse((x$State=="KS"),"Kansas", x$STATE)
  x$STATE<-ifelse((x$State=="KY"),"Kentucky", x$STATE)
  x$STATE<-ifelse((x$State=="LA"),"Louisiana", x$STATE)
  x$STATE<-ifelse((x$State=="ME"),"Maine", x$STATE)
  x$STATE<-ifelse((x$State=="MD"),"Maryland", x$STATE)
  x$STATE<-ifelse((x$State=="MA"),"Massachusetts", x$STATE)
  x$STATE<-ifelse((x$State=="MI"),"Michigan", x$STATE)
  x$STATE<-ifelse((x$State=="MN"),"Minnesota", x$STATE)
  x$STATE<-ifelse((x$State=="MS"),"Mississippi", x$STATE)
  x$STATE<-ifelse((x$State=="MO"),"Missouri", x$STATE)
  x$STATE<-ifelse((x$State=="MT"),"Montana", x$STATE)
  x$STATE<-ifelse((x$State=="NE"),"Nebraska", x$STATE)
  x$STATE<-ifelse((x$State=="NV"),"Nevada", x$STATE)
  x$STATE<-ifelse((x$State=="NH"),"New Hampshire", x$STATE)
  x$STATE<-ifelse((x$State=="NJ"),"New Jersey", x$STATE)
  x$STATE<-ifelse((x$State=="NM"),"New Mexico", x$STATE)
  x$STATE<-ifelse((x$State=="NY"),"New York", x$STATE)
  x$STATE<-ifelse((x$State=="NC"),"North Carolina", x$STATE)
  x$STATE<-ifelse((x$State=="ND"),"North Dakota", x$STATE)
  x$STATE<-ifelse((x$State=="OH"),"Ohio", x$STATE)
  x$STATE<-ifelse((x$State=="OK"),"Oklahoma", x$STATE)
  x$STATE<-ifelse((x$State=="OR"),"Oregon", x$STATE)
  x$STATE<-ifelse((x$State=="PA"),"Pennsylvania", x$STATE)
  x$STATE<-ifelse((x$State=="RI"),"Rhode Island", x$STATE)
  x$STATE<-ifelse((x$State=="SC"),"South Carolina", x$STATE)
  x$STATE<-ifelse((x$State=="SD"),"South Dakota", x$STATE)
  x$STATE<-ifelse((x$State=="TN"),"Tennessee", x$STATE)
  x$STATE<-ifelse((x$State=="TX"),"Texas", x$STATE)
  x$STATE<-ifelse((x$State=="UT"),"Utah", x$STATE)
  x$STATE<-ifelse((x$State=="VT"),"Vermont", x$STATE)
  x$STATE<-ifelse((x$State=="VA"),"Virginia", x$STATE)
  x$STATE<-ifelse((x$State=="WA"),"Washington", x$STATE)
  x$STATE<-ifelse((x$State=="WV"),"West Virginia", x$STATE)
  x$STATE<-ifelse((x$State=="WI"),"Wisconsin", x$STATE)
  x$STATE<-ifelse((x$State=="WY"),"Wyoming", x$STATE)
  x$STATE<-ifelse((x$State=="PR"), "Puerto Rico", x$STATE)
  x$STATE<-tolower(x$STATE)
  return(x)
}

### pop data from NHGIS
dat1<-read.csv("nhgis0014_ds201_20135_2013_county.csv", head=TRUE)
### 2012 election data from http://www.theguardian.com/news/datablog/2012/nov/07/us-2012-election-county-results-download
dat2<-read.csv("pres2012.csv", head=TRUE)

dat1$STATE<-tolower(dat1$STATE)
dat1$id<-tolower(dat1$COUNTY)
dat1$id<-as.character(strsplit(dat1$id, " county"))
dat1$id<-gsub("[.]", "", dat1$id)
dat1$COUNTY<-as.character(strsplit(as.character(dat1$COUNTY), " County"))


dat2$STATE<-NA
dat2$State<-dat2$State.Postal
dat2<-cleanstate(dat2)
dat2$County<-dat2$County.Name
dat2$id<-tolower(dat2$County)
dat2$id<-as.character(strsplit(dat2$id, " county"))
dat2$id<-gsub("[.]", "", dat2$id)
dat2<-dat2[-(which(dat2$FIPS==0)),]

dat2$RepVotes<-NA
dat2$RepVotes<-(with(dat2, ifelse(Party=="GOP", Votes,
  ifelse(Party.1=="GOP", Votes.1, 
    ifelse(Party.2=="GOP", Votes.2,
    ifelse(Party.3=="GOP", Votes.3, 
    ifelse(Party.4=="GOP", Votes.4, RepVotes
    )))))))
dat2$pct.rep2012<-dat2$RepVotes/dat2$TOTAL.VOTES.CAST

dat1<-merge(dat1, dat2, by=c("STATE", "id"))

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

dat1$pctwht<-with(dat1,
  dat1$white.pop/dat1$tot.pop)

states<-c("washington", "california", "texas", "illinois", 
  "minnesota", "missouri", "georgia", "new york")
s.dat<-list()
for(s in 1:length(states)){
  s.dat[[s]]<-dat1[dat1$STATE==states[s],]
 }

MapPlot<-list()

for(i in 1:length(states)){

  n<-nrow(s.dat[[i]])
  map.dat<-with(s.dat[[i]], 
    data.frame(
      region=rep(STATE, 6),
      subregion=rep(id, 6),
      q=as.factor(c(returnquant(tot.pop), returnquant(pct.rep2012), returnquant(pctblk), 
        returnquant(pctlat), returnquant(pov.rt), returnquant(gini))),
      c=c(rep("Total Population",n), rep("% R Pres Vote 2012",n), rep("% Black Pop",n), 
        rep("% Latino Pop",n), rep("Poverty Rate",n), rep("Gini",n))
    ))

  map.dat$c<-factor(map.dat$c, levels=c("Total Population", "% R Pres Vote 2012","% Black Pop",
   "% Latino Pop", "Poverty Rate", "Gini"))

  county_map <- map_data("county", states[i])
    county_map$subregion<-ifelse(county_map$subregion=="de kalb", "dekalb", county_map$subregion)
    county_map$subregion<-ifelse(county_map$subregion=="du page", "dupage", county_map$subregion)
    county_map$subregion<-ifelse(county_map$subregion=="la salle", "lasalle", county_map$subregion)
  choro<-merge(county_map, map.dat, by="subregion")
  choro <- choro[order(choro$order), ]

  MapPlot[[i]] <- ggplot(choro,
   aes(x = long, y = lat, group = group, fill = q)) +
   geom_polygon(aes(fill = q), colour = "black") +
    scale_fill_brewer(palette = "Blues", labels=c("Lowest 10%","", "", "", "", "", "", " ", "Highest 10%"),
      name=" ") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL) +
  scale_x_continuous(name="", breaks=NULL) +
  theme(strip.background=element_blank(), 
    strip.text.x=element_text(size=10),
    strip.text.y=element_blank())+
  xlab(NULL) + ylab(NULL)+
  facet_wrap(~c, ncol=2)

}

ggsave(plot=MapPlot[[1]], "WAmap.pdf", h=8, w=9)
ggsave(plot=MapPlot[[2]], "CAmap.pdf", h=8, w=6)
ggsave(plot=MapPlot[[3]], "TXmap.pdf", h=8, w=7)
ggsave(plot=MapPlot[[4]], "ILmap.pdf", h=8, w=4.5)
ggsave(plot=MapPlot[[5]], "MNmap.pdf", h=8, w=5.7)
ggsave(plot=MapPlot[[6]], "MOmap.pdf", h=8, w=7)
ggsave(plot=MapPlot[[7]], "GAmap.pdf", h=8, w=6.5)
ggsave(plot=MapPlot[[8]], "NYmap.pdf", h=8, w=8)


# ### Make state tables 
# captions<-c("Washington Counties", "California Counties",
#   "Texas Counties", "Illinois Counties", "Minnesota Counties",
#   "Missouri Counties", "Georgia Counties", "New York Counties")

# s.tab<-list()
# for(i in 1:length(states)){
#   s.dat[[i]]<-s.dat[[i]]%>%
#   select(., STATE, COUNTY, tot.pop, gini, per.cap.inc, pct.rep2012, pov.rt, unemp.rt, pctblk, pctlat)
#   s.dat[[i]]$pct.rep2012<-s.dat[[i]]$pct.rep2012*100
#   s.dat[[i]]$pov.rt<-s.dat[[i]]$pov.rt*100
#   s.dat[[i]]$unemp.rt<-s.dat[[i]]$unemp.rt*100
#   s.dat[[i]]$pctblk<-s.dat[[i]]$pctblk*100
#   s.dat[[i]]$pctlat<-s.dat[[i]]$pctlat*100
#   names(s.dat[[i]])<-c("State","County", "Total Pop", "Gini", 
#     "Per Cap Income", "R Pres Vote '12", "Pov Rt", 
#     "Unemp Rt", "Pct Black Pop", "Pct Latino Pop")
#   s.tab[[i]]<-xtable(s.dat[[i]][,2:ncol(s.dat[[i]])], 
#     caption=captions[i], caption.placement="top")
# }
# s.out<-rbind(s.dat[[1]], s.dat[[2]], s.dat[[3]], s.dat[[4]], 
#   s.dat[[5]], s.dat[[6]], s.dat[[7]], s.dat[[8]])
# write.csv(s.out, "county.descriptives.csv")
# print.xtable(s.tab[[1]], file="WA-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[2]], file="CA-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[3]], file="TX-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[4]], file="IL-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[5]], file="MN-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[6]], file="MO-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[7]], file="GA-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)
# print.xtable(s.tab[[8]], file="NY-tab.tex", 
#   include.rownames=FALSE, tabular.environment='longtable',
#   floating=FALSE)

