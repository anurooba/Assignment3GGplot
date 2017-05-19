thinshallow = read.table("data/resthin.shallow.txt", header=T)
thindeep = read.table("data/resthin.deep.txt", header=T)
clim=read.table("data/sierraclim.txt",header=T)
#make sure you have libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyverse)

sierraclim <- read.delim("~/Anurooba/UCSB/BREN/3 Spring 2017/262/Assignment/Assignment 3/sierraclim.txt", sep = " ",na="")
resthindeep <- read.delim("~/Anurooba/UCSB/BREN/3 Spring 2017/262/Assignment/Assignment 3/resthin.deep.txt", sep = " ",na="")
resthinshallow <- read.delim("~/Anurooba/UCSB/BREN/3 Spring 2017/262/Assignment/Assignment 3/resthin.shallow.txt", sep = " ",na="")


clim <- as_tibble(sierraclim)
clim
View(clim)

#Changing factor vector to data
clim$date <- parse_date(clim$date, "%m/%d/%y")

#Average temperature of the day
clim$AvgTemp<-(clim$tmax+clim$tmin)/2
quartz()
ggplot(data = clim, aes(AvgTemp,rain))+
     geom_line(size=0.75, color="lightblue") +
ggtitle("Average Monthly Temperature and Rainfall \n in Sierra [1941-2013]")+
     xlab("Monthly Average Temperatre")+
     ylab("Rainfall") +
     annotate("text", label="1965 : Highest rainfall year \nAverage Temperature 6.63 Degree", x=15.63, y= 404.06, size=3.2,
              fontface="bold", colour= "lightblue") +
     annotate("segment", x = 6.73, xend = 9.2, y =405.06, yend = 408.08, colour="Orange", size=.5)+
     theme(text=element_text(family="Times", colour = "lightblue", size = 14)) +
     theme_bw()+
     theme(
          panel.background = element_rect(fill = "navy"),
          panel.border      = element_rect(fill = NA, colour = "Orange"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
         # panel.grid.major.y = element_line(linetype = "dotted", colour = "orange"),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size = 20, color = "navy"),
          axis.line.x = element_line(size = 0.5, colour = "navy"), 
          axis.line.y = element_line(size = 0.5, colour = "navy")
     )


#Monthly Temperature


#splitting the data into monthly representation.
# This could also be added via tibble as a factor. for now consider as below
clim$month <- as.character(clim$month)
clim$month[clim$month == "1"] <- "1-January"
clim$month[clim$month == "2"] <- "2-February"
clim$month[clim$month == "3"] <- "3-March"
clim$month[clim$month == "4"] <- "4-April"
clim$month[clim$month == "5"] <- "5-May"
clim$month[clim$month == "6"] <- "6-June"
clim$month[clim$month == "7"] <- "7-July"
clim$month[clim$month == "8"] <- "8-August"
clim$month[clim$month == "9"] <- "9-September"
clim$month[clim$month == "10"] <- "10-October"
clim$month[clim$month == "11"] <- "11-November"
clim$month[clim$month == "12"] <- "12-December"

quartz()
t = ggplot(clim) + geom_line(aes(x=year, y=tmin), stat="summary", fun.y="mean", col="rosybrown2") + geom_line(aes(x=year, y=tmax), stat="summary", fun.y="mean", col="red4")
t= t+facet_wrap(~as.factor(month)) +
          theme(strip.text.x = element_text(size=8, angle=50),
           strip.text.y = element_text(size=12, face="bold"),
           strip.background = element_rect(colour="red", fill="#CCCCFF"))
t = t+theme_bw() +
     theme(
          panel.background = element_rect(fill = "navy"),
          panel.border      = element_rect(fill = NA, colour = "Orange"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title=element_text(size = 20, color = "navy"),
          axis.line.x = element_line(size = 0.5, colour = "navy"), 
          axis.line.y = element_line(size = 0.5, colour = "navy")
     )
t = t + labs(y= expression(paste("Maximum Temperature ",C**degree)), x="Year") 
t = t+  ggtitle("Monthly Temperature in Sierras")
t

####################################


resthindeep <- as_tibble(resthindeep)

resthindeepShared <- filter(resthindeep, resthindeep$shared == "TRUE") 
resthindeepShared
resthindeepIsolated <- filter(resthindeep, resthindeep$shared == "FALSE") 
resthindeepIsolated


p1 = ggplot(resthindeepShared) + 
     geom_bar(aes(x=wy,y=plantc), stat="summary", fun.y="mean", fill = "skyblue2") + 
     geom_line(aes(x=wy, y=trans), stat="summary", fun.y="mean", col = "blue") + 
     geom_line(aes(x=wy, y=evap), stat="summary", fun.y="mean", col = "red") + 
     labs(x="Years since thinning", y="Biomass")+
     ggtitle("Biomass growth for trees with deep roots & \nshared resources") +
     theme_bw() + 
     theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
          ) + 
     annotate("text", x=5, y=1.8, label="Transpiration", colour="blue", size=6, hjust=0.5) +
     annotate("text", x=8, y=0.2, label="Evaporation", colour="red", size=6, hjust=0)



p2 = ggplot(resthindeepShared, aes(x=as.factor(wy), y=plantc, fill=as.factor(thin))) +
     geom_boxplot() +
     labs(x="Years since thinning", y="Biomass") +
     ggtitle("Thinning intensity for trees with deep roots & \n shared resources") +
     theme_bw() + 
     theme(
          axis.text= element_text(face="bold", size=14), 
          plot.margin = unit(c(15,15,15,5),"pt"), 
          axis.title = element_text(size=14)
          )


p3 = ggplot(resthindeepIsolated) + 
     geom_bar(aes(x=wy,y=plantc), stat="summary", fun.y="mean", fill = "skyblue2") + 
     geom_line(aes(x=wy, y=trans), stat="summary", fun.y="mean", col = "blue") + 
     geom_line(aes(x=wy, y=evap), stat="summary", fun.y="mean", col = "red") + 
     labs(x="Years since thinning", y="Biomass")+
     ggtitle("Biomass growth for trees with deep roots & \nisolated resources") +
     theme_bw() + 
     theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     ) + 
     annotate("text", x=5, y=1.8, label="Transpiration", colour="blue", size=6, hjust=0.5) +
     annotate("text", x=8, y=0.2, label="Evaporation", colour="red", size=6, hjust=0)


p4 = ggplot(resthindeepIsolated, aes(x=as.factor(wy), y=plantc, fill=as.factor(thin))) +
     geom_boxplot() +
     labs(x="Years since thinning", y="Biomass") +
     ggtitle("Thinning intensity for trees with deep roots & \nisolated resources") +
     theme_bw() + 
     theme(
          axis.text= element_text(face="bold", size=14), 
          plot.margin = unit(c(15,15,15,5),"pt"), 
          axis.title = element_text(size=14)
     )

quartz()
grid.arrange(p1,p2,p3,p4)


#########################
resthindeep <- as.tibble(resthindeep)
resthinshallow <- as.tibble(resthinshallow)

resthindeep_wettestmonth <- filter(resthindeep, resthindeep$month == 6)
resthindeep_wettestmonth
resthinshallow_wettestmonth <- filter(resthinshallow, resthinshallow$month == 6)
resthinshallow_wettestmonth


# Basic barplot:

w1= ggplot(resthindeep_wettestmonth, aes(x=day, y=evap)) +
     geom_line(aes(x=day, y=evap), stat="summary", fun.y="mean", col="blue") +
     scale_x_continuous(limits = c(1,30), expand = c(0,0),breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
     theme_bw() + 
     theme(plot.title = element_text(size=10)) +
     theme(
          panel.background = element_rect(fill = "mistyrose2"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     ) +
     labs(x="June Days (1950-1980)", y="Evaporation") +
     ggtitle("Deep Roots : Daily Mean Evaporation\nfor June(wettest month)")

w2= ggplot(resthindeep_wettestmonth)+
     geom_line(aes(x=day, y=trans), stat="summary", fun.y="mean", col="blue") +
     labs(x="June Days (1950-1980)", y="Transpiration") +
     scale_x_continuous(limits = c(1,30), expand = c(0,0),breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
     ggtitle("Deep Roots : Daily Mean Transpiration\nfor June(wettest month)")+
     theme_bw() + 
     theme(plot.title = element_text(size=10)) +
     theme(
          panel.background = element_rect(fill = "mistyrose2"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     ) 


w3= ggplot(resthindeep_wettestmonth)+
     geom_line(aes(x=day, y=plantc), stat="summary", fun.y="mean", col="blue") +
     labs(x="June Days (1950-1980)", y="Biomass") +
     scale_x_continuous(limits = c(1,30), expand = c(0,0),breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
     ggtitle("Deep Roots : Mean Biomass\nfor June(wettest month)")+
     theme_bw() + 
     theme(
          panel.background = element_rect(fill = "mistyrose2"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     ) 
     
w4= ggplot(resthinshallow_wettestmonth)+
     geom_line(aes(x=day, y=evap), stat="summary", fun.y="mean", col="blue") +
     scale_x_continuous(limits = c(1,30), expand = c(0,0),breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
     theme_bw() + 
     theme(plot.title = element_text(size=10)) +
     theme(
          panel.background = element_rect(fill = "thistle2"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     )

w5= ggplot(resthinshallow_wettestmonth)+
     geom_line(aes(x=day, y=trans), stat="summary", fun.y="mean", col="blue") +
     labs(x="June Days (1950-1980)", y="Transpiration") +
     scale_x_continuous(limits = c(1,30), expand = c(0,0),breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
     ggtitle("Shallow Roots : Daily Mean Transpiration\nfor June(wettest month)")+
     theme_bw() + 
     theme(plot.title = element_text(size=10)) +
     theme(
          panel.background = element_rect(fill = "thistle2"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     ) 


w6= ggplot(resthinshallow_wettestmonth)+
     geom_line(aes(x=day, y=plantc), stat="summary", fun.y="mean", col="blue") +
     labs(x="June Days (1950-1980)", y="Biomass") +
     scale_x_continuous(limits = c(1,30), expand = c(0,0),breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
     ggtitle("Shallow Roots : Mean Biomass\nfor June(wettest month)")+
     theme_bw() + 
     theme(plot.title = element_text(size=10)) +
     theme(
          panel.background = element_rect(fill = "thistle2"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
     ) 

quartz()
grid.arrange(w1,w2,w3,w4,w5,w6, ncol = 3)