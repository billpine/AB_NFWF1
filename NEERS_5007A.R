#NEERS 5007 A from https://dev.seacar.waterinstitute.usf.edu/programs/details/5007


library(readxl)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(MASS)
library(sjPlot)
library(reshape)
library(ggplot2)
library(lubridate)
library(AICcmodavg)
library(ggeffects)
library(cowplot)


d1 <- read.csv("~/GitHub/AB_NFWF1/Data_5007A_Final.csv")

names(d1)
head(d1)

#subset the columns to the ones you want to work with
d2 <- d1 %>% 
  dplyr::select(Harvested, Site, Quadrat, Weight_kg, Total_Adults_75mm, 
         Total_Seed_26_74mm, Total_Spat_0_25mm, Total_Live)
  
#rename headers  

names(d2)[]<-c("Date", "Site", "Quadrat", "Weight", "Legal", "Sublegal", "Spat", "Total_Live")
    

head(d2)



#just extra year, month, day from the single date column

d3 <- d2 %>%
  mutate(Year = year(d2$Date),
         Month = month(d2$Date),
         Day = day(d2$Date))


##

d3$Period <- NA
firstyear <- 2015
endyear <- max(d3$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(d3)){
    if(d3$Year[j] == y & d3$Month[j] > 3 & d3$Month[j] < 10) d3$Period[j] = p[1] #year i months 4-9
    if(d3$Year[j] == y & d3$Month[j] > 9) d3$Period[j] = p[2] #year i months 10-12
    if(d3$Year[j] == y+1 & d3$Month[j] < 4) d3$Period[j] = p[2] #year i+1 months 1-3
  }
}

d3$season <- "Winter"
d3$season[d3$Period == 1 | d3$Period == 3 | d3$Period == 5 | d3$Period == 7 | d3$Period == 9] <- "Summer"

d3$season




