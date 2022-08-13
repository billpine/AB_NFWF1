#These are the FWC NFWF1 data provided by Ryan Gandy 
#Jan 12, 2021

#TO DO

#this is my second attempt at these data, this
#time modifying how I'm summing the counts
#based on email exchanges with Matt Davis
#and trying to get random effects as the site.

#read data from FWC Excel file, only need sheet 3

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

#################################################

#####################

d1 <- read_excel("NFWF_RAW_UF_copy.xlsx", sheet=3)

names(d1)

max(d1$LiveSpat)
min(d1$LiveSpat)

#remove -999 in live spat rows
#d2 <- d1[d1$LiveSpat > -1,]
#d2 <- d2[d2$TotalWt > -1,]

#switch -999 to NA instead of removing
d2 <- d1
d2$LiveSpat[d2$LiveSpat < -1] <- NA
d2$TotalWt[d2$TotalWt < -1] <- NA
d2$Drills[d2$Drills < -1] <- NA
d2$LiveOysters[d2$LiveOysters < -1] <- NA

#from Matt Davis answer to
#bp question
#To calculate the number of spat I 
#should then sum the LiveSpat and Live Oysters column 
#and then use the proportion of oysters < 25-mm from your 
#size information to convert the 
#sum of (LiveSpat + LiveOysters) to an estimated number of 
#spat?
#Matt's answer That’s correct.

d2$TotalOysters <-(d2$LiveOysters + d2$LiveSpat)

str(d2)

names(d2)

#subset the columns to the ones you want to work with
d3 <- d2 %>% 
  dplyr::select(Survey, Date, StationName, StationNumber, Cultch, 
                Quadrat, TotalVol, TotalWt, LiveSpat, TotalOysters, Drills)

#just year, month, day from the single date column

d3 <- d3 %>%
  mutate(Year = year(d3$Date),
         Month = month(d3$Date),
         Day = day(d3$Date))



#make summary table

month <- d3 %>%
  dplyr::group_by(Year, Month, StationName) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Year, Month, StationName)
names(month) <- c("Year", "Month", "Station Name",
                  "Number Quadrats")


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer
#######################
#So April through September is summer and October through March is winter


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

d3$Season <- "Winter"
d3$Season[d3$Period == 1 | d3$Period == 3 | d3$Period == 5 | d3$Period == 7 | d3$Period == 9| d3$Period == 11| d3$Period == 13] <- "Summer"

d3$Season


period <- d3 %>%
  dplyr::group_by(Period,Year, Month, StationName) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Period, Year, Month, StationName)
names(period) <- c("Period", "Year","Month", "Station Name",
                   "Number Quadrats")

#just writing the table with number of quadrats by year, month, station to folder
#write.table(period, file = "period_yr_mnth_station.txt", row.names = FALSE,
#            col.names = TRUE,sep = ",")

##We will mostly work with period as the unit of time
##This combines some of the months obviously

##
#now add spat prop from proportion calculator script


d4<-d3
d4$Spatprop <-0.99
d4$Spatprop[d4$Period ==2] <-0.79
d4$Spatprop[d4$Period ==3] <-0.37
d4$Spatprop[d4$Period ==4] <-0.88
d4$Spatprop[d4$Period ==5] <-0.81
d4$Spatprop[d4$Period ==6] <-0.995
d4$Spatprop[d4$Period ==7] <-0.99
d4$Spatprop[d4$Period ==8] <-0.999
d4$Spatprop[d4$Period ==9] <-0.999

d4$Seedprop <-0.99
d4$Seedprop[d4$Period ==2] <-0.21
d4$Seedprop[d4$Period ==3] <-0.60
d4$Seedprop[d4$Period ==4] <-0.09
d4$Seedprop[d4$Period ==5] <-0.16
d4$Seedprop[d4$Period ==6] <-0.005
d4$Seedprop[d4$Period ==7] <-0.01
d4$Seedprop[d4$Period ==8] <-0.00006
d4$Seedprop[d4$Period ==9] <-0.01

d4$Legalprop <-0.99
d4$Legalprop[d4$Period ==2] <-0.0002
d4$Legalprop[d4$Period ==3] <-0.0002
d4$Legalprop[d4$Period ==4] <-0.0002
d4$Legalprop[d4$Period ==5] <-0.03
d4$Legalprop[d4$Period ==6] <-0.0002
d4$Legalprop[d4$Period ==7] <-0.0007
d4$Legalprop[d4$Period ==8] <-0
d4$Legalprop[d4$Period ==9] <-0

#now multiply these proportions * the TotalOysters
#round it so there are no fractions of oysters
#and convert to integer
d4$TotalSpat <-as.integer(round((d4$TotalOysters * d4$Spatprop),0))
d4$TotalSeed <-as.integer(round((d4$TotalOysters * d4$Seedprop),0))
d4$TotalLegal <-as.integer(round((d4$TotalOysters * d4$Legalprop),0))


write.table(d4, file = "~/Git/AB_DEP/20220812_FWC_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")

