#This reads in the 2022 Apalach data from matt davis from two different tabs
#one is the shell heights and the other are the counts
#need to calculate the proportion of the different size categories and apply those proportions to the counts


library(readxl)
library(tidyverse)
library(dplyr)
library(reshape)
library(ggplot2)
library(lubridate)

SH <- read_excel("AB_ShellBudget_FSU.xlsx", sheet = "SHs", col_types = c("numeric", 
"date", "text", "text", "numeric","text", "numeric"))


#SH <- read_excel("AB_ShellBudget_FSU.xlsx",sheet = "SHs", range = "A1370:J1669")

Counts <-  read_excel("AB_ShellBudget_FSU.xlsx", sheet = "Counts")

#now start cleaning SH first

names(SH)

s2 <- SH
str(s2)

names(s2)


names(s2)[3] <- "StationName"


s2$SH[s2$SH == ""] <-NA

#now make SH a number
s2$SH <- as.numeric(s2$SH)

#work on date

s3 <- s2 %>%
  mutate(Year = year(s2$Date),
         Month = month(s2$Date),
         Day = day(s2$Date))
        
s3<- subset(s3, s3$Year == "2022")

str(s3)


###
#add period

s3$Period <- NA
firstyear <- 2015
endyear <- max(s3$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2014 = 3 and 4, and so forth.
  for(j in 1:nrow(s3)){
    if(s3$Year[j] == y & s3$Month[j] > 3 & s3$Month[j] < 10) s3$Period[j] = p[1] #year i months 4-9
    if(s3$Year[j] == y & s3$Month[j] > 9) s3$Period[j] = p[2] #year i months 10-12
    if(s3$Year[j] == y+1 & s3$Month[j] < 4) s3$Period[j] = p[2] #year i+1 months 1-3
  }
}

s3$Season <- "Winter"
s3$Season[s3$Period == 1 | s3$Period == 3 | s3$Period == 5 | s3$Period == 7 | s3$Period == 9| s3$Period == 14| s3$Period == 13| s3$Period == 15] <- "Summer"

####

names(s3)

unique(s3$Period)

table(s3$Period,s3$Month)
table(s3$Period,s3$Season)

num_spat<-length(s3$SH[s3$SH<26])
num_total<-length(s3$SH)

proportion_spat<-num_spat/num_total

########
####SPAT######
p14<- subset(s3, s3$Period == 14)
num_spat_p14<-length(p14$SH[p14$SH<26])
num_total_p14<-length(p14$SH)
proportion_spat_p14<-num_spat_p14/num_total_p14
#0.78 spat

p15 <- subset(s3, s3$Period == 15)
num_spat_p15<-length(p15$SH[p15$SH<26])
num_total_p15<-length(p15$SH)
proportion_spat_p15<-num_spat_p15/num_total_p15
#0.47 spat



hist(s3$SH)

###SEED
#seed are done using dplyr, so different than seed or legal 

num_seed_p14<-length(subset(p14$SH, p14$SH>=26 & p14$SH<76))
num_total_p14<-length(p14$SH)
proportion_seed_p14<-num_seed_p14/num_total_p14
#0.22 seed

num_seed_p15<-length(subset(p15$SH, p15$SH>=26 & p15$SH<76))
num_total_p15<-length(p15$SH)
proportion_seed_p15<-num_seed_p15/num_total_p15
#0.52 seed

####Legal######
num_Legal_p14<-length(subset(p14$SH, p14$SH>=76))
num_total_p14<-length(p14$SH)
proportion_legal_p14<-num_Legal_p14/num_total_p14
#0.002 Legal

num_Legal_p15<-length(subset(p15$SH, p15$SH>=76))
num_total_p15<-length(p15$SH)
proportion_legal_p15<-num_Legal_p15/num_total_p15
#0.009 Legal


###now clean counts

c1<-Counts

names(c1)
names(c1)[5] <- "StationName"
names(c1)[7] <- "Quadrat_live"
names(c1)[8] <- "Quadrat_dead"
names(c1)[9] <- "Volume_L"
names(c1)[10] <- "Weight_kg"
names(c1)[14] <- "Number_drills"

str(c1)

unique(c1$Quadrat)


###
#add period

c2 <-c1 %>%
  mutate(Year = year(c2$Date),
         Month = month(c2$Date),
         Day = day(c2$Date))

c2$Period <- NA
firstyear <- 2015
endyear <- max(c2$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2014 = 3 and 4, and so forth.
  for(j in 1:nrow(c2)){
    if(c2$Year[j] == y & c2$Month[j] > 3 & c2$Month[j] < 10) c2$Period[j] = p[1] #year i months 4-9
    if(c2$Year[j] == y & c2$Month[j] > 9) c2$Period[j] = p[2] #year i months 10-12
    if(c2$Year[j] == y+1 & c2$Month[j] < 4) c2$Period[j] = p[2] #year i+1 months 1-3
  }
}

c2$Season <- "Winter"
c2$Season[c2$Period == 1 | c2$Period == 3 | c2$Period == 5 | c2$Period == 7 | c2$Period == 9| c2$Period == 14| c2$Period == 13] <- "Summer"

####

names(s3)
names(c2)


#subset the columns to the ones you want to work with
c3 <- c2 %>% 
  dplyr::select("Survey","Site","Station","StationName","Quadrat"
                ,"Quadrat_live","Quadrat_dead","Weight_kg",
                "Number_drills","Month","Day","Year","Period","Season")

unique(c3$Period)


c3$Spatprop <-0.99
c3$Spatprop[c3$Period ==14] <-proportion_spat_p14
c3$Spatprop[c3$Period ==12] <-proportion_spat_p12
c3$Spatprop[c3$Period ==13] <-proportion_spat_p13

c3$Seedprop <-0.99
c3$Seedprop[c3$Period ==14] <-proportion_seed_p14
c3$Seedprop[c3$Period ==12] <-proportion_seed_p12
c3$Seedprop[c3$Period ==13] <-proportion_seed_p13

c3$Legalprop <-0.99
c3$Legalprop[c3$Period ==14] <-proportion_legal_p14
c3$Legalprop[c3$Period ==12] <-proportion_legal_p12
c3$Legalprop[c3$Period ==13] <-proportion_legal_p13

##next you need to take these proportions and apply them to the total counts in each period
#this is how you did it in the FWC "random" for random effects file


# 
# #now multiply these proportions * the Quadrat_live
# #round it so there are no fractions of oysters
# #and convert to integer
c3$TotalSpat <-as.integer(round((c3$Quadrat_live * c3$Spatprop),0))
c3$TotalSeed <-as.integer(round((c3$Quadrat_live * c3$Seedprop),0))
c3$TotalLegal <-as.integer(round((c3$Quadrat_live * c3$Legalprop),0))

# 

#subset the columns to the ones you want to work with
c4 <- c3 %>% 
  dplyr::select("Survey","Site","Station","StationName","Quadrat"
                ,"TotalSpat","TotalSeed","TotalLegal","Weight_kg",
                "Number_drills","Month","Day","Year","Period","Season")



write.table(c4, file = "~/Git/AB_DEP/FWC_2021_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")


