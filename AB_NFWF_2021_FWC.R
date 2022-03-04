#This reads in the 2021 Apalach data from jim estes from two different tabs
#one is the shell heights and the other are the counts
#need to calculate the proportion of the different size categories and apply those proportions to the counts


library(readxl)
library(tidyverse)
library(dplyr)
library(reshape)
library(ggplot2)
library(lubridate)

SH <- read_excel("AB_Survey_2021.xlsx",sheet = "SH", range = "A4:G4380")

Counts <- read_excel("AB_Survey_2021.xlsx",sheet = "SH", range = "A4:K689")

#now start cleaning

names(SH)

s2 <- SH
str(s2)

names(s2)


names(s2)[5] <- "StationName"
names(s2)[6] <- "Quadrat"
names(s2)[7] <- "SH"

s2$SH[s2$SH == "Z"] <-NA

#now make SH a number
s2$SH <- as.numeric(s2$SH)


###
#add period

s3 <- s2 %>%
  mutate(Year = year(s2$Date),
         Month = month(s2$Date),
         Day = day(s2$Date))

s3$Period <- NA
firstyear <- 2015
endyear <- max(s3$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(s3)){
    if(s3$Year[j] == y & s3$Month[j] > 3 & s3$Month[j] < 10) s3$Period[j] = p[1] #year i months 4-9
    if(s3$Year[j] == y & s3$Month[j] > 9) s3$Period[j] = p[2] #year i months 10-12
    if(s3$Year[j] == y+1 & s3$Month[j] < 4) s3$Period[j] = p[2] #year i+1 months 1-3
  }
}

s3$Season <- "Winter"
s3$Season[s3$Period == 1 | s3$Period == 3 | s3$Period == 5 | s3$Period == 7 | s3$Period == 9] <- "Summer"

####

names(s3)

unique(s3$Period)

num_spat<-length(s3$SH[s3$SH<26])
num_total<-length(s3$SH)

proportion_spat<-num_spat/num_total

########
####SPAT######
p11 <- subset(s3, s3$Period == 11)
num_spat_p11<-length(p11$SH[p11$SH<26])
num_total_p11<-length(p11$SH)
proportion_spat_p11<-num_spat_p11/num_total_p11
#0.94 spat

p12 <- subset(s3, s3$Period == 12)
num_spat_p12<-length(p12$SH[p12$SH<26])
num_total_p12<-length(p12$SH)
proportion_spat_p12<-num_spat_p12/num_total_p12
#0.74 spat

p13 <- subset(s3, s3$Period == 13)
num_spat_p13<-length(p13$SH[p13$SH<26])
num_total_p13<-length(p13$SH)
proportion_spat_p13<-num_spat_p13/num_total_p13
#0.81 spat

hist(s3$SH)

###SEED
#seed are done using dplyr, so different than seed or legal 

num_seed_p11<-length(subset(p11$SH, p11$SH>=26 & p11$SH<76))
num_total_p11<-length(p11$SH)
proportion_seed_p11<-num_seed_p11/num_total_p11
#0.05 seed

num_seed_p12<-length(subset(p12$SH, p12$SH>=26 & p12$SH<76))
num_total_p12<-length(p12$SH)
proportion_seed_p12<-num_seed_p12/num_total_p12
#0.24 seed

num_seed_p13<-length(subset(p13$SH, p13$SH>=26 & p13$SH<76))
num_total_p13<-length(p13$SH)
proportion_seed_p13<-num_seed_p13/num_total_p13
#0.15 seed

####Legal######
num_Legal_p11<-length(subset(p11$SH, p11$SH>=76))
num_total_p11<-length(p11$SH)
proportion_Legal_p11<-num_Legal_p11/num_total_p11
#0 Legal

num_Legal_p12<-length(subset(p12$SH, p12$SH>=76))
num_total_p12<-length(p12$SH)
proportion_Legal_p12<-num_Legal_p12/num_total_p12
#0.02 Legal

num_Legal_p13<-length(subset(p13$SH, p13$SH>=76))
num_total_p13<-length(p13$SH)
proportion_Legal_p13<-num_Legal_p13/num_total_p13
#0.04 Legal

##next you need to take these proportions and apply them to the total counts in each period
#this is how you did it in the FWC "random" for random effects file


# d4$Legalprop <-0.99
# d4$Legalprop[d4$Period ==2] <-0.0002
# d4$Legalprop[d4$Period ==3] <-0.0002
# d4$Legalprop[d4$Period ==4] <-0.0002
# d4$Legalprop[d4$Period ==5] <-0.03
# d4$Legalprop[d4$Period ==6] <-0.0002
# d4$Legalprop[d4$Period ==7] <-0.0007
# d4$Legalprop[d4$Period ==8] <-0
# d4$Legalprop[d4$Period ==9] <-0
# 
# #now multiply these proportions * the TotalOysters
# #round it so there are no fractions of oysters
# #and convert to integer
# d4$TotalSpat <-as.integer(round((d4$TotalOysters * d4$Spatprop),0))
# d4$TotalSeed <-as.integer(round((d4$TotalOysters * d4$Seedprop),0))
# d4$TotalLegal <-as.integer(round((d4$TotalOysters * d4$Legalprop),0))
# 
# 
# write.table(d4, file = "~/Git/AB_DEP/FWC_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")


