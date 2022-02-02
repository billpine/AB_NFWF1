
library(readxl)
library(tidyverse)
library(dplyr)



h1 <- read_excel("NFWF_RAW_UF_copy.xlsx", sheet=4)
names(h1)

min(h1$SH)
max(h1$SH)


#switch -999 to NA instead of removing
h2 <- h1

h2$SH[h2$SH < -1] <- NA

hist(h2$SH)


#add period

h3 <- h2 %>%
  mutate(Year = year(h2$Date),
         Month = month(h2$Date),
         Day = day(h2$Date))

h3$Period <- NA
firstyear <- 2015
endyear <- max(h3$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(h3)){
    if(h3$Year[j] == y & h3$Month[j] > 3 & h3$Month[j] < 10) h3$Period[j] = p[1] #year i months 4-9
    if(h3$Year[j] == y & h3$Month[j] > 9) h3$Period[j] = p[2] #year i months 10-12
    if(h3$Year[j] == y+1 & h3$Month[j] < 4) h3$Period[j] = p[2] #year i+1 months 1-3
  }
}

h3$Season <- "Winter"
h3$Season[h3$Period == 1 | h3$Period == 3 | h3$Period == 5 | h3$Period == 7 | h3$Period == 9] <- "Summer"

####

unique(h3$Period)

num_spat<-length(h2$SH[h2$SH<26])
num_total<-length(h2$SH)

proportion_spat<-num_spat/num_total

p2 <- subset(h3, h3$Period == 2)
num_spat_p2<-length(p2$SH[p2$SH<26])
num_total_p2<-length(p2$SH)
proportion_spat_p2<-num_spat_p2/num_total_p2
#0.79 spat

p3 <- subset(h3, h3$Period == 3)
num_spat_p3<-length(p3$SH[p3$SH<26])
num_total_p3<-length(p3$SH)
proportion_spat_p3<-num_spat_p3/num_total_p3
#0.37 spat

p4 <- subset(h3, h3$Period == 4)
num_spat_p4<-length(p4$SH[p4$SH<26])
num_total_p4<-length(p4$SH)
proportion_spat_p4<-num_spat_p4/num_total_p4
#0.88 spat

p5 <- subset(h3, h3$Period == 5)
num_spat_p5<-length(p5$SH[p5$SH<26])
num_total_p5<-length(p5$SH)
proportion_spat_p5<-num_spat_p5/num_total_p5
#0.81 spat

p6 <- subset(h3, h3$Period == 6)
num_spat_p6<-length(p6$SH[p6$SH<26])
num_total_p6<-length(p6$SH)
proportion_spat_p6<-num_spat_p6/num_total_p6
#0.995 spat

p7 <- subset(h3, h3$Period == 7)
num_spat_p7<-length(p7$SH[p7$SH<26])
num_total_p7<-length(p7$SH)
proportion_spat_p7<-num_spat_p7/num_total_p7
#0.99 spat

p8 <- subset(h3, h3$Period == 8)
num_spat_p8<-length(p8$SH[p8$SH<26])
num_total_p8<-length(p8$SH)
proportion_spat_p8<-num_spat_p8/num_total_p8
#0.999 spat

p9 <- subset(h3, h3$Period == 9)
num_spat_p9<-length(p9$SH[p9$SH<26])
num_total_p9<-length(p9$SH)
proportion_spat_p9<-num_spat_p9/num_total_p9
#0.999 spat

hist(d2$SH)



