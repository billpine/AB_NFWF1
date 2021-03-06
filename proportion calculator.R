
#this calculates the proportions of the size classes 
#from te FWC data

library(readxl)
library(tidyverse)
library(dplyr)
library(reshape)
library(ggplot2)
library(lubridate)





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

####SPAT######
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

###SEED

num_seed_p2<-length(subset(p2$SH, p2$SH>=26 & p2$SH<76))
num_total_p2<-length(p2$SH)
proportion_seed_p2<-num_seed_p2/num_total_p2
#0.23 seed


num_seed_p3<-length(subset(p3$SH, p3$SH>=26 & p3$SH<76))
num_total_p3<-length(p3$SH)
proportion_seed_p3<-num_seed_p3/num_total_p3
#0.60 seed

num_seed_p4<-length(subset(p4$SH, p4$SH>=26 & p4$SH<76))
num_total_p4<-length(p4$SH)
proportion_seed_p4<-num_seed_p4/num_total_p4
#0.10 seed

num_seed_p5<-length(subset(p5$SH, p5$SH>=26 & p5$SH<76))
num_total_p5<-length(p5$SH)
proportion_seed_p5<-num_seed_p5/num_total_p5
#0.16 seed

num_seed_p6<-length(subset(p6$SH, p6$SH>=26 & p6$SH<76))
num_total_p6<-length(p6$SH)
proportion_seed_p6<-num_seed_p6/num_total_p6
#0.02 seed

num_seed_p7<-length(subset(p7$SH, p7$SH>=26 & p7$SH<76))
num_total_p7<-length(p7$SH)
proportion_seed_p7<-num_seed_p7/num_total_p7
#0.07 seed

num_seed_p8<-length(subset(p8$SH, p8$SH>=26 & p8$SH<76))
num_total_p8<-length(p8$SH)
proportion_seed_p8<-num_seed_p8/num_total_p8
#0.11 seed

num_seed_p9<- length(subset(p9$SH, p9$SH>=26 & p9$SH<76))
num_total_p9<-length(p9$SH)
proportion_seed_p9<-num_seed_p9/num_total_p9
#0.01 seed

####Legal######
p2 <- subset(h3, h3$Period == 2)
num_Legal_p2<-length(subset(p2$SH, p2$SH>=76))
num_total_p2<-length(p2$SH)
proportion_Legal_p2<-num_Legal_p2/num_total_p2
#0.0002 Legal

p3 <- subset(h3, h3$Period == 3)
num_Legal_p3<-length(subset(p3$SH, p3$SH>=76))
num_total_p3<-length(p3$SH)
proportion_Legal_p3<-num_Legal_p3/num_total_p3
#0.03 Legal

p4 <- subset(h3, h3$Period == 4)
num_Legal_p4<-length(subset(p2$SH, p2$SH>=76))
num_total_p4<-length(p4$SH)
proportion_Legal_p4<-num_Legal_p4/num_total_p4
#0.0002 Legal

p5 <- subset(h3, h3$Period == 5)
num_Legal_p5<-length(subset(p5$SH, p5$SH>=76))
num_total_p5<-length(p5$SH)
proportion_Legal_p5<-num_Legal_p5/num_total_p5
#0.03 Legal

p6 <- subset(h3, h3$Period == 6)
num_Legal_p6<-length(subset(p6$SH, p6$SH>=76))
num_total_p6<-length(p6$SH)
proportion_Legal_p6<-num_Legal_p6/num_total_p6
#0.0002 Legal

p7 <- subset(h3, h3$Period == 7)
num_Legal_p7<-length(subset(p2$SH, p2$SH>=76))
num_total_p7<-length(p7$SH)
proportion_Legal_p7<-num_Legal_p7/num_total_p7
#0.0007 Legal

p8 <- subset(h3, h3$Period == 8)
num_Legal_p8<-length(subset(p8$SH, p8$SH>=76))
num_total_p8<-length(p8$SH)
proportion_Legal_p8<-num_Legal_p8/num_total_p8
#0 Legal

p9 <- subset(h3, h3$Period == 9)
num_Legal_p9<-length(subset(p9$SH, p9$SH>=76))
num_total_p9<-length(p9$SH)
proportion_Legal_p9<-num_Legal_p9/num_total_p9
#0.30 Legal


#####################

height_dat <- h3 %>% 
  filter(!is.na(SH))

num_speck<-length(height_dat$SH[height_dat$SH<=10])
num_total<-length(height_dat$SH)
proportion_speck<-num_speck/num_total


d1<-ggplot(height_dat, aes(x=SH))+
  geom_density(color="darkblue", fill="lightblue")+
  scale_x_continuous(breaks=seq(0,140,10))+
  xlab("Shell height (mm)") +
  ylab("PDF") +
  facet_wrap(~Period)

max(height_dat$SH)




gg_dens_ridge_treat <- ggplot(data = h3, 
                              aes(x = SH,
                                  y = Period,
                                  fill = Period)) +
  geom_density_ridges(aes(point_color = Period,
                          point_fill = Period),
                      jittered_points = TRUE,
                      alpha = 0.3,
                      point_alpha = 0.5) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975),
                      alpha = 0.3) +
  scale_x_continuous(limits = c(0, 125)) +
  facet_wrap(~ harvest) +
  xlab("Oyster Heights (mm)") +
  ylab("") +
  theme_ridges() +
  theme(legend.position = "none")



