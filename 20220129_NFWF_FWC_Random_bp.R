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


#from Matt Davis To calculate the number of spat I 
#bp question
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
d3$Season[d3$Period == 1 | d3$Period == 3 | d3$Period == 5 | d3$Period == 7 | d3$Period == 9] <- "Summer"

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

#now multiply these proportions * the TotalOysters
#round it so there are no fractions of oysters
#and convert to integer
d4$TotalSpat <-as.integer(round((d4$TotalOysters * d4$Spatprop),0))


# ###################
# #from oyster weekly report
# 
# options(scipen = 2)
# sumstats = function(x){ 
#   y=na.omit(x)
#   bstrap <- c()
#   for (i in 1:1000){
#     bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
#   c(
#     Mean=mean(y), 
#     Median=median(y),
#     SD=sd(y), 
#     Var=var(y),
#     CV=sd(y)/mean(y),
#     SE=sd(y)/sqrt(length(y)),
#     L95SE=mean(y)-1.96*(sd(y)/sqrt(length(y))),
#     U95SE=mean(y)+1.96*(sd(y)/sqrt(length(y))),
#     BSMEAN = mean(bstrap),
#     L95BS = quantile(bstrap,.025),
#     U95BS= quantile(bstrap,.975))
# }
# 
# a<-round(sumstats(d3$LiveSpat[d3$StationName == "NFWF Hotel Bar" & d3$Period == "2" ]),2)
# write.table(a, file = "hotel_p2.txt", row.names = TRUE,
#             col.names = TRUE,sep = ",")
# 
# 
# b<-round(sumstats(d3$LiveSpat[d3$StationName == "NFWF Hotel Bar" & d3$Period == "9" ]),2)
# write.table(b, file = "hotel_p9.txt", row.names = TRUE,
#             col.names = TRUE,sep = ",")
# 
# 
# sumstats(d3$LiveSpat[d3$StationName == "NFWF Hotel Bar"])
# sumstats(d3$LiveSpat[d3$StationName == "NFWF Dry Bar"])

##################

#by period & station
summary1<-d4%>%
  group_by(Period,StationName)%>%
  summarise(mean=mean(TotalSpat,na.rm=TRUE),
            std_dev=sd(TotalSpat, na.rm=TRUE))

#add bootstrap mean and 95% CI to plot

ggplot(d4, aes(Period, TotalSpat)) +
  geom_point(na.rm=TRUE) +
  ggtitle("Total Spat by Period") +
  xlab("Period") +
  ylab("TotalSpat") +
  facet_wrap(~StationName) +
  stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 0.6,na.rm=TRUE)

# #now total wt
# 
# ggplot(d3.1, aes(Period, TotalWt)) +
#   geom_point() +
#   ggtitle("Total Weight by Period") +
#   xlab("Period") +
#   ylab("Total Weight") +
#   facet_wrap(~StationName) +
#   stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 0.6)

# #now make another plot with the bootstrap mean and CI
# ggplot(report_table, aes(Period, mean)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lwr, ymax = upr)) +
#   ggtitle(StationName) +
#   xlab("Period") +
#   ylab("Live Spat") +
#   facet_wrap(~StationName)


#########
live <- d4 %>%
  group_by(StationName, Period) %>%
  summarise(sum = n())
live

#sum live counts for each transect
count_live=aggregate(TotalSpat~StationName+StationNumber+Cultch+Period+Season,data=d4,sum)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(TotalSpat~StationName+StationNumber+Cultch+Period+Season,data=d4,length)
count_quads <- dplyr::rename(count_quads,StationName=StationName,StationNumber=StationNumber, Cultch=Cultch, Num_quads=TotalSpat, Period=Period,Season=Season)


#merge live count total data frame with the tran_length total data frame
d5=merge(count_live,count_quads,by=c("StationName","StationNumber","Cultch","Period", "Season"))

names(d5)

#now add drills to dataset - took the mean - should it be sum?
count_drills = aggregate(Drills~StationName+StationNumber+Cultch+Period+Season,data=d3,mean)
d5 = merge(d5, count_drills, by=c("StationName", "StationNumber", "Cultch", "Period", "Season"), all.x=TRUE)

# #summary table thinking about dispersion
# #by station and cultch density
# summarise_live<-d5%>%
#   group_by(StationName,Cultch,Period)%>%
#   summarise(mean=mean(LiveSpat,na.rm=TRUE),
#             var=var(LiveSpat, na.rm=TRUE))


#plot
jim1<-ggplot(d5, aes(x=Cultch, y= TotalSpat, color=StationName)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Total Spat by Station") +
  xlab("Cultch") +
  ylab("Total Spat") +
  facet_wrap(~Period)

#

d5$StationName <- as.factor(d5$StationName)
d5$Season <- as.factor(d5$Season)
#fit basic NB GLM
m1 <- glm.nb(TotalSpat ~ Period + offset(log(Num_quads)), data = d5) 
m2 <- glm.nb(TotalSpat ~ Period + StationName + offset(log(Num_quads)), data = d5) 
m3 <- glm.nb(TotalSpat ~ Period * StationName + offset(log(Num_quads)), data = d5) 
m4 <- glm.nb(TotalSpat ~ Cultch + offset(log(Num_quads)), data = d5) 
m5 <- glm.nb(TotalSpat ~ Cultch + Period + offset(log(Num_quads)), data = d5) 
m6 <- glm.nb(TotalSpat ~ Cultch + Period + StationName + offset(log(Num_quads)), data = d5) 
m7 <- glm.nb(TotalSpat ~ Cultch + Period + StationName + Season + offset(log(Num_quads)), data = d5) 
m8 <- glm.nb(TotalSpat ~ Drills + offset(log(Num_quads)), data = d5)
m9 <- glm.nb(TotalSpat ~ Cultch + Period + StationName + Season + Drills + offset(log(Num_quads)), data = d5) 
#model with drills (m9) is Lower then m7 (delta AIC of 821)

cand.set = list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
modnames = c("period", "period + station", "period * station", "cultch", "cultch + period", "cultch+period+station", "cultch+period+station+Season", "drills", "cultch+period+station+Season+drills")
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

#
#https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
anova(m7, m9)

summary(m7)

#try a model without period 2 just to see
temp <- subset(d5, d5$Period > 2)
m_temp <- glm.nb(TotalSpat ~ Cultch + Period + StationName + Season + offset(log(Num_quads)), data = temp)
summary(m_temp)

#plot
jim2<-ggplot(temp, aes(x=Cultch, y= TotalSpat, color=StationName)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Total Spat by Station") +
  xlab("Cultch") +
  ylab("Live Spat") +
  facet_wrap(~Period)


plot_grid(jim1,jim2)

#very little difference, guess it doesn't really matter - 
#prob b/c just period 2 is so much higher than others

# 
# 
# ##use m7 to predict and create plots
# newdata2 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 2, Num_quads = 15)
# newdata3 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 3, Num_quads = 15)
# newdata4 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 4, Num_quads = 15)
# newdata5 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 5, Num_quads = 15)
# newdata6 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 6, Num_quads = 15)
# newdata7 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 7, Num_quads = 15)
# newdata8 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 8, Num_quads = 15)
# newdata9 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), Season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
#                        Period = 9, Num_quads = 15)
# 
# test2 = cbind(newdata2, predict(m7, 
#                                 newdata=newdata2, 
#                                 type = 'response', se.fit = T))
# test3 = cbind(newdata3, predict(m7, 
#                                 newdata=newdata3, 
#                                 type = 'response', se.fit = T))
# test4 = cbind(newdata4, predict(m7, 
#                                 newdata=newdata4, 
#                                 type = 'response', se.fit = T))
# test5 = cbind(newdata5, predict(m7, 
#                                 newdata=newdata5, 
#                                 type = 'response', se.fit = T))
# test6 = cbind(newdata6, predict(m7, 
#                                 newdata=newdata6, 
#                                 type = 'response', se.fit = T))
# test7 = cbind(newdata7, predict(m7, 
#                                 newdata=newdata7, 
#                                 type = 'response', se.fit = T))
# test8 = cbind(newdata8, predict(m7, 
#                                 newdata=newdata8, 
#                                 type = 'response', se.fit = T))
# test9 = cbind(newdata9, predict(m7, 
#                                 newdata=newdata9, 
#                                 type = 'response', se.fit = T))
# 

# 
# 
# #plot for period 2
# par(mfrow=c(1,3))
# plot(test2$Cultch[1:5], test2$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test2$Cultch[6:10], test2$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test2$Cultch[1:5], rev(test2$Cultch[1:5])),
#         y = c(test2$fit[1:5] - 2*test2$se.fit[1:5], 
#               rev(test2$fit[1:5] + 2*test2$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test2$Cultch[6:10], rev(test2$Cultch[6:10])),
#         y = c(test2$fit[6:10] - 2*test2$se.fit[6:10], 
#               rev(test2$fit[6:10] + 2*test2$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test2$Cultch[11:15], test2$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test2$Cultch[16:20], test2$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test2$Cultch[11:15], rev(test2$Cultch[11:15])),
#         y = c(test2$fit[11:15] - 2*test2$se.fit[11:15], 
#               rev(test2$fit[11:15] + 2*test2$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test2$Cultch[16:20], rev(test2$Cultch[16:20])),
#         y = c(test2$fit[16:20] - 2*test2$se.fit[16:20], 
#               rev(test2$fit[16:20] + 2*test2$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test2$Cultch[21:25], test2$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test2$Cultch[26:30], test2$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test2$Cultch[21:25], rev(test2$Cultch[21:25])),
#         y = c(test2$fit[21:25] - 2*test2$se.fit[21:25], 
#               rev(test2$fit[21:25] + 2*test2$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test2$Cultch[26:30], rev(test2$Cultch[26:30])),
#         y = c(test2$fit[26:30] - 2*test2$se.fit[26:30], 
#               rev(test2$fit[26:30] + 2*test2$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")
# 
# #plot for period 3
# par(mfrow=c(1,3))
# plot(test3$Cultch[1:5], test3$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test3$Cultch[6:10], test3$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test3$Cultch[1:5], rev(test3$Cultch[1:5])),
#         y = c(test3$fit[1:5] - 2*test3$se.fit[1:5], 
#               rev(test3$fit[1:5] + 2*test3$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test3$Cultch[6:10], rev(test3$Cultch[6:10])),
#         y = c(test3$fit[6:10] - 2*test3$se.fit[6:10], 
#               rev(test3$fit[6:10] + 2*test3$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test3$Cultch[11:15], test3$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test3$Cultch[16:20], test3$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test3$Cultch[11:15], rev(test3$Cultch[11:15])),
#         y = c(test3$fit[11:15] - 2*test3$se.fit[11:15], 
#               rev(test3$fit[11:15] + 2*test3$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test3$Cultch[16:20], rev(test3$Cultch[16:20])),
#         y = c(test3$fit[16:20] - 2*test3$se.fit[16:20], 
#               rev(test3$fit[16:20] + 2*test3$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test3$Cultch[21:25], test3$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test3$Cultch[26:30], test3$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test3$Cultch[21:25], rev(test3$Cultch[21:25])),
#         y = c(test3$fit[21:25] - 2*test3$se.fit[21:25], 
#               rev(test3$fit[21:25] + 2*test3$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test3$Cultch[26:30], rev(test3$Cultch[26:30])),
#         y = c(test3$fit[26:30] - 2*test3$se.fit[26:30], 
#               rev(test3$fit[26:30] + 2*test3$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")
# 
# #plot for period 4
# par(mfrow=c(1,3))
# plot(test4$Cultch[1:5], test4$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test4$Cultch[6:10], test4$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test4$Cultch[1:5], rev(test4$Cultch[1:5])),
#         y = c(test4$fit[1:5] - 2*test4$se.fit[1:5], 
#               rev(test4$fit[1:5] + 2*test4$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test4$Cultch[6:10], rev(test4$Cultch[6:10])),
#         y = c(test4$fit[6:10] - 2*test4$se.fit[6:10], 
#               rev(test4$fit[6:10] + 2*test4$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test4$Cultch[11:15], test4$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test4$Cultch[16:20], test4$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test4$Cultch[11:15], rev(test4$Cultch[11:15])),
#         y = c(test4$fit[11:15] - 2*test4$se.fit[11:15], 
#               rev(test4$fit[11:15] + 2*test4$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test4$Cultch[16:20], rev(test4$Cultch[16:20])),
#         y = c(test4$fit[16:20] - 2*test4$se.fit[16:20], 
#               rev(test4$fit[16:20] + 2*test4$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test4$Cultch[21:25], test4$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test4$Cultch[26:30], test4$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test4$Cultch[21:25], rev(test4$Cultch[21:25])),
#         y = c(test4$fit[21:25] - 2*test4$se.fit[21:25], 
#               rev(test4$fit[21:25] + 2*test4$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test4$Cultch[26:30], rev(test4$Cultch[26:30])),
#         y = c(test4$fit[26:30] - 2*test4$se.fit[26:30], 
#               rev(test4$fit[26:30] + 2*test4$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")
# 
# #plot for period 5
# par(mfrow=c(1,3))
# plot(test5$Cultch[1:5], test5$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test5$Cultch[6:10], test5$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test5$Cultch[1:5], rev(test5$Cultch[1:5])),
#         y = c(test5$fit[1:5] - 2*test5$se.fit[1:5], 
#               rev(test5$fit[1:5] + 2*test5$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test5$Cultch[6:10], rev(test5$Cultch[6:10])),
#         y = c(test5$fit[6:10] - 2*test5$se.fit[6:10], 
#               rev(test5$fit[6:10] + 2*test5$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test5$Cultch[11:15], test5$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test5$Cultch[16:20], test5$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test5$Cultch[11:15], rev(test5$Cultch[11:15])),
#         y = c(test5$fit[11:15] - 2*test5$se.fit[11:15], 
#               rev(test5$fit[11:15] + 2*test5$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test5$Cultch[16:20], rev(test5$Cultch[16:20])),
#         y = c(test5$fit[16:20] - 2*test5$se.fit[16:20], 
#               rev(test5$fit[16:20] + 2*test5$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test5$Cultch[21:25], test5$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test5$Cultch[26:30], test5$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test5$Cultch[21:25], rev(test5$Cultch[21:25])),
#         y = c(test5$fit[21:25] - 2*test5$se.fit[21:25], 
#               rev(test5$fit[21:25] + 2*test5$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test5$Cultch[26:30], rev(test5$Cultch[26:30])),
#         y = c(test5$fit[26:30] - 2*test5$se.fit[26:30], 
#               rev(test5$fit[26:30] + 2*test5$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")
# 
# #plot for period 6
# par(mfrow=c(1,3))
# plot(test6$Cultch[1:5], test6$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test6$Cultch[6:10], test6$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test6$Cultch[1:5], rev(test6$Cultch[1:5])),
#         y = c(test6$fit[1:5] - 2*test6$se.fit[1:5], 
#               rev(test6$fit[1:5] + 2*test6$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test6$Cultch[6:10], rev(test6$Cultch[6:10])),
#         y = c(test6$fit[6:10] - 2*test6$se.fit[6:10], 
#               rev(test6$fit[6:10] + 2*test6$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test6$Cultch[11:15], test6$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test6$Cultch[16:20], test6$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test6$Cultch[11:15], rev(test6$Cultch[11:15])),
#         y = c(test6$fit[11:15] - 2*test6$se.fit[11:15], 
#               rev(test6$fit[11:15] + 2*test6$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test6$Cultch[16:20], rev(test6$Cultch[16:20])),
#         y = c(test6$fit[16:20] - 2*test6$se.fit[16:20], 
#               rev(test6$fit[16:20] + 2*test6$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test6$Cultch[21:25], test6$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test6$Cultch[26:30], test6$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test6$Cultch[21:25], rev(test6$Cultch[21:25])),
#         y = c(test6$fit[21:25] - 2*test6$se.fit[21:25], 
#               rev(test6$fit[21:25] + 2*test6$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test6$Cultch[26:30], rev(test6$Cultch[26:30])),
#         y = c(test6$fit[26:30] - 2*test6$se.fit[26:30], 
#               rev(test6$fit[26:30] + 2*test6$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")
# 
# #plot for period 7
# par(mfrow=c(1,3))
# plot(test7$Cultch[1:5], test7$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test7$Cultch[6:10], test7$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test7$Cultch[1:5], rev(test7$Cultch[1:5])),
#         y = c(test7$fit[1:5] - 2*test7$se.fit[1:5], 
#               rev(test7$fit[1:5] + 2*test7$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test7$Cultch[6:10], rev(test7$Cultch[6:10])),
#         y = c(test7$fit[6:10] - 2*test7$se.fit[6:10], 
#               rev(test7$fit[6:10] + 2*test7$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test7$Cultch[11:15], test7$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test7$Cultch[16:20], test7$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test7$Cultch[11:15], rev(test7$Cultch[11:15])),
#         y = c(test7$fit[11:15] - 2*test7$se.fit[11:15], 
#               rev(test7$fit[11:15] + 2*test7$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test7$Cultch[16:20], rev(test7$Cultch[16:20])),
#         y = c(test7$fit[16:20] - 2*test7$se.fit[16:20], 
#               rev(test7$fit[16:20] + 2*test7$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test7$Cultch[21:25], test7$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test7$Cultch[26:30], test7$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test7$Cultch[21:25], rev(test7$Cultch[21:25])),
#         y = c(test7$fit[21:25] - 2*test7$se.fit[21:25], 
#               rev(test7$fit[21:25] + 2*test7$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test7$Cultch[26:30], rev(test7$Cultch[26:30])),
#         y = c(test7$fit[26:30] - 2*test7$se.fit[26:30], 
#               rev(test7$fit[26:30] + 2*test7$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")
# 
# #plot for period 8
# par(mfrow=c(1,3))
# plot(test8$Cultch[1:5], test8$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test8$Cultch[6:10], test8$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test8$Cultch[1:5], rev(test8$Cultch[1:5])),
#         y = c(test8$fit[1:5] - 2*test8$se.fit[1:5], 
#               rev(test8$fit[1:5] + 2*test8$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test8$Cultch[6:10], rev(test8$Cultch[6:10])),
#         y = c(test8$fit[6:10] - 2*test8$se.fit[6:10], 
#               rev(test8$fit[6:10] + 2*test8$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test8$Cultch[11:15], test8$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
# lines(test8$Cultch[16:20], test8$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test8$Cultch[11:15], rev(test8$Cultch[11:15])),
#         y = c(test8$fit[11:15] - 2*test8$se.fit[11:15], 
#               rev(test8$fit[11:15] + 2*test8$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test8$Cultch[16:20], rev(test8$Cultch[16:20])),
#         y = c(test8$fit[16:20] - 2*test8$se.fit[16:20], 
#               rev(test8$fit[16:20] + 2*test8$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test8$Cultch[21:25], test8$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
# lines(test8$Cultch[26:30], test8$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test8$Cultch[21:25], rev(test8$Cultch[21:25])),
#         y = c(test8$fit[21:25] - 2*test8$se.fit[21:25], 
#               rev(test8$fit[21:25] + 2*test8$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test8$Cultch[26:30], rev(test8$Cultch[26:30])),
#         y = c(test8$fit[26:30] - 2*test8$se.fit[26:30], 
#               rev(test8$fit[26:30] + 2*test8$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")

# 
# #plot for period 9
# par(mfrow=c(1,3))
# plot(test9$Cultch[1:5], test9$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test9$Cultch[6:10], test9$fit[6:10], lwd = 2, col = 'red')
# polygon(x = c(test9$Cultch[1:5], rev(test9$Cultch[1:5])),
#         y = c(test9$fit[1:5] - 2*test9$se.fit[1:5], 
#               rev(test9$fit[1:5] + 2*test9$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test9$Cultch[6:10], rev(test9$Cultch[6:10])),
#         y = c(test9$fit[6:10] - 2*test9$se.fit[6:10], 
#               rev(test9$fit[6:10] + 2*test9$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Bulkhead")
# 
# plot(test9$Cultch[11:15], test9$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test9$Cultch[16:20], test9$fit[16:20], lwd = 2, col = 'red')
# polygon(x = c(test9$Cultch[11:15], rev(test9$Cultch[11:15])),
#         y = c(test9$fit[11:15] - 2*test9$se.fit[11:15], 
#               rev(test9$fit[11:15] + 2*test9$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test9$Cultch[16:20], rev(test9$Cultch[16:20])),
#         y = c(test9$fit[16:20] - 2*test9$se.fit[16:20], 
#               rev(test9$fit[16:20] + 2*test9$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Dry Bar")
# 
# plot(test9$Cultch[21:25], test9$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
# lines(test9$Cultch[26:30], test9$fit[26:30], lwd = 2, col = 'red')
# polygon(x = c(test9$Cultch[21:25], rev(test9$Cultch[21:25])),
#         y = c(test9$fit[21:25] - 2*test9$se.fit[21:25], 
#               rev(test9$fit[21:25] + 2*test9$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
# polygon(x = c(test9$Cultch[26:30], rev(test9$Cultch[26:30])),
#         y = c(test9$fit[26:30] - 2*test9$se.fit[26:30], 
#               rev(test9$fit[26:30] + 2*test9$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
# legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Winter"], pch = 16)
# points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$Season == "Summer"], pch = 16, col = 'red')
# title("NFWF Hotel Bar")

###################################
#now take mean TotalWt and include in new GLMs
###################################

mean_wt=aggregate(TotalWt~StationName+StationNumber+Cultch+Period+Season,data=d3,mean)
names(mean_wt)[6]<-c("Mean_weight")



#or use the mean b/c the offset is applied to the counts, so that's why we use sum
#but offset not applied to weights

# mean_wt=aggregate(TotalWt~StationName+StationNumber+Cultch+Period+Season,data=d3,mean)
# names(mean_wt)[6]<-c("Mean_weight")

#merge live & tran_length total data frame
d6=merge(d5,mean_wt, by=c("StationName","StationNumber","Cultch","Period", "Season"))


as.integer(d6$Mean_weight)

#fit basic NB GLM
m1.1 <- glm.nb(TotalSpat ~ Period + offset(log(Num_quads)), data = d6)
m1.2 <- glm.nb(TotalSpat ~ Mean_weight + Period+ offset(log(Num_quads)), data = d6) 
m2.1 <- glm.nb(TotalSpat ~ Period + StationName + offset(log(Num_quads)), data = d6) 
m3.1 <- glm.nb(TotalSpat ~ Period * StationName + offset(log(Num_quads)), data = d6) 
m4.1 <- glm.nb(TotalSpat ~ Cultch +Mean_weight + offset(log(Num_quads)), data = d6) 
m5.1 <- glm.nb(TotalSpat ~ Cultch +Mean_weight + Period + offset(log(Num_quads)), data = d6) 
m6.1 <- glm.nb(TotalSpat ~ Cultch +Mean_weight + Period + StationName + offset(log(Num_quads)), data = d6) 
m7.1 <- glm.nb(TotalSpat ~ Cultch + Mean_weight + Period + StationName + Season + offset(log(Num_quads)), data = d6) 


cand.set = list(m1.1,m1.2,m3.1,m4.1,m5.1,m6.1,m7.1)
modnames = c("year", "year + station", "year * station","Cultch+MeanWt", "Cultch + MeanWt + year","Cultch + MeanWt + year+ Station", "Cultch + MeanWt + year+ Station+Season")
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

summary(m7.1)

names (d7)

#plot
f6<-ggplot(d6, aes(x=Cultch, y= Mean_weight, color=StationName)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Mean weight material from quadrats and Cultch density") +
  xlab("Cultch") +
  ylab("Mean weight from quadrat") +
  facet_wrap(~Period)
f6

#plot
f7<-ggplot(d6, aes(x=Mean_weight, y= TotalSpat, color=StationName)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Total Spat and Mean Cultch Biomass") +
  ylim(0,30000)+
  xlab("Mean weight") +
  ylab("Live oyster spat") +
  facet_wrap(~Period)
f7

#hmm how much does weight change with each period
#let's go back a little and keep it simple and just assume
#same number quadrats each period

par(mfrow=c(1,1))
plot(d3$TotalWt~d3$Period)

w1 <- lm(TotalWt ~ Period, data = d3)
summary(w1)

abline(w1)

#yes total weight declines over time but model is terrible, 
#should be exponential decay or maybe just delete the zero cultch
#but zero cultch could still accumulate weight, so maybe not
# delete zero cultch

period_pred<-seq(min(d3$Period),max(d3$Period),length=200)

nwx1=data.frame(Period=period_pred) #make sequence data a data frame

prd=predict(w1,newdata=nwx1,interval="confidence",
            level = 0.95)

plot(d3$TotalWt~d3$Period)
abline(w1, col="lightblue")

matlines(period_pred, prd[,2:3], col = "blue", lty=2)

#now convert cultch to factor to see if 
#all the cultch levels decline over time
d3.1<-d3
d3.1$Cultch <- as.factor(d3.1$Cultch)

w1.1 <- glm(TotalWt ~ Period*Cultch, data = d3.1)
summary(w1.1)
#and yes it declines over time for all densities
#but model still terrible fit

#we can return later to modeling cultch declines and fit better models

############
############
##Random effects
############

#need to go back to d4 so we have quadrat and treat the quadrat as a random effect

#sum live counts for each quadrat just to check
#this isn't really necessary as the counts are by transect,
#but it makes it easier to merge with number of quads
rcount_live=aggregate(TotalSpat~StationName+StationNumber+Quadrat+Cultch+Period+Season,
                      data=d4,sum)

#merge live count total data frame with the tran_length total data frame
r5=merge(rcount_live,count_quads,by=c("StationName","StationNumber","Cultch","Period", "Season"))


library(lme4) #mixed effect models
library(MASS) #negative binomial models

#make quadrat factor for random effect
r5$StationName<-as.factor(r5$StationName)


#no offset, station name as random
r0.0 <- glmer.nb(TotalSpat ~ Period + (1|StationName), data = r5) #no converge
r0.1 <- glmer.nb(TotalSpat ~ Cultch + (1|StationName), data = r5) #no converge
r0.2 <- glmer.nb(TotalSpat ~ Cultch + Period + (1|StationName), data = r5) #converge
r0.3 <- glmer.nb(TotalSpat ~ Cultch + Period + Season + (1|StationName), data = r5) # no converge




#tran_length as an offset and quadrat a random effect


#r0 <- glmer.nb(TotalSpat ~ Period + (1|StationName) + offset(log(Num_quads)), data = r5) #no converge

#no idea on these convergence issues

#is it worth trying glmmADMB?
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)

fit_zipoiss<- glmmadmb(TotalSpat ~ Cultch + 
                         #offset(log(Num_quads)
                         +(1|StationName), 
                          data = r5,
                          zeroInflation=TRUE,
                          family="poisson")
