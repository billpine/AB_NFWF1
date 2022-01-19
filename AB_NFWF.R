#These are the FWC NFWF1 data provided by Ryan Gandy Jan 12, 2021

#TO DO

#bring in the summary stats loop as in the weekly report
#fix the period so you can account for summer and winter
#plots of observed and predicted counts by station and density
#think about year, since these are just spat counts does that matter?

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

str(d2)

names(d2)

#subset the columns to the ones you want to work with
d3 <- d2 %>% 
  dplyr::select(Survey, Date, StationName, StationNumber, Cultch, 
                Quadrat, TotalVol, TotalWt, LiveSpat, Drills)

#just extra year, month, day from the single date column

d3 <- d3 %>%
  mutate(Year = year(d3$Date),
         Month = month(d3$Date),
         Day = day(d3$Date))


#some simple plots

#let's look at cultch (x axis) and live spat (y axis)
#for each Station (color) and then the red is the mean and 95% CI for the treatment

theme_set(theme_gray(base_size = 18))

f1<-ggplot(d3, aes(Cultch, LiveSpat, color=StationName)) +
  geom_point(size=4) +
  ggtitle("Live Spat by Cultch") +
  xlab("Cultch") +
  ylab("Live Spat") +
  stat_summary(fun = mean, geom = "point", size=1.5, aes(group= StationName), color="gold") +
  stat_summary(fun.data = "mean_cl_boot",aes(group= StationName), size = 1.5, geom = "errorbar", width = 0.5, color="gold")

f1

#work on this  more https://stackoverflow.com/questions/17414565/interpretation-of-stat-summary-mean-cl-boot-at-ggplot2

###
f2<-ggplot(d3, aes(Cultch, LiveSpat, color=StationName)) +
  geom_point() +
  ggtitle("Live Spat by Cultch") +
  xlab("Cultch") +
  ylab("Live Spat") +
  facet_wrap(~Year) +
  stat_summary(fun = mean, geom = "point", size=1.5, aes(group= StationName), color="gold") 
#+
#  stat_summary(fun.data = "mean_cl_boot",aes(group= StationName), size = 1.5, geom = "errorbar", width = 0.5, color="gold")
  

f2


#make summary table


month <- d3 %>%
  dplyr::group_by(Year, Month, StationName) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Year, Month, StationName)
names(month) <- c("Year", "Month", "Station Name",
                  "Number Quadrats")

#just writing the table with number of quadrats by year, month, station to folder
write.table(month, file = "yr_mnth_station.txt", row.names = FALSE,
            col.names = TRUE,sep = ",")


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer


#######################
#add period to split the year like we do with Lone Cabbage
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

d3$season <- "Winter"
d3$season[d3$Period == 1 | d3$Period == 3 | d3$Period == 5 | d3$Period == 7 | d3$Period == 9] <- "Summer"

d3$season

period <- d3 %>%
  dplyr::group_by(Period,Year, Month, StationName) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Period, Year, Month, StationName)
names(period) <- c("Period", "Year","Month", "Station Name",
                  "Number Quadrats")

#just writing the table with number of quadrats by year, month, station to folder
write.table(period, file = "period_yr_mnth_station.txt", row.names = FALSE,
            col.names = TRUE,sep = ",")

##We will mostly work with period as the unit of time
##This combines some of the months obviously

###################
#from oyster weekly report

options(scipen = 2)
sumstats = function(x){ 
  y=x
  bstrap <- c()
  for (i in 1:1000){
    bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
  c(
    Mean=mean(y), 
    Median=median(y),
    SD=sd(y), 
    Var=var(y),
    CV=sd(y)/mean(y),
    SE=sd(y)/sqrt(length(y)),
    L95SE=mean(y)-1.96*(sd(y)/sqrt(length(y))),
    U95SE=mean(y)+1.96*(sd(y)/sqrt(length(y))),
    BSMEAN = mean(bstrap),
    L95BS = quantile(bstrap,.025),
    U95BS= quantile(bstrap,.975))
}

a<-round(sumstats(d3$LiveSpat[d3$StationName == "NFWF Hotel Bar" & d3$Period == "2" ]),2)
write.table(a, file = "hotel_p2.txt", row.names = TRUE,
            col.names = TRUE,sep = ",")


b<-round(sumstats(d3$LiveSpat[d3$StationName == "NFWF Hotel Bar" & d3$Period == "9" ]),2)
write.table(b, file = "hotel_p9.txt", row.names = TRUE,
            col.names = TRUE,sep = ",")


sumstats(d3$LiveSpat[d3$StationName == "NFWF Hotel Bar"])
sumstats(d3$LiveSpat[d3$StationName == "NFWF Dry Bar"])

##################



##########################
windows(record=T)
####
#some boxplots of elevation
p1<-ggplot(data=d3) +
  labs(title="Counts of live spat") + 
  geom_boxplot(
    mapping = aes(
      x=StationName,
      y=LiveSpat))
  p1

p1.1<-p1+  
  facet_grid(Period~.) +
    labs(title = "Boxplot by period")
p1.1
    
#wow complete collapse in counts

p2<-ggplot(data=d3) +
  labs(title="Counts of live spat") + 
  aes(x=LiveSpat,
      color=StationName)+
stat_density(aes(group = StationName), position="stack",geom="line", 
             size= 1.5)
p2

p2.1<-p2+  
  facet_grid(Period~.) +
  labs(title = "PDF live spat by period")
p2.1

#this collapse is odd

#by period & station
summary1<-d3%>%
  group_by(Period,StationName)%>%
  summarise(mean=mean(LiveSpat,na.rm=TRUE),
            std_dev=sd(LiveSpat, na.rm=TRUE))

#add bootstrap mean and 95% CI to plot

ggplot(d3, aes(Period, LiveSpat)) +
  geom_point() +
  ggtitle("Live Spat by Period") +
  xlab("Period") +
  ylab("Live Spat") +
  facet_wrap(~StationName) +
  stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 0.6)

#now total wt


ggplot(d3.1, aes(Period, TotalWt)) +
  geom_point() +
  ggtitle("Total Weight by Period") +
  xlab("Period") +
  ylab("Total Weight") +
  facet_wrap(~StationName) +
  stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 0.6)



# #now make another plot with the bootstrap mean and CI
# ggplot(report_table, aes(Period, mean)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lwr, ymax = upr)) +
#   ggtitle(StationName) +
#   xlab("Period") +
#   ylab("Live Spat") +
#   facet_wrap(~StationName)


#########
live <- d3 %>%
  group_by(StationName, Period) %>%
  summarise(sum = n())
live

#sum live counts for each transect
count_live=aggregate(LiveSpat~StationName+StationNumber+Cultch+Period+season,data=d3,sum)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(LiveSpat~StationName+StationNumber+Cultch+Period+season,data=d3,length)
count_quads <- dplyr::rename(count_quads,StationName=StationName,StationNumber=StationNumber, Cultch=Cultch, Num_quads=LiveSpat, Period=Period,season=season)


#merge live count total data frame with the tran_length total data frame
d5=merge(count_live,count_quads,by=c("StationName","StationNumber","Cultch","Period", "season"))

names(d5)

#now add drills to dataset - took the mean - does that make sense or should it be sum?
count_drills = aggregate(Drills~StationName+StationNumber+Cultch+Period+season,data=d3,mean)
d5 = merge(d5, count_drills, by=c("StationName", "StationNumber", "Cultch", "Period", "season"), all.x=TRUE)

# #summary table thinking about dispersion
# #by station and cultch density
# summarise_live<-d5%>%
#   group_by(StationName,Cultch,Period)%>%
#   summarise(mean=mean(LiveSpat,na.rm=TRUE),
#             var=var(LiveSpat, na.rm=TRUE))


#plot
ggplot(d5, aes(x=Cultch, y= LiveSpat, color=StationName)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Live Spat by Station") +
  xlab("Cultch") +
  ylab("Live Spat") +
  facet_wrap(~Period)


#convert counts of spat to integers
d5$LiveSpat <- as.integer(d5$LiveSpat)
str(d5)

d5$StationName <- as.factor(d5$StationName)
d5$season <- as.factor(d5$season)
#fit basic NB GLM
m1 <- glm.nb(LiveSpat ~ Period + offset(log(Num_quads)), data = d5) 
m2 <- glm.nb(LiveSpat ~ Period + StationName + offset(log(Num_quads)), data = d5) 
m3 <- glm.nb(LiveSpat ~ Period * StationName + offset(log(Num_quads)), data = d5) 
m4 <- glm.nb(LiveSpat ~ Cultch + offset(log(Num_quads)), data = d5) 
m5 <- glm.nb(LiveSpat ~ Cultch + Period + offset(log(Num_quads)), data = d5) 
m6 <- glm.nb(LiveSpat ~ Cultch + Period + StationName + offset(log(Num_quads)), data = d5) 
m7 <- glm.nb(LiveSpat ~ Cultch + Period + StationName + season + offset(log(Num_quads)), data = d5) 
m8 <- glm.nb(LiveSpat ~ Drills + offset(log(Num_quads)), data = d5)
m9 <- glm.nb(LiveSpat ~ Cultch + Period + StationName + season + Drills + offset(log(Num_quads)), data = d5) 
#model with drills (m9) is quite a bit better then m7 (delta AIC of 830)

#try a model without period 2 just to see
temp <- subset(d5, d5$Period > 2)
m_temp <- glm.nb(LiveSpat ~ Cultch + Period + StationName + season + offset(log(Num_quads)), data = temp)
summary(m_temp)
#very little difference, guess it doesn't really matter - prob b/c just period 2 is crazy number all the rest are low

cand.set = list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
modnames = c("period", "period + station", "period * station", "cultch", "cultch + period", "cultch+period+station", "cultch+period+station+season", "drills", "cultch+period+station+season+drills")
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

summary(m7)

##use m7 to predict and create plots
newdata2 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 2, Num_quads = 15)
newdata3 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 3, Num_quads = 15)
newdata4 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 4, Num_quads = 15)
newdata5 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 5, Num_quads = 15)
newdata6 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 6, Num_quads = 15)
newdata7 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 7, Num_quads = 15)
newdata8 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 8, Num_quads = 15)
newdata9 <- data.frame(Cultch = rep(c(0, 100, 200, 300, 400),each=1,times=6), season = rep(c("Winter", "Summer"), each = 5, times = 3), StationName = rep(c("NFWF Bulkhead", "NFWF Dry Bar", "NFWF Hotel Bar"), each = 10, times = 1),
                       Period = 9, Num_quads = 15)

test2 = cbind(newdata2, predict(m7, 
                                newdata=newdata2, 
                                type = 'response', se.fit = T))
test3 = cbind(newdata3, predict(m7, 
                                newdata=newdata3, 
                                type = 'response', se.fit = T))
test4 = cbind(newdata4, predict(m7, 
                                newdata=newdata4, 
                                type = 'response', se.fit = T))
test5 = cbind(newdata5, predict(m7, 
                                newdata=newdata5, 
                                type = 'response', se.fit = T))
test6 = cbind(newdata6, predict(m7, 
                                newdata=newdata6, 
                                type = 'response', se.fit = T))
test7 = cbind(newdata7, predict(m7, 
                                newdata=newdata7, 
                                type = 'response', se.fit = T))
test8 = cbind(newdata8, predict(m7, 
                                newdata=newdata8, 
                                type = 'response', se.fit = T))
test9 = cbind(newdata9, predict(m7, 
                                newdata=newdata9, 
                                type = 'response', se.fit = T))




#plot for period 2
par(mfrow=c(1,3))
plot(test2$Cultch[1:5], test2$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test2$Cultch[6:10], test2$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test2$Cultch[1:5], rev(test2$Cultch[1:5])),
        y = c(test2$fit[1:5] - 2*test2$se.fit[1:5], 
              rev(test2$fit[1:5] + 2*test2$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test2$Cultch[6:10], rev(test2$Cultch[6:10])),
        y = c(test2$fit[6:10] - 2*test2$se.fit[6:10], 
              rev(test2$fit[6:10] + 2*test2$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test2$Cultch[11:15], test2$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test2$Cultch[16:20], test2$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test2$Cultch[11:15], rev(test2$Cultch[11:15])),
        y = c(test2$fit[11:15] - 2*test2$se.fit[11:15], 
              rev(test2$fit[11:15] + 2*test2$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test2$Cultch[16:20], rev(test2$Cultch[16:20])),
        y = c(test2$fit[16:20] - 2*test2$se.fit[16:20], 
              rev(test2$fit[16:20] + 2*test2$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test2$Cultch[21:25], test2$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test2$Cultch[26:30], test2$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test2$Cultch[21:25], rev(test2$Cultch[21:25])),
        y = c(test2$fit[21:25] - 2*test2$se.fit[21:25], 
              rev(test2$fit[21:25] + 2*test2$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test2$Cultch[26:30], rev(test2$Cultch[26:30])),
        y = c(test2$fit[26:30] - 2*test2$se.fit[26:30], 
              rev(test2$fit[26:30] + 2*test2$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")

#plot for period 3
par(mfrow=c(1,3))
plot(test3$Cultch[1:5], test3$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test3$Cultch[6:10], test3$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test3$Cultch[1:5], rev(test3$Cultch[1:5])),
        y = c(test3$fit[1:5] - 2*test3$se.fit[1:5], 
              rev(test3$fit[1:5] + 2*test3$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test3$Cultch[6:10], rev(test3$Cultch[6:10])),
        y = c(test3$fit[6:10] - 2*test3$se.fit[6:10], 
              rev(test3$fit[6:10] + 2*test3$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test3$Cultch[11:15], test3$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test3$Cultch[16:20], test3$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test3$Cultch[11:15], rev(test3$Cultch[11:15])),
        y = c(test3$fit[11:15] - 2*test3$se.fit[11:15], 
              rev(test3$fit[11:15] + 2*test3$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test3$Cultch[16:20], rev(test3$Cultch[16:20])),
        y = c(test3$fit[16:20] - 2*test3$se.fit[16:20], 
              rev(test3$fit[16:20] + 2*test3$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test3$Cultch[21:25], test3$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test3$Cultch[26:30], test3$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test3$Cultch[21:25], rev(test3$Cultch[21:25])),
        y = c(test3$fit[21:25] - 2*test3$se.fit[21:25], 
              rev(test3$fit[21:25] + 2*test3$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test3$Cultch[26:30], rev(test3$Cultch[26:30])),
        y = c(test3$fit[26:30] - 2*test3$se.fit[26:30], 
              rev(test3$fit[26:30] + 2*test3$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")

#plot for period 4
par(mfrow=c(1,3))
plot(test4$Cultch[1:5], test4$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test4$Cultch[6:10], test4$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test4$Cultch[1:5], rev(test4$Cultch[1:5])),
        y = c(test4$fit[1:5] - 2*test4$se.fit[1:5], 
              rev(test4$fit[1:5] + 2*test4$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test4$Cultch[6:10], rev(test4$Cultch[6:10])),
        y = c(test4$fit[6:10] - 2*test4$se.fit[6:10], 
              rev(test4$fit[6:10] + 2*test4$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test4$Cultch[11:15], test4$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test4$Cultch[16:20], test4$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test4$Cultch[11:15], rev(test4$Cultch[11:15])),
        y = c(test4$fit[11:15] - 2*test4$se.fit[11:15], 
              rev(test4$fit[11:15] + 2*test4$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test4$Cultch[16:20], rev(test4$Cultch[16:20])),
        y = c(test4$fit[16:20] - 2*test4$se.fit[16:20], 
              rev(test4$fit[16:20] + 2*test4$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test4$Cultch[21:25], test4$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test4$Cultch[26:30], test4$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test4$Cultch[21:25], rev(test4$Cultch[21:25])),
        y = c(test4$fit[21:25] - 2*test4$se.fit[21:25], 
              rev(test4$fit[21:25] + 2*test4$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test4$Cultch[26:30], rev(test4$Cultch[26:30])),
        y = c(test4$fit[26:30] - 2*test4$se.fit[26:30], 
              rev(test4$fit[26:30] + 2*test4$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")

#plot for period 5
par(mfrow=c(1,3))
plot(test5$Cultch[1:5], test5$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test5$Cultch[6:10], test5$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test5$Cultch[1:5], rev(test5$Cultch[1:5])),
        y = c(test5$fit[1:5] - 2*test5$se.fit[1:5], 
              rev(test5$fit[1:5] + 2*test5$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test5$Cultch[6:10], rev(test5$Cultch[6:10])),
        y = c(test5$fit[6:10] - 2*test5$se.fit[6:10], 
              rev(test5$fit[6:10] + 2*test5$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test5$Cultch[11:15], test5$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test5$Cultch[16:20], test5$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test5$Cultch[11:15], rev(test5$Cultch[11:15])),
        y = c(test5$fit[11:15] - 2*test5$se.fit[11:15], 
              rev(test5$fit[11:15] + 2*test5$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test5$Cultch[16:20], rev(test5$Cultch[16:20])),
        y = c(test5$fit[16:20] - 2*test5$se.fit[16:20], 
              rev(test5$fit[16:20] + 2*test5$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test5$Cultch[21:25], test5$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test5$Cultch[26:30], test5$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test5$Cultch[21:25], rev(test5$Cultch[21:25])),
        y = c(test5$fit[21:25] - 2*test5$se.fit[21:25], 
              rev(test5$fit[21:25] + 2*test5$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test5$Cultch[26:30], rev(test5$Cultch[26:30])),
        y = c(test5$fit[26:30] - 2*test5$se.fit[26:30], 
              rev(test5$fit[26:30] + 2*test5$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")

#plot for period 6
par(mfrow=c(1,3))
plot(test6$Cultch[1:5], test6$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test6$Cultch[6:10], test6$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test6$Cultch[1:5], rev(test6$Cultch[1:5])),
        y = c(test6$fit[1:5] - 2*test6$se.fit[1:5], 
              rev(test6$fit[1:5] + 2*test6$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test6$Cultch[6:10], rev(test6$Cultch[6:10])),
        y = c(test6$fit[6:10] - 2*test6$se.fit[6:10], 
              rev(test6$fit[6:10] + 2*test6$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test6$Cultch[11:15], test6$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test6$Cultch[16:20], test6$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test6$Cultch[11:15], rev(test6$Cultch[11:15])),
        y = c(test6$fit[11:15] - 2*test6$se.fit[11:15], 
              rev(test6$fit[11:15] + 2*test6$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test6$Cultch[16:20], rev(test6$Cultch[16:20])),
        y = c(test6$fit[16:20] - 2*test6$se.fit[16:20], 
              rev(test6$fit[16:20] + 2*test6$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test6$Cultch[21:25], test6$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test6$Cultch[26:30], test6$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test6$Cultch[21:25], rev(test6$Cultch[21:25])),
        y = c(test6$fit[21:25] - 2*test6$se.fit[21:25], 
              rev(test6$fit[21:25] + 2*test6$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test6$Cultch[26:30], rev(test6$Cultch[26:30])),
        y = c(test6$fit[26:30] - 2*test6$se.fit[26:30], 
              rev(test6$fit[26:30] + 2*test6$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")

#plot for period 7
par(mfrow=c(1,3))
plot(test7$Cultch[1:5], test7$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test7$Cultch[6:10], test7$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test7$Cultch[1:5], rev(test7$Cultch[1:5])),
        y = c(test7$fit[1:5] - 2*test7$se.fit[1:5], 
              rev(test7$fit[1:5] + 2*test7$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test7$Cultch[6:10], rev(test7$Cultch[6:10])),
        y = c(test7$fit[6:10] - 2*test7$se.fit[6:10], 
              rev(test7$fit[6:10] + 2*test7$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test7$Cultch[11:15], test7$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test7$Cultch[16:20], test7$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test7$Cultch[11:15], rev(test7$Cultch[11:15])),
        y = c(test7$fit[11:15] - 2*test7$se.fit[11:15], 
              rev(test7$fit[11:15] + 2*test7$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test7$Cultch[16:20], rev(test7$Cultch[16:20])),
        y = c(test7$fit[16:20] - 2*test7$se.fit[16:20], 
              rev(test7$fit[16:20] + 2*test7$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test7$Cultch[21:25], test7$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test7$Cultch[26:30], test7$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test7$Cultch[21:25], rev(test7$Cultch[21:25])),
        y = c(test7$fit[21:25] - 2*test7$se.fit[21:25], 
              rev(test7$fit[21:25] + 2*test7$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test7$Cultch[26:30], rev(test7$Cultch[26:30])),
        y = c(test7$fit[26:30] - 2*test7$se.fit[26:30], 
              rev(test7$fit[26:30] + 2*test7$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")

#plot for period 8
par(mfrow=c(1,3))
plot(test8$Cultch[1:5], test8$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test8$Cultch[6:10], test8$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test8$Cultch[1:5], rev(test8$Cultch[1:5])),
        y = c(test8$fit[1:5] - 2*test8$se.fit[1:5], 
              rev(test8$fit[1:5] + 2*test8$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test8$Cultch[6:10], rev(test8$Cultch[6:10])),
        y = c(test8$fit[6:10] - 2*test8$se.fit[6:10], 
              rev(test8$fit[6:10] + 2*test8$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test8$Cultch[11:15], test8$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 150), xlab = "Cultch", ylab = "Live Spat")
lines(test8$Cultch[16:20], test8$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test8$Cultch[11:15], rev(test8$Cultch[11:15])),
        y = c(test8$fit[11:15] - 2*test8$se.fit[11:15], 
              rev(test8$fit[11:15] + 2*test8$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test8$Cultch[16:20], rev(test8$Cultch[16:20])),
        y = c(test8$fit[16:20] - 2*test8$se.fit[16:20], 
              rev(test8$fit[16:20] + 2*test8$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test8$Cultch[21:25], test8$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 700), xlab = "Cultch", ylab = "Live Spat")
lines(test8$Cultch[26:30], test8$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test8$Cultch[21:25], rev(test8$Cultch[21:25])),
        y = c(test8$fit[21:25] - 2*test8$se.fit[21:25], 
              rev(test8$fit[21:25] + 2*test8$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test8$Cultch[26:30], rev(test8$Cultch[26:30])),
        y = c(test8$fit[26:30] - 2*test8$se.fit[26:30], 
              rev(test8$fit[26:30] + 2*test8$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")


#plot for period 9
par(mfrow=c(1,3))
plot(test9$Cultch[1:5], test9$fit[1:5], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test9$Cultch[6:10], test9$fit[6:10], lwd = 2, col = 'red')
polygon(x = c(test9$Cultch[1:5], rev(test9$Cultch[1:5])),
        y = c(test9$fit[1:5] - 2*test9$se.fit[1:5], 
              rev(test9$fit[1:5] + 2*test9$se.fit[1:5])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test9$Cultch[6:10], rev(test9$Cultch[6:10])),
        y = c(test9$fit[6:10] - 2*test9$se.fit[6:10], 
              rev(test9$fit[6:10] + 2*test9$se.fit[6:10])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Bulkhead" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Bulkhead")

plot(test9$Cultch[11:15], test9$fit[11:15], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test9$Cultch[16:20], test9$fit[16:20], lwd = 2, col = 'red')
polygon(x = c(test9$Cultch[11:15], rev(test9$Cultch[11:15])),
        y = c(test9$fit[11:15] - 2*test9$se.fit[11:15], 
              rev(test9$fit[11:15] + 2*test9$se.fit[11:15])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test9$Cultch[16:20], rev(test9$Cultch[16:20])),
        y = c(test9$fit[16:20] - 2*test9$se.fit[16:20], 
              rev(test9$fit[16:20] + 2*test9$se.fit[16:20])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Dry Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Dry Bar")

plot(test9$Cultch[21:25], test9$fit[21:25], type = 'l', lwd = 2, ylim = c(0, 800), xlab = "Cultch", ylab = "Live Spat")
lines(test9$Cultch[26:30], test9$fit[26:30], lwd = 2, col = 'red')
polygon(x = c(test9$Cultch[21:25], rev(test9$Cultch[21:25])),
        y = c(test9$fit[21:25] - 2*test9$se.fit[21:25], 
              rev(test9$fit[21:25] + 2*test9$se.fit[21:25])), col = adjustcolor('black', alpha.f=0.10), border = NA)
polygon(x = c(test9$Cultch[26:30], rev(test9$Cultch[26:30])),
        y = c(test9$fit[26:30] - 2*test9$se.fit[26:30], 
              rev(test9$fit[26:30] + 2*test9$se.fit[26:30])), col = adjustcolor('red', alpha.f=0.10), border = NA)
legend("topleft", legend = c("Winter", "Summer"), col = c("black", "red"), lty=1, lwd=2)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Winter"], pch = 16)
points(d5$Cultch[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], d5$LiveSpat[d5$StationName == "NFWF Hotel Bar" & d5$season == "Summer"], pch = 16, col = 'red')
title("NFWF Hotel Bar")


#now sum TotalWt and include that


sum_wt=aggregate(TotalWt~StationName+StationNumber+Cultch+Period+season,data=d3,sum)
names(sum_wt)[6]<-c("Sum_weight")



#or use the mean b/c the offset is applied to the counts, so that's why we use sum
#but offset not applied to weights

mean_wt=aggregate(TotalWt~StationName+StationNumber+Cultch+Period+season,data=d3,mean)
#names(mean_wt)[7]<-c("Mean_weight")

#merge live & tran_length total data frame
d6=merge(d5,sum_wt, by=c("StationName","StationNumber","Cultch","Period", "season"))
d7=merge(d5,mean_wt, by=c("StationName","StationNumber","Cultch","Period", "season"))

as.integer(d7$Mean_weight)

#fit basic NB GLM
m1.1 <- glm.nb(LiveSpat ~ Period + offset(log(Num_quads)), data = d7) 
m2.1 <- glm.nb(LiveSpat ~ Period + StationName + offset(log(Num_quads)), data = d7) 
m3.1 <- glm.nb(LiveSpat ~ Period * StationName + offset(log(Num_quads)), data = d7) 
m4.1 <- glm.nb(LiveSpat ~ Cultch +TotalWt + offset(log(Num_quads)), data = d7) 
m5.1 <- glm.nb(LiveSpat ~ Cultch +TotalWt + Period + offset(log(Num_quads)), data = d7) 
m6.1 <- glm.nb(LiveSpat ~ Cultch +TotalWt + Period + StationName + offset(log(Num_quads)), data = d7) 
m7.1 <- glm.nb(LiveSpat ~ Cultch + TotalWt + Period + StationName + season + offset(log(Num_quads)), data = d7) 


cand.set = list(m1.1,m2.1,m3.1,m5.1,m6.1,m7.1)
modnames = c("year", "year + station", "year * station", "Cultch + TotalWt + year","Cultch + TotalWt + year+ Station", "Cultch + TotalWt + year+ Station+season")
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

summary(m7.1)

