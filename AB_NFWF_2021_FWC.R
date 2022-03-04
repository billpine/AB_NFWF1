library(readxl)

SH <- read_excel("AB_Survey_2021.xlsx",sheet = "SH", range = "A4:G4380")

Counts <- read_excel("AB_Survey_2021.xlsx",sheet = "SH", range = "A4:K689")

#now start cleaning

names(SH)

s2 <- SH
str(s2)

names(d4.1)[3] <- "Weight"
names(d4.1)[4] <- "Legal"
names(d4.1)[5] <- "Sublegal"
names(d4.1)[6] <- "Spat"


d2$LiveSpat[d2$LiveSpat < -1] <- NA
d2$TotalWt[d2$TotalWt < -1] <- NA
d2$Drills[d2$Drills < -1] <- NA