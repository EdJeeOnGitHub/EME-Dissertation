rm(list = ls())

library(readxl)

path.Terror1 <- "C:/Users/Ed/Dropbox/Ed/Ed Uni work/EME/Data/Clean Data/Terror/United Kingdom.xls"
path.Terror2 <- "D:/EME/Data/Clean Data/Terror/United Kingdom.xls"

UK.Terror.Dataset <- read_excel(path.Terror1)
UK.Terror.Dataset$Date <- as.Date(UK.Terror.Dataset$Date)
UK.Terror.Dataset.Subset <- subset(UK.Terror.Dataset, select = c(Date,
                                                                 nkill,
                                                                 nwound,
                                                                 propvalue,
                                                                 incident))
# Creating a terror intensity index to pick the largest events for now
library(dplyr)

# First creating a weighting for property damage which varies with damage intensity identical to the Global Terrorism Index
# methodology

UK.Terror.Dataset.Subset <- mutate(UK.Terror.Dataset.Subset, prop.weight = ifelse(propvalue == 0,
                                                                                  0,
                                                                                  ifelse(propvalue < 1*10^6,
                                                                                         1,
                                                                                         ifelse(propvalue < 1*10^9,
                                                                                                2,
                                                                                                ifelse(propvalue > 1*10^9,
                                                                                                       3, 0)))))
# The weights used by the GTI
fatality.weight <- 3
incident.weight <- 1
injury.weight <- 0.5

# Now creating an intensity column
UK.Terror.Dataset.Subset <- mutate(UK.Terror.Dataset.Subset,
                                   Terror.Intensity = incident*incident.weight +
                                     nwound*injury.weight +
                                     nkill*fatality.weight +
                                     prop.weight)


plot(UK.Terror.Dataset.Subset$Terror.Intensity)

# TI <- UK.Terror.Dataset.Subset$Terror.Intensity
# 
# t.density <- density(log(UK.Terror.Dataset.Subset$Terror.Intensity), bw = 'nrd0',
#         kernel = 'gaussian')
# plot(t.density)
# t.density
# summary(UK.Terror.Dataset.Subset$Terror.Intensity)
# summary(log(UK.Terror.Dataset.Subset$Terror.Intensity))
# 
# library(kedd)
# density2 <- dkde(UK.Terror.Dataset.Subset$Terror.Intensity)
# plot(density2)
# # 
# # density3 <- dkde(UK.Terror.Dataset.Subset$Terror.Intensity, h.bcv)
# # plot(density3)
# 
# Ldensity4 <- dkde(log(UK.Terror.Dataset.Subset$Terror.Intensity))
# plot(Ldensity4)
# 
# density5 <- dkde( TI, kernel = 'uniform')
# plot(density5)
# 
# density6 <- dkde( log(TI), kernel = 'uniform')
# plot(density6)
# 
# density7 <- dkde( TI, kernel = "epanechnikov")
# plot(density7)
# 
# density8 <- dkde( log(TI), kernel = "epanechnikov")
# plot(density8)