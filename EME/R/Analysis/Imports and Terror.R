# Cleaning Trade data
rm(list = ls())
try(dev.off())

library(readxl)
library(rprojroot)
library(tidyverse)
library(zoo)
library(foreign)

## Import data
root <- has_file(".git/index")
root.file <- root$make_fix_file()
path.import <- root.file('EME' , 'Data' , 'Original Data', 'WITS-Partner-Timeseries-Imports.xlsx')


import.data <- read_excel(path.import, sheet = 2)
colnames(import.data)[2] <- 'country_txt'
head(import.data)
import.data <- gather(import.data, Date, Imports,  '1993' : '2016')



# Terror Data
path.terror <- root.file('EME', 'Data', 'Clean Data', 'Terror', 'global_events.xlsx')
terror.data <- read_excel(path.terror)
terror.data$Date <- as.Date(terror.data$Date)
terror.data <- terror.data[terror.data$Date > as.Date('1993-01-01'),]
head(terror.data)

# US CPI Data
path.CPI <- root.file('EME', 'Data', 'Original Data', 'US CPI.xls')
CPI.data <- read_excel(path.CPI)
CPI.data <- CPI.data %>% 
  separate(observation_date, into = c('Year', 'Month', 'Day'), sep = '-') %>% 
  group_by(Year) %>% 
  mutate(CPI = mean(CPIAUCSL))
CPI.data <- select(CPI.data, -c(Month, Day, CPIAUCSL))
CPI.data <- CPI.data[!duplicated(CPI.data),]
CPI.data

# USD:GBP Data
path.forex <- root.file('EME', 'Data', 'Original Data', 'USD-GBP.xls')
dates <- list( Date = seq(from = as.Date('1993-01-01'), to = as.Date('2017-01-01'), by = 1)) %>% 
  data.frame
  

forex.data <- read_excel(path.forex)
forex.data$Date <- as.Date(forex.data$observation_date, format = '%Y-%m-%d')

forex.data <- left_join(dates, forex.data, 'Date')
forex.data <- forex.data %>% 
  select(- observation_date) %>% 
  na.locf %>% 
  as.tibble

forex.data$`USD GBP` <- as.numeric(forex.data$`USD GBP`)
forex.data$Date <- as.Date(forex.data$Date)






merged.data <- merge(x = terror.data, y = import.data, by.x = c('country_txt', 'iyear'), by.y = c('country_txt', 'Date'), all.x = TRUE)
merged.data <- merged.data %>% 
  merge(  y = CPI.data, by.x = 'iyear', by.y = 'Year', all.x = TRUE) %>% 
  select( - c(X__1, imonth, iday, `Trade Flow`, `eventid`, `Indicator`, `Reporter Name`, `Product Group`)) %>% 
  merge( y = forex.data, by = 'Date', all.x = TRUE) %>%
  as.tibble %>% 
  mutate(real.imports = Imports / (CPI/100),
         real.imports.gbp = real.imports * `USD GBP`)
merged.data

# TODO: Add distance to capitals. Consider re-writing as functions?


path.distance <- root.file('EME', 'Data', 'Original Data', 'geo_cepii.xls')
distance.data <- read_excel(path.distance)
distance.data

















# path.gravity <- root.file('EME', 'Data', 'Original Data', 'gravdata.dta')
# gravity.data <- read.dta(path.gravity)
# colnames(gravity.data)
# 
# uk.gravity <- as.tibble(gravity.data[(gravity.data$iso2_o == 'GB'),])
# uk.gravity