# Cleaning Trade data
rm(list = ls())
try(dev.off())

library(readxl)
library(rprojroot)
library(tidyverse)



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


merged.data <- merge(x = terror.data, y = import.data, by.x = c('country_txt', 'iyear'), by.y = c('country_txt', 'Date'), all.x = TRUE)
merged.data <- as.tibble(merged.data)
merged.data


