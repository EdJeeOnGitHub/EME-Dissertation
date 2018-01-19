# This code loads original data by Bloom (2009) and compares it
# with new data updated by JFV
# Haverford, June 27, 2013

# Package to read xls data
require("gdata")

# Package to make reports
require("knitr")

# Cleaning previous data
rm(list=ls())

# Read original Bloom's data in xls format
var_data <- read.xls("VARDATA.xlsx")

# Read updated Bloom's data in xls format
var_data_updated <- read.xls("VARDATA_UPDATED.xlsx")

# We find the difference between the two datasets for 1962:7-2008:6
# (the months for which Bloom's data is complete), which corresponds to
# the first 552 rows
comparison_data <- var_data_updated[1:552,]-var_data[1:552,]

# IPM
# We need to rescale the IPM for comparison purposes because it changed the 
# base year between Bloom's data and our updated data
ipm_rescaled <-var_data_updated[1:552,3]*var_data[1,3]/var_data_updated[1,3]-var_data[1:552,3]
plot(var_data[1:552,1], ipm_rescaled, col = "blue", xlab = "Month", ylab = "IMP")

# EMPM
plot(var_data[1:552,1], comparison_data[,4], col = "blue", xlab = "Month",ylab = "EMPM")

# HOURSM
plot(var_data[1:552,1], comparison_data[,5], col = "blue", xlab = "Month", ylab = "HOURSM")

# CPI
plot(var_data[1:552,1], comparison_data[,6], col = "blue", xlab = "Month", ylab = "CPI")

# WAGE
plot(var_data[1:552,1], comparison_data[,7], col = "blue", xlab = "Month", ylab = "WAGE")

# FFR
plot(var_data[1:552,1], comparison_data[,8], col = "blue", xlab = "Month", ylab = "FFR")

# STOCK
plot(var_data[1:552,1], comparison_data[,9], col = "blue", xlab = "Month", ylab = "STOCK")

# VOLATBL
plot(var_data[1:552,1], comparison_data[,10], col = "blue", xlab = "Month", ylab = "VOLTABL")