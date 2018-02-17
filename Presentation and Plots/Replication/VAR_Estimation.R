# VAR by Bloom (2009) implemented in R

# Package to read xls data
require("gdata")

# Package to run VARs
require("vars")

# Package to run HP filters
require("mFilter")

# Package to make reports
require("knitr")

# Cleaning previous data
rm(list=ls())

# Read xls data
var_data <- read.xls("VARDATA_UPDATED.xlsx")

# Drop last observations in 2013
var_data <- var_data[1:606,]

# Reorder variables
var_datap <- var_data[c(9,10,8,7,6,5,4,3)]

# Stock index
var_datap[,1] <- log(var_datap[,1])
var_aux <- hpfilter(var_datap[,1],freq=129600)
var_datap[,1] <- var_aux$cycle

# Volatility: we define an indicator function on 17 events defined
# by Bloom plus a new one (last Bloom's event has different timing
# with augmented data).
# Check indicator.m to see how we find these dates and the issues 
# with 197 and 305.

var_datap[,2] <- 0

var_datap[4,2]   <- 1 # 1962:10 (1 event)
var_datap[17,2]  <- 1 # 1963:11 (2 event)
var_datap[50,2]  <- 1 # 1966:8 (3 event)
var_datap[95,2]  <- 1 # 1970:5 (4 event)
var_datap[138,2] <- 1 # 1973:12 (5 event)
var_datap[148,2] <- 1 # 1974:10 (6 event)
var_datap[197,2] <- 1 # 1978:11 (7 event) ???
var_datap[213,2] <- 1 # 1980:3 (8 event)
var_datap[244,2] <- 1 # 1982:10 (9 event)
var_datap[305,2] <- 1 # 1987:11 (10 event) ???
var_datap[340,2] <- 1 # 1990:10 (11 event)
var_datap[425,2] <- 1 # 1997:11 (12 event)
var_datap[435,2] <- 1 # 1998:9 (13 event)
var_datap[471,2] <- 1 # 2001:9 (14 event)
var_datap[483,2] <- 1 # 2002:9 (15 event)
var_datap[488,2] <- 1 # 2003:2 (16 event)
var_datap[556,2] <- 1 # 2008:10 (17 event) 
var_datap[591,2] <- 1 # 2011:09 (18 event)

# FFR
var_aux <- hpfilter(var_datap[,3],freq=129600)
var_datap[,3] <- var_aux$cycle

# Wage
var_datap[,4] <- log(var_datap[,4])
var_aux <- hpfilter(var_datap[,4],freq=129600)
var_datap[,4] <- var_aux$cycle

# CPI
var_datap[,5] <- log(var_datap[,5])
var_aux <- hpfilter(var_datap[,5],freq=129600)
var_datap[,5] <- var_aux$cycle

# Hours
var_aux <- hpfilter(var_datap[,6],freq=129600)
var_datap[,6] <- var_aux$cycle

# Employment
var_datap[,7] <- log(var_datap[,7])
var_aux <- hpfilter(var_datap[,7],freq=129600)
var_datap[,7] <- var_aux$cycle

# Industrial Production
var_datap[,8] <- log(var_datap[,8])
var_aux <- hpfilter(var_datap[,8],freq=129600)
var_datap[,8] <- var_aux$cycle

# This flag selects sample size used by Bloom's code (until 2008:6)

flag <- 0
if (flag){
  var_datap <- var_datap[1:552,]
  var_datap[549,2] <- 1 # 2008:3 (17 event)
}

# Check data
View(var_datap)

summary(var_datap)

# VAR Estimation
var_results <- VAR(var_datap,p=12, type="both")

#var_results

#plot(var_results)

var_results.irf <- irf(var_results, response = "IPM", impulse = "VOLATBL", n.ahead = 36, boot = TRUE)

plot(var_results.irf)