###########################
###UK Event study by decade
###########################

rm(list = ls())

#Reading in the file
library(readxl)
path1 <- "C:/Users/Ed/Dropbox/Ed/Ed Uni work/EME/Data/Clean Data/Indices/All_indices_cleaned_test.csv"
path2 <- "D:/EME/Data/Clean Data/Indices/All_indices_cleaned_test.csv"
return.Data <- read.csv(path1)

#Cleaning Date and index column
return.Data$Date <-as.Date(return.Data$Date)
return.Data$X <- NULL

#UK data only
keep.Vars <- c('Date',
               "FTSE.ALL.SHARE...PRICE.INDEX",
               "FTSE.100...PRICE.INDEX",
               "FT.30.ORDINARY.SHARE...PRICE.INDEX",
               "MSCI.UK...PRICE.INDEX",
               "UK...TO.US....WMR....EXCHANGE.RATE")
UK.Index.Data <- return.Data[keep.Vars]

# 1980s
UK.1980s <- UK.Index.Data[ UK.Index.Data$Date > as.Date("1979-12-31"), ]
UK.1980s <- UK.1980s[UK.1980s$Date < as.Date("1990-01-01"),]

# 1990s
UK.1990s <- UK.Index.Data[ UK.Index.Data$Date > as.Date("1989-12-31"), ]
UK.1990s <- UK.1990s[ UK.1990s$Date < as.Date('2000-01-01'),]

# 2000s
UK.2000s <- UK.Index.Data[ UK.Index.Data$Date > as.Date("1999-12-31"),]
UK.2000s <- UK.2000s[ UK.2000s$Date < as.Date("2010-01-01"),]

#2010s
UK.2010s <- UK.Index.Data[ UK.Index.Data$Date > as.Date("2009-12-31"),]
UK.2010s <- UK.2010s[ UK.2010s$Date < as.Date("2020-01-01"),]

# Picking one index and then repeating the column 5 times to fit the event study package

index.chosen <-  "FTSE.ALL.SHARE...PRICE.INDEX"
index.selector <- function(dataframe, n){
  keep <- c( 'Date', index.chosen)
  mydf <- dataframe[, keep, drop = FALSE]
  # mydf <- cbind(mydf, replicate(n, mydf[,index.chosen]))
  return(mydf)
}

UK.index.1980s <- index.selector(UK.1980s, 5)
UK.index.1990s <- index.selector(UK.1990s, 5)
UK.index.2000s <- index.selector(UK.2000s, 5)
UK.index.2010s <- index.selector(UK.2010s, 5)




#Converting to 'zoo' format as required by the eventstudies package
library(zoo)

zoo.1980 <- read.zoo(UK.index.1980s)
zoo.1990 <- read.zoo(UK.index.1990s)
zoo.2000 <- read.zoo(UK.index.2000s)
zoo.2010 <- read.zoo(UK.index.2010s)

zoo.list <- lapply(UK.index.list, read.zoo)

# Now reading in UK events and formatting
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


decade.Event.Filter <- function(data, start.Date, end.Date, n.events){
  myData <- data[ data$Date < as.Date(end.Date), ]
  myData <- myData[ myData$Date > as.Date(start.Date),]
  
  to.return <- myData[order( - myData$Terror.Intensity),]
  to.return <- to.return[1:n.events, ]
  return(to.return)
  
}

events.80s <- decade.Event.Filter(data = UK.Terror.Dataset.Subset, start.Date = '1979-12-31', end.Date = '1990-01-01', n.events = 5)
events.90s <- decade.Event.Filter(data = UK.Terror.Dataset.Subset, start.Date = '1989-12-31', end.Date = '2000-01-01', n.events = 5)
events.00s <- decade.Event.Filter(data = UK.Terror.Dataset.Subset, start.Date = '1999-12-31', end.Date = '2010-01-01', n.events = 5)
events.10s <- decade.Event.Filter(data = UK.Terror.Dataset.Subset, start.Date = '2009-12-31', end.Date = '2020-01-01', n.events = 5) 


 
# Only want the column in the above dataframes that have the dates of events
event.dates <- function(data){
  df.dates <- data[, 'Date']
  return(df.dates)
}

event.dates.80s <- event.dates(events.80s)
event.dates.90s <- event.dates(events.90s)
event.dates.00s <- event.dates(events.00s)
event.dates.10s <- event.dates(events.10s)





# Now we deal with index data 


# Transforming to returns and selecting only one index
prices2returns <- function(x) 100*diff(log(x))

zoo.return.1980 <- prices2returns(zoo.1980)
zoo.return.1990 <- prices2returns(zoo.1990)
zoo.return.2000 <- prices2returns(zoo.2000)
zoo.return.2010 <- prices2returns(zoo.2010)

zoo.NA.1980 <- na.omit(zoo.return.1980)
zoo.NA.1990 <- na.omit(zoo.return.1990)
zoo.NA.2000 <- na.omit(zoo.return.2000)
zoo.NA.2010 <- na.omit(zoo.return.2010)

# A function to find the corresponding event row index for the zoo with no NA values.
# The function checks if the date is missing for an event then +/- a day to get the
# nearest weekday index instead, otherwise just the row index is returned.


  
NA.index.number <- function(data, event, n){
  single.zoo <- data
  date.event <- event[[1]][n]
  if (length(which(attributes(na.omit(single.zoo))$index == date.event)) == 0){
    if (length(which(attributes(na.omit(single.zoo))$index == date.event + 1)) == 0){
      
      no <- which(attributes(na.omit(single.zoo))$index == date.event - 1)
    } else {
      no <- which(attributes(na.omit(single.zoo))$index == date.event + 1)
    }
    
  } else {
    no <- which(attributes(na.omit(single.zoo))$index == date.event)
  }
  return(no)
}

test.t.AR.CAR <- function(esti.window, ev.window){
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  
  test <- ev.window/(estimation.stdev/root.n)
  return(test)
}
library(tibble)
library(gtools)
ES <- function(index, events, n){
  row.index <- NA.index.number(index, events, n)
  
  
  # Calculating the estimation window
  estimation.window.start <- attributes(index[row.index - 31])$index
  estimation.window.end <- attributes(index[row.index -12])$index
  estimation.window <- window(index, start = estimation.window.start, end = estimation.window.end)
  
  constant.mean.return <- mean(estimation.window)
  estimation.ar <- estimation.window - constant.mean.return
  estimation.car <- cumsum(estimation.ar)
  
  # Now the event window
  event.window.start <- attributes(index[row.index])$index
  event.window.end <- attributes(index[row.index + 10])$index
  event.window <- window(index, start = event.window.start, end = event.window.end)
  
  
  # Calculating abnormal returns - using the constant mean return model.
  event.ar <- event.window - constant.mean.return
  event.car <- cumsum(event.ar)
  
  
  
  t.statistic.CAR <- test.t.AR.CAR(esti.window = estimation.car,
                ev.window = event.car)
  p.value <- 2*pt(-abs(t.statistic.CAR),df=length(estimation.window)-1)
  
  return.zoo <- merge.zoo(event.ar, event.car, t.statistic.CAR, p.value)
  return.df <- data.frame(return.zoo)
  return.df <- rownames_to_column(return.df, 'Date')
  return.df$Date <- as.Date(return.df$Date)
  return.df$Stars <- stars.pval(return.df$p.value)
  list.return <- list('CAR' = event.car,'AR' = event.ar)
  return(return.df)
}






# Calculating standard error in order to plot confidence intervals

SE <- function(esti.window){
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  se <- qt(0.975, df = length(esti.window) - 1)*(estimation.stdev/root.n)
  return(se)
}


doggy1 <- ES(zoo.NA.1980, event.dates.80s, 1)
doggy2 <- ES(zoo.NA.1980, event.dates.80s, 2)
doggy3 <- ES(zoo.NA.1980, event.dates.80s, 3)
doggy4 <- ES(zoo.NA.1980, event.dates.80s, 4)
doggy5 <- ES(zoo.NA.1980, event.dates.80s, 5)

decade.event.study <- function(index, event, car.length){
  
  es1 <- ES(index, event, 1)
  es2 <- ES(index, event, 2)
  es3 <- ES(index, event, 3)
  es4 <- ES(index, event, 4)
  es5 <- ES(index, event, 5)
  
  decade.es1 <- es1[car.length, ]
  decade.es2 <- es2[car.length, ]
  decade.es3 <- es3[car.length, ]
  decade.es4 <- es4[car.length, ]
  decade.es5 <- es5[car.length, ]
  
  
  return.df <- rbind( decade.es1,
                           decade.es2,
                           decade.es3,
                           decade.es4,
                           decade.es5)
  return(return.df)
  
}




# 
# options(scipen = 999)
# 
# doggy1
# doggy2
# doggy3
# doggy4
# doggy5
# 

CAR.10.80s <- decade.event.study( zoo.NA.1980, event.dates.80s, car.length = 10)
print(CAR.10.80s)

CAR.10.90s <- decade.event.study(zoo.NA.1990, event.dates.90s, car.length = 10)
print(CAR.10.90s)

CAR.10.00s <- decade.event.study(zoo.NA.2000, event.dates.00s, car.length = 10)
print(CAR.10.00s)

CAR.10.10s <- decade.event.study(zoo.NA.2010, event.dates.10s, car.length = 10)
print(CAR.10.10s)

CAR.10.total <- rbind( CAR.10.80s,
                       CAR.10.90s,
                       CAR.10.00s,
                       CAR.10.10s)
CAR.10.total






CAR.6.80s <- decade.event.study(zoo.NA.1980, event.dates.80s, car.length = 6)
CAR.6.90s <- decade.event.study(zoo.NA.1990, event.dates.90s, car.length = 6)
CAR.6.00s <- decade.event.study(zoo.NA.2000, event.dates.00s, car.length = 6)
CAR.6.10s <- decade.event.study(zoo.NA.2010, event.dates.10s, car.length = 6)

CAR.6.total <- rbind(CAR.6.80s,
                     CAR.6.90s,
                     CAR.6.00s,
                     CAR.6.10s)
CAR.6.total