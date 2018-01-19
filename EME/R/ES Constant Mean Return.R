###########################
###UK Event study by decade
###########################

rm(list = ls())
dev.off()
#Reading in the file

library(rprojroot)
root <- rprojroot::has_file(".git/index")
root_file <- root$make_fix_file()
path_indices <- root_file('EME' , 'Data' , 'Clean Data' ,'Indices', 'All_indices_cleaned_test.csv')

return.Data <- read.csv(path_indices)

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

# This isn't strictly necessary at this stage but makes the eventstudy package easier to use later

index.chosen <-  "UK...TO.US....WMR....EXCHANGE.RATE"
index.selector <- function(dataframe, n){
  keep <- c( 'Date', index.chosen)
  mydf <- dataframe[, keep, drop = FALSE]
  # mydf <- cbind(mydf, replicate(n, mydf[,index.chosen]))
  return(mydf)
}


UK.Indices.list <- list(UK.1980s, UK.1990s, UK.2000s, UK.2010s)

UK.index.list <- lapply(UK.Indices.list, function(x) {index.selector(x, 5)})


#Converting to 'zoo' format as required by the eventstudies package
library(zoo)

zoo.list <- lapply(UK.index.list, read.zoo)

# Now reading in UK events and formatting
path.Terror <- root_file('EME', 'Data', 'Clean Data', 'Terror', 'United Kingdom.xls')

library(readxl)
UK.Terror.Dataset <- read_excel(path.Terror)
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



# Filtering events via decade and top n events measured by terror intensity
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

events.list <- list(events.80s, events.90s, events.00s, events.10s)


# Only want the column in the above dataframes that have the dates of events
event.dates <- function(data){
  df.dates <- data[, 'Date']
  return(df.dates)
}

event.dates.list <- lapply(events.list, event.dates)

# Now we deal with index data 


# Transforming to returns and selecting only one index
prices2returns <- function(x) 100*diff(log(x))


zoo.return.list <- lapply(zoo.list, prices2returns)

zoo.NA.list <- lapply(zoo.return.list, na.omit)

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

# t-test calculator. Take the values in the estimation window and find their standard deviation and calculate root N to adjust degrees of freedom
# then divide the CAR given in the event window by the standard error. N.B. we test against the null of CAR = 0 as we don't subtract anything from ev.window

test.t.AR.CAR <- function(esti.window, ev.window){
  esti.window <- na.omit(esti.window)
  ev.window <- na.omit(ev.window)
  
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  
  test <- ev.window/(estimation.stdev/root.n)
  return(test)
}

# This function returns the CAR of an event after the specified date. 'index' is a zoo object with returns indexed by date. 'events' is a list of dates whilst
# n refers to the row of the date to be calculated. 'car.length' gives the size of CAR to be calculated - i.e. 10 would mean the 10-day cumulative abnormal return
# The function currently returns the date of the CAR, the AR on that day, the CAR it's t statistic and p value.

# N.B. This function is using the constant mean return model to calculate returns and a hardcoded estimation window of 20 days, starting 30 days before the event
# occurs and ending 10 days before the event occurs. (Or maybe 31 and 11 days - need to check???)


# Needed for the rownames_to_column function used in ES
library(tibble)
library(gtools)

ES <- function(index, events, n, car.length){
  row.index <- NA.index.number(index, events, n)
  date.event <- events[[1]][n]
  
  # Calculating the estimation window
  estimation.window.start <- attributes(index[row.index - 31])$index
  estimation.window.end <- attributes(index[row.index -12])$index
  estimation.window <- window(index, start = estimation.window.start, end = estimation.window.end)
  
  constant.mean.return <- mean(estimation.window)
  estimation.ar <- estimation.window - constant.mean.return
  estimation.car <- rollsum(estimation.ar, car.length, fill = NA, align = 'right')
  
  # Now the event window
  event.window.start <- attributes(index[row.index])$index
  event.window.end <- attributes(index[row.index + 10])$index
  event.window <- window(index, start = event.window.start, end = event.window.end)
  
  
  # Calculating abnormal returns - using the constant mean return model.
  event.ar <- event.window - constant.mean.return
  event.car <- rollsum(event.ar, car.length, fill = NA, align = 'right')
  
  
  t.statistic.CAR <- test.t.AR.CAR(esti.window = estimation.car,
                                   ev.window = event.car)
  p.value <- 2*pt(-abs(t.statistic.CAR),df=length(na.omit(estimation.car))-1)
  
  return.zoo <- merge.zoo(event.ar, event.car, t.statistic.CAR, p.value)
  return.df <- data.frame(return.zoo[ car.length, ])

  return.df <- rownames_to_column(return.df, 'Date')
  return.df$Date <- as.Date(return.df$Date)
  return.df$Stars <- stars.pval(return.df$p.value)
  
  
  return(return.df)
}

# This function essentially repeats the above function to fill up the event window - it's easier this way as we don't have
# to re-calculate the t statistic function for every day in the event window

Full.ES <- function(index, events, n, car.length){
  CAR.event.values <- ES(index, events, n, 1)
  for (i in 2:car.length){
    single.es <- ES(index, events, n, i)
    CAR.event.values <- rbind(CAR.event.values, single.es)
  }
  return(CAR.event.values)
}





# Calculating standard error in order to plot confidence intervals

SE <- function(esti.window){
  esti.window <- na.omit(esti.window)
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  se <- qt(0.975, df = length(esti.window) - 1)*(estimation.stdev/root.n)
  return(se)
}


# This function repeats the ES function for the top 5 dates in a given event list. Essentially finding the CAR for the top 5 events
# per decade. N.B. top 5 events is hardcoded, should use a for loop with length of event.dates.list element TODO....

decade.event.study <- function(index, event, car.length){

  es1 <- ES(index, event, 1, car.length)
  es2 <- ES(index, event, 2, car.length)
  es3 <- ES(index, event, 3, car.length)
  es4 <- ES(index, event, 4, car.length)
  es5 <- ES(index, event, 5, car.length)

  return.df <- rbind(es1,
                     es2,
                     es3,
                     es4,
                     es5)

  return(return.df)

}

CAR.11.80s <- decade.event.study(zoo.NA.list[[1]], event.dates.list[[1]], car.length = 11)


CAR.11.90s <- decade.event.study(zoo.NA.list[[2]], event.dates.list[[2]], car.length = 11)


CAR.11.00s <- decade.event.study(zoo.NA.list[[3]], event.dates.list[[3]], car.length = 11)


CAR.11.10s <- decade.event.study(zoo.NA.list[[4]], event.dates.list[[4]], car.length = 11)


CAR.11.total <- rbind( CAR.11.80s,
                       CAR.11.90s,
                       CAR.11.00s,
                       CAR.11.10s)
# CAR.11.total

CAR.6.80s <- decade.event.study(zoo.NA.list[[1]], event.dates.list[[1]], car.length = 6)
CAR.6.90s <- decade.event.study(zoo.NA.list[[2]], event.dates.list[[2]], car.length = 6)
CAR.6.00s <- decade.event.study(zoo.NA.list[[3]], event.dates.list[[3]], car.length = 6)
CAR.6.10s <- decade.event.study(zoo.NA.list[[4]], event.dates.list[[4]], car.length = 6)

CAR.6.total <- rbind(CAR.6.80s,
                     CAR.6.90s,
                     CAR.6.00s,
                     CAR.6.10s)
# CAR.6.total

# Now using the eventstudies package we aggregate up CARS to give us CAARS

library(eventstudies)


name.function <- function(x){
  ed.list <- c(index.chosen, 2:5)
  x$name <- ed.list
  colnames(x) <- c('when', 'name')
  return(x)
}
# Converting dates into a format the eventstudies package can read

event.dates.list.es <- lapply(event.dates.list, name.function)
event.dates.list.es <- lapply(event.dates.list.es, data.frame)

# Picking one index and then repeating the column 5 times to fit the event study package (Copied from beginning of 
# the script).


index.selector2 <- function(dataframe, n){
  keep <- c( 'Date', index.chosen)
  mydf <- dataframe[, keep, drop = FALSE]
  mydf <- cbind(mydf, replicate(n, mydf[,index.chosen]))
  keep2 <- c('Date', 1:n)
  # mydf <- mydf[, keep2]
  return(mydf)
}



UK.Indices.list.es <- list(UK.1980s, UK.1990s, UK.2000s, UK.2010s)

UK.index.list.es <- lapply(UK.Indices.list.es, function(x) {index.selector2(x, 5)})

zoo.es <- lapply(UK.index.list.es, read.zoo)


zoo.transformed <-
        zoo.es %>%
  lapply(prices2returns) %>%
       lapply(na.omit)
head(zoo.transformed[[1]])



es.80s <- eventstudy(firm.returns = zoo.transformed[[1]],
                  event.list = event.dates.list.es[[1]],
                  event.window = 10,
                  type = 'None',
                  inference = TRUE,
                  inference.strategy = 'wilcox')
plot(es.80s)
es.80s

es.90s <- eventstudy(firm.returns = zoo.transformed[[2]],
                     event.list = event.dates.list.es[[2]],
                     event.window = 10,
                     type = 'None',
                     inference.strategy = 'wilcox')
plot(es.90s)
es.90s

es.00s <- eventstudy(firm.returns = zoo.transformed[[3]],
                     event.list = event.dates.list.es[[3]],
                     event.window = 10,
                     type = 'None',
                     inference.strategy = 'wilcox')
plot(es.00s)
es.00s

es.10s <- eventstudy(firm.returns = zoo.transformed[[4]],
                     event.list = event.dates.list.es[[4]],
                     event.window = 10,
                     type = 'None',
                     inference.strategy = 'wilcox')
plot(es.10s)
es.10s

ES.80s.1 <- ES(index = zoo.NA.list[[1]], events = event.dates.list[[1]], n = 1, car.length = 9)
ES.80s.1
full.80s.1 <- Full.ES(index = zoo.NA.list[[1]], events = event.dates.list[[1]], n = 1, car.length = 10)
full.80s.1
