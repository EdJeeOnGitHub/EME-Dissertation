## Event Study Script ##

## Working Directory automatically set by project

## Clearing workspace and any graphics left over
rm(list = ls())
dev.off()

# Libraries
library(rprojroot) # Allows use of relative instead of absolute paths when reading in files
library(dplyr)
library(zoo)
library(readxl)
library(tibble)

## Data transformation and cleaning ##


##### Index Data Cleaning #####

# Reading in file, using projroot library so only relative path needed


root <- has_file(".git/index")
root.file <- root$make_fix_file()
path.index <- root.file('EME' , 'Data' , 'Clean Data' ,'Indices', 'All_indices_cleaned_test.csv')

raw.index.data <- read.csv(path.index)

# Formatting Date column
raw.index.data[, 'Date'] <- as.Date(raw.index.data[, 'Date'])
raw.index.data[, 'X'] <- NULL

# Selecting a subset of the data that only includes UK indices
keep.vars <- c('Date',
               "FTSE.ALL.SHARE...PRICE.INDEX",
               "FTSE.100...PRICE.INDEX",
               "FT.30.ORDINARY.SHARE...PRICE.INDEX",
               "MSCI.UK...PRICE.INDEX",
               "UK...TO.US....WMR....EXCHANGE.RATE")
raw.index.data.UK <- raw.index.data[keep.vars]

# Slicing index data by date
slice.index.by.date <- function(index, start.date, end.date){
  # Subsetting first by start date, then by end date
  sliced.index <- index[ index[, 'Date'] > as.Date(start.date), ]
  sliced.index <- sliced.index[sliced.index[, 'Date'] < as.Date(end.date), ]
  
  return(sliced.index)
}

raw.index.data.UK.80s <- slice.index.by.date(raw.index.data.UK,
                                             start.date = "1979-12-31",
                                             end.date = '1990-01-01')
raw.index.data.UK.90s <- slice.index.by.date(raw.index.data.UK, 
                                             start.date = '1989-12-31',
                                             end.date = '2000-01-01')
raw.index.data.UK.00s <- slice.index.by.date(raw.index.data, 
                                             start.date = '1999-12-31',
                                             end.date = '2010-01-01')
raw.index.data.UK.10s <- slice.index.by.date(raw.index.data.UK, 
                                             start.date = '2009-12-31',
                                             end.date = '2020-01-01')



select.index <- function(multi.index.data, index.to.select, rep = FALSE, n){
  # Currently our index data contains multiple indices - this function selects just one.
  # if rep is set to TRUE it also creates n columns of the selected index, this makes using the 'eventstudies' package easier
  keep.columns <- c( 'Date', index.to.select)
  subset.df <- multi.index.data[, keep.columns]
  
  if (rep == TRUE){
    subset.df <- cbind(subset.df, replicate(n, subset.df[, index.to.select]))
  }
  
  return(subset.df)
}

# Combining the decade index data into a list for ease of use

index.data.UK.decade.list <- list(raw.index.data.UK.80s,
                                  raw.index.data.UK.90s,
                                  raw.index.data.UK.00s,
                                  raw.index.data.UK.10s)

index.data.UK.decade.list.ALLSHARE <- lapply(index.data.UK.decade.list, function(x){select.index(x,
                                                                                                 index.to.select = "FTSE.ALL.SHARE...PRICE.INDEX",
                                                                                                 rep = FALSE)})
# Converting the index list to a zoo format

index.zoo.UK.decade.list.ALLSHARE <- lapply(index.data.UK.decade.list.ALLSHARE, read.zoo)

# Transforming index data from prices to returns and dropping NA values
prices.to.returns <- function(x) 100*diff(log(x))
index.zoo.UK.decade.list.ALLSHARE <- lapply(index.zoo.UK.decade.list.ALLSHARE, prices.to.returns)
index.zoo.UK.decade.list.ALLSHARE.omitted <- lapply(index.zoo.UK.decade.list.ALLSHARE, na.omit)


# Cleaning up unwanted variables
removal.list.index <- c('root',
                 'raw.index.data',
                 'raw.index.data.UK',
                 'raw.index.data.UK.90s',
                 'raw.index.data.UK.80s',
                 'raw.index.data.UK.00s',
                 'raw.index.data.UK.10s',
                 'index.data.UK.decade.list.ALLSHARE',
                 'keep.vars',
                 'path.index',
                 'index.data.UK.decade.list',
                 'removal.list.index')

rm(list = removal.list.index)

#### Terror Event Cleaning ####

path.terror <- root.file('EME', 'Data', 'Clean Data', 'Terror', 'United Kingdom.xls')

raw.terror.data <- read_excel(path.terror)

# Cleaning date function and selecting relevant columns
raw.terror.data$Date <- as.Date(raw.terror.data$Date)
raw.terror.data <- subset(raw.terror.data, select = c(Date,
                                                      nkill,
                                                      nwound,
                                                      propvalue,
                                                      incident))

# Creating the weights with which to calculate terror intensity. These are identical to those used by the GTI
fatality.weight <- 3
incident.weight <- 1
injury.weight <- 0.5

# The property weight takes a variable value depending on the level of property damage
terror.data <- mutate(raw.terror.data, prop.weight = ifelse(propvalue == 0,
                                                                     0,
                                                                     ifelse(propvalue < 1*10^6,
                                                                            1,
                                                                            ifelse(propvalue < 1*10^9,
                                                                                   2,
                                                                                   ifelse(propvalue > 1*10^9,
                                                                                          3, 0)))))
# The weights used by the GTI
terror.data <- mutate(terror.data,
                      terror.intensity = incident*incident.weight +
                        nwound*injury.weight +
                        nkill*fatality.weight +
                        prop.weight)

# Filtering events via decade and top n events measured by terror intensity
filter.event.decade <- function(event.data, start.Date, end.Date, n.events){
  myData <- event.data[ event.data$Date < as.Date(end.Date), ]
  myData <- myData[ myData$Date > as.Date(start.Date),]
  
  to.return <- myData[order( - myData$terror.intensity),]
  to.return <- to.return[1:n.events, ]
  return(to.return)
}

events.80s <- filter.event.decade(event.data = terror.data, start.Date = '1979-12-31', end.Date = '1990-01-01', n.events = 5)
events.90s <- filter.event.decade(event.data = terror.data, start.Date = '1989-12-31', end.Date = '2000-01-01', n.events = 5)
events.00s <- filter.event.decade(event.data = terror.data, start.Date = '1999-12-31', end.Date = '2010-01-01', n.events = 5)
events.10s <- filter.event.decade(event.data = terror.data, start.Date = '2009-12-31', end.Date = '2020-01-01', n.events = 5) 


# Creating a list of the events by decade for ease of use again

events.decade.list <- list(events.80s, events.90s, events.00s, events.10s)

# Removing all the columns in the events dataframe apart from the date of the event
select.date.column <- function(event.data){
  df.dates <- event.data[, 'Date']
  return(df.dates)
}

events.decade.list <- lapply(events.decade.list, select.date.column)

































#### Analysis #####

# To run our analysis we need to remove NA values however this creates a problem when the attacks we want to study occur on the weekend. To overcome this the function
# checks if the event date is missing from the omitted zoo. If it is, it adds one day and checks again, if it's still missing it adds another day and checks again. If a check
# is successful it returns the relevant index number. Essentially we're replacing the row index number of events happening on weekend with the row index of the following
# Monday.
find.NA.index.number <- function(index.data, events, n){
  single.zoo <- index.data
  date.event <- events[[1]][n]
  if (length(which(attributes(na.omit(single.zoo))$index == date.event)) == 0){
    if (length(which(attributes(na.omit(single.zoo))$index == date.event + 1)) == 0){
      
      index.number <- which(attributes(na.omit(single.zoo))$index == date.event + 2)
    } else {
      index.number <- which(attributes(na.omit(single.zoo))$index == date.event + 1)
    }
    
  } else {
    index.number <- which(attributes(na.omit(single.zoo))$index == date.event)
  }
  return(index.number)
}
# t-test calculator. Take the values in the estimation window and find their standard deviation and calculate root N to adjust degrees of freedom
# then divide the CAR given in the event window by the standard error. N.B. we test against the null of CAR = 0 as we don't subtract anything from ev.window
calculate.CAR.t.test <- function(esti.window, ev.window){
  esti.window <- na.omit(esti.window)
  ev.window <- na.omit(ev.window)
  
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  
  test <- ev.window/(estimation.stdev/root.n)
  return(test)
}

# Calculating confidence intervals
calculate.CI <- function(esti.window){
  esti.window <- na.omit(esti.window)
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  ci <- qt(0.975, df = length(esti.window) - 1)*(estimation.stdev/root.n)
  return(ci)
}


# Selecting the estimation window used in the constant mean return model
find.estimation.window <- function(index, events, n, window.end, window.length){
  row.index <- find.NA.index.number(index.data = index,
                                    events = events,
                                    n = n)
  estimation.window.start <- attributes(index[row.index - (window.end + window.length)])$index
  estimation.window.end <- attributes(index[row.index - (window.end + 1)])$index
  estimation.window <- window(index, start = estimation.window.start, end = estimation.window.end)
  return(estimation.window)
}

# Selecting the event window
find.event.window <- function(index, events, n, window.length){
  row.index <- find.NA.index.number(index.data = index,
                                    events = events,
                                    n = n)
  event.window.start <- attributes(index[row.index])$index
  event.window.end <- attributes(index[row.index + window.length])$index
  event.window <- window(index, start = event.window.start, end = event.window.end)
  return(event.window)
}

# Calculating cumulative abnormal returns under the constant mean return model.
calculate.CAR <- function(window, mean.return, car.length){
  ar <- window - mean.return
  car <- rollsum(ar, car.length, fill = NA, align = 'right')
}


####### Testing #########
# esti.window.test <- find.estimation.window(index = index.zoo.UK.decade.list.ALLSHARE.omitted[[1]],
#                                            events = events.decade.list[[1]],
#                                            n = 1,window.end = 10, window.length = 20
# )
# 
# 
# event.window.test <- find.event.window(index = index.zoo.UK.decade.list.ALLSHARE.omitted[[1]],
#                                        events = events.decade.list[[1]],
#                                        n = 1,
#                                        window.length = 10)
# mean.test <- mean(esti.window.test)
# esti.car <- calculate.CAR(esti.window.test, mean.return = mean.test, car.length = 10)
# event.car <- calculate.CAR(event.window.test, mean.test, car.length = 10)
# 
# esti.window.test
# event.window.test
# esti.car
# event.car
# 
# event.t <- calculate.CAR.t.test(esti.window = esti.car,
#                                 ev.window = event.car)
# event.t
# 




# Calculating p value based off t-statistic and estimation window df
calculate.p.value <- function(t.stat, estimation.car){
  p <- p.value <- 2*pt(-abs(t.stat),df=length(na.omit(estimation.car))-1)
  return(p)
}

# Converts event date into market days since attack
calculate.attack.time.delta <- function(event.study){
  event.study$time.delta <- seq(length(event.study[, 1]))
  return(event.study)
}
# Pulling all the above together into a single function for event studies.
perform.one.day.event.study <- function(index, events, n, car.length = 10, estimation.window.length = 20, estimation.window.end = 10){
  
  estimation.window <- find.estimation.window(index = index,
                                              events = events,
                                              n = n,
                                              window.end = estimation.window.end,
                                              window.length = estimation.window.length)
  event.window <- find.event.window(index = index,
                                    events = events,
                                    n = n,
                                    window.length = car.length)
  
  constant.mean.return <- mean(estimation.window)
  
  estimation.car <- calculate.CAR(window = estimation.window,
                                  mean.return = constant.mean.return,
                                  car.length = car.length)
  
  event.car <- calculate.CAR(window = event.window,
                             mean.return = constant.mean.return,
                             car.length = car.length)
  
  event.t.stat <- calculate.CAR.t.test(esti.window = estimation.window,
                                       ev.window = event.window)
  
  event.p.value <- calculate.p.value(t.stat = event.t.stat,
                                     estimation.car = estimation.car)
  
  event.confidence.interval <- calculate.CI(esti.window = estimation.car)
  
  return.zoo <- merge.zoo(event.car,
                          event.t.stat,
                          event.p.value,
                          event.confidence.interval)
  return.df <- data.frame(return.zoo[car.length, ])
  return.df <- rownames_to_column(return.df, 'Date')
  return.df$Date <- as.Date(return.df$Date)
  return(return.df)
}

perform.event.study <- function(index, events, n, car.length = 10, estimation.window.length = 20, estimation.window.end = 10){
  
  full.event.study <- perform.one.day.event.study(index = index,
                                                     events = events,
                                                     n = n,
                                                     car.length = 1,
                                                     estimation.window.length = estimation.window.length,
                                                     estimation.window.end = estimation.window.end)
  for (i in 2:6){
    single.event.study <- perform.one.day.event.study(n = i,
                                                      index = index,
                                                      events = events,
                                                      car.length = car.length,
                                                      estimation.window.length = estimation.window.length,
                                                      estimation.window.end = estimation.window.end)

    full.event.study <- rbind(full.event.study, single.event.study)
  }
  full.event.study <- calculate.attack.time.delta(full.event.study)
  return(full.event.study)
}

event.study.80s.1 <- perform.event.study(index = index.zoo.UK.decade.list.ALLSHARE.omitted[[1]],
                                         events = events.decade.list[[1]],
                                         n = 1)
event.study.80s.1

one.day.es.80s.1.1 <- perform.one.day.event.study(index = index.zoo.UK.decade.list.ALLSHARE.omitted[[1]],
                                                events = events.decade.list[[1]],
                                                car.length = 1,
                                                n = 1)
one.day.es.80s.1.1