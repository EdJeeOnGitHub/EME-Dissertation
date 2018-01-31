## Ed Dissertation Script ##

## Working Directory automatically set by project

## Clearing workspace and any graphics left over
rm(list = ls())
try(dev.off(), silent = TRUE)

# Libraries
library(rprojroot) # Allows use of relative instead of absolute paths when reading in files
library(ggplot2) # Plotting
library(dplyr) # Data manipulation
library(tibble) # Nicer data.frames
library(zoo) # Time series manipulation
library(readxl) # Reading in excel
library(ggthemes) # Some extra themes for plotting
library(eventstudies) # Package useful for calculating CAARs
library(knitr) # Presentations
library(KernSmooth) # Local Polynomial fitting
library(locfit) # More local polynomial fitting
library(kedd) # Bandwidth selection for LPR
library(purrr) # Mapping functions


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


# Transforming index data from prices to returns and dropping NA values
prices.to.returns <- function(x) 100*diff(log(x))

# Creating zoo indices that aren't split up into decades

# FTSE ALLSHARE
index.data.UK.ALLSHARE <- select.index(raw.index.data.UK,
                                       index.to.select = "FTSE.ALL.SHARE...PRICE.INDEX")
index.zoo.UK.ALLSHARE.omitted <-
  index.data.UK.ALLSHARE %>%
  read.zoo %>%
  na.omit %>%
  prices.to.returns

# Cleaning up unwanted variables
removal.list.index <- c('root',
                        'raw.index.data',
                        'keep.vars',
                        'path.index',
                        'removal.list.index',
                        'index.data.UK.ALLSHARE')

rm(list = removal.list.index)

#### Terror Event Cleaning ####

path.terror <- root.file('EME', 'Data', 'Clean Data', 'Terror', 'global_events.xlsx')

raw.terror.data <- read_excel(path.terror)


# Cleaning date function and selecting relevant columns
raw.terror.data$Date <- as.Date(raw.terror.data$Date)
raw.terror.data <- subset(raw.terror.data, select = c(Date,
                                                      country,
                                                      country_txt,
                                                      nkill,
                                                      nwound,
                                                      incident))

# Creating the weights with which to calculate terror intensity. These are identical to those used by the GTI
# however, I don't use property damage as it's measured particularly poorly
fatality.weight <- 3
incident.weight <- 1
injury.weight <- 0.5

# The weights used by the GTI
terror.data <- mutate(raw.terror.data,
                      terror.intensity = incident*incident.weight +
                        nwound*injury.weight +
                        nkill*fatality.weight)

# Filtering events via decade and top n events measured by terror intensity
filter.events <- function(event.data, start.Date, end.Date, n.events){
  myData <- event.data[ event.data$Date < as.Date(end.Date), ]
  myData <- myData[ myData$Date > as.Date(start.Date),]
  
  to.return <- myData[order( - myData$terror.intensity),]
  to.return <- to.return[1:n.events, ]
  return(to.return)
}


# using filter.events to sort the terror data and select every observation rather than top n
all.events.sorted <- filter.events(event.data = terror.data, 
                               start.Date = '1970-01-01',
                               end.Date = '2020-01-01',
                               n.events = nrow(terror.data))


# Cleaning up workspace
removal.list.terror <- c('raw.terror.data',
                         'fatality.weight',
                         'incident.weight',
                         'injury.weight',
                         'path.terror',
                         'removal.list.terror')
rm(list = removal.list.terror)

## Analysis Functions ####


# To run our analysis we need to remove NA values from index data (weekends) however this creates a problem when the attacks we want to study occur on the weekend.
# To overcome this the function checks if the event date is missing from the na omitted zoo. If it is, it adds one day and checks again, if it's still missing it adds 
# another day and checks again. If a check is successful it returns the relevant index number. Essentially we're replacing the row index number of events happening
# on weekend with the row index of the following Monday.
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

# Selecting the estimation window used before an attack occurs
find.estimation.window <- function(index, events, n, window.end, window.length){
  row.index <- find.NA.index.number(index.data = index,
                                    events = events,
                                    n = n)
  estimation.window.start <- attributes(index[row.index - (window.end + window.length)])$index
  estimation.window.end <- attributes(index[row.index - (window.end + 1)])$index
  estimation.window <- window(index, start = estimation.window.start, end = estimation.window.end)
  return(estimation.window)
}

 # Calculates the return the day of the terror event
calculate.event.day.return <- function(event.date, n, index){
  
  terror.event.date <- event.date[[n, 'Date']]
  
  
  row.index <- find.NA.index.number(index.data = index,
                                    events = event.date,
                                    n = n)
  
  event.day.return <- index[row.index]
  event.day.return.L1 <- index[(row.index - 1)]
  
  
  return.list <- list(event.day.return, event.day.return.L1, terror.event.date)
  return(return.list)
  
}


# Performs the data transformations in order to calculate conditional probability as laid out by Marc Chesney, Ganna Reshetar, Mustafa Karaman
# at https://doi.org/10.1016/j.jbankfin.2010.07.026

# The indicator function: Y = I(R < r) where R is observed return and r is the return the day of the attack
# The X.temp variable is just R lagged
# The X.L1.conditioned variable is X - r(t-1) i.e X minus the return the day before the terror attack.
# The X.mean.conditioned variable is X - mean(R) i.e. conditioning on the mean of estimation.length observations before the attack

# A regression of Y on X.L1/.mean is equivalent to finding the conditional probability of observing a return as bad or worse on the 
# day of the attack.
calculate.variables <- function(event.day.return.vector, index,  estimation.length = 200){
  
  event.day.return <- event.day.return.vector[[1]]
  event.day.return.L1 <- event.day.return.vector[[2]]
  event.date <- event.day.return.vector[[3]]
  
  estimation.window <- find.estimation.window(index = index,
                                              events = event.date,
                                              n = 1,
                                              window.end = 0,
                                              window.length = estimation.length)
  
  estimation.window.returns <- coredata(estimation.window)
  index.df <- data.frame(estimation.window.returns)
  index.df$event.day.return <-  event.day.return
  index.df$event.day.return.L1 <- event.day.return.L1
  
  index.df <- mutate(index.df, Y = as.numeric(estimation.window.returns < event.day.return))
  index.df$X.temp <- coredata(lag(estimation.window, 1))
  index.df$X.L1.conditioned <- index.df$X.temp - index.df$event.day.return.L1
  
  index.df <- mutate(index.df, X.mean.conditioned = index.df$X.temp - mean(estimation.window.returns) )
  index.df <- na.omit(index.df)
  return(index.df)
}


## Regression data

