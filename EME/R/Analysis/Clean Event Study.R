## Event Study Script ##

## Working Directory automatically set by project

## Clearing workspace and any graphics left over
rm(list = ls())
try(dev.off(), silent = TRUE)

# Libraries
library(rprojroot) # Allows use of relative instead of absolute paths when reading in files
library(tidyverse) # Range of data manipulation packages
library(zoo) # Time series manipulation
library(readxl) # Reading in excel
library(ggthemes) # Some extra themes for plotting
library(eventstudies) # Package useful for calculating CAARs
library(knitr) # Presentations
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

# Creating zoo indices that aren't split up into decades

# FTSE ALLSHARE
index.data.UK.ALLSHARE <- select.index(raw.index.data.UK,
                                       index.to.select = "FTSE.ALL.SHARE...PRICE.INDEX")
index.zoo.UK.ALLSHARE.omitted <-
  index.data.UK.ALLSHARE %>%
  read.zoo %>%
  prices.to.returns %>%
  na.omit


# Creating a zoo index in the eventstudies package format - a zoo with repeated columns of index data
eventstudies.index.ALLSHARE <- select.index(raw.index.data.UK,
                                            index.to.select = "FTSE.ALL.SHARE...PRICE.INDEX",
                                            rep = TRUE,
                                            n = 5)

eventstudies.zoo.ALLSHARE <- 
  eventstudies.index.ALLSHARE %>%
  read.zoo %>%
  prices.to.returns %>%
  na.omit




# FTSE100 eventstudies
eventstudies.index.UK.FTSE100 <- select.index(raw.index.data.UK,
                                      index.to.select = "FTSE.100...PRICE.INDEX",
                                      rep = TRUE,
                                      n = 5)
eventstudies.zoo.UK.FTSE100.omitted <-
  eventstudies.index.UK.FTSE100 %>%
  read.zoo %>%
  prices.to.returns %>%
  na.omit
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
                 'removal.list.index',
                 # 'index.data.UK.ALLSHARE',
                 'index.zoo.UK.decade.list.ALLSHARE',
                 'eventstudies.index.ALLSHARE',
                 'eventstudies.index.UK.FTSE100')

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
filter.events <- function(event.data, start.Date, end.Date, n.events){
  myData <- event.data[ event.data$Date < as.Date(end.Date), ]
  myData <- myData[ myData$Date > as.Date(start.Date),]
  
  to.return <- myData[order( - myData$terror.intensity),]
  to.return <- to.return[1:n.events, ]
  return(to.return)
}

events.80s <- filter.events(event.data = terror.data, start.Date = '1979-12-31', end.Date = '1990-01-01', n.events = 5)
events.90s <- filter.events(event.data = terror.data, start.Date = '1989-12-31', end.Date = '2000-01-01', n.events = 5)
events.00s <- filter.events(event.data = terror.data, start.Date = '1999-12-31', end.Date = '2010-01-01', n.events = 5)
events.10s <- filter.events(event.data = terror.data, start.Date = '2009-12-31', end.Date = '2020-01-01', n.events = 5) 


# Creating a list of the events by decade for ease of use again

events.decade.list <- list(events.80s, events.90s, events.00s, events.10s)

# Removing all the columns in the events dataframe apart from the date of the event
select.date.column <- function(event.data){
  df.dates <- event.data[, 'Date']
  return(df.dates)
}

events.decade.list <- lapply(events.decade.list, select.date.column)



# Creating a list with the five largest events ever recorded in the UK
events.top5 <- filter.events(event.data = terror.data, start.Date = '1980-01-01', end.Date = '2020-01-01', n.events = 5)


events.top5$event.name <- c('Lockerbie', 'London 7/7', 'Omagh', '1996 Manchester', 'Droppin Well')
events.top5$event.name <- factor(events.top5$event.name, levels = events.top5$event.name[order(events.top5$terror.intensity)])

# using filter.events to sort the terror data and select every observation rather than top n
events.sorted <- filter.events(event.data = terror.data, 
                               start.Date = '1970-01-01',
                               end.Date = '2020-01-01',
                               n.events = nrow(terror.data))








removal.list.terror <- c('raw.terror.data',
                         'fatality.weight',
                         'incident.weight',
                         'injury.weight',
                         'path.terror',
                         'removal.list.terror')
rm(list = removal.list.terror)






#### eventstudies Package Cleaning ####

## Data transformation for the eventstudies package - namely creating a 'when' and 'name' column that 'eventstudies' package can read

create.name.when.columns <- function(event.data, index.selected, n){
  name.list <- c(index.selected, 2:n)
  event.data$name <- name.list
  event.data <- data.frame(event.data)
  colnames(event.data) <- c('when', 'name')
  return(event.data)
}

# Applying to largest events

eventstudies.events.top5 <- 
  events.top5 %>%
  select.date.column %>%
  create.name.when.columns(index.selected = "FTSE.100...PRICE.INDEX",
                           n = 5)





#### Analysis Functions #####

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

# Calculate AR
calculate.AR <- function(window, mean.return){
  ar <- window - mean.return
  return(ar)
}


# Calculating cumulative abnormal returns under the constant mean return model.
calculate.CAR <- function(window, mean.return, car.length){
  ar <- window - mean.return
  car <- rollsum(ar, car.length, fill = NA, align = 'right')
}


# Calculating p value based off t-statistic and estimation window df
calculate.p.value <- function(t.stat, estimation.car){
  p <- p.value <- 2*pt(-abs(t.stat),df=length(na.omit(estimation.car))-1)
  return(p)
}

# Converts event date into market days since attack
calculate.attack.time.delta <- function(event.study){
  event.study$time.delta <- seq(length(event.study[, 1])) - 1
  return(event.study)
}
# Pulling all the above together into a single function to run an event study. This will give an n-day CAR observation only.
perform.one.day.event.study <- function(index, events, n, car.length = 11, estimation.window.length = 20, estimation.window.end = 10){
  
  event.market.date <- events$Date[n]
  
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
  
  event.ar <- calculate.AR(window = event.window,
                           mean.return = constant.mean.return)
  
  event.car <- calculate.CAR(window = event.window,
                             mean.return = constant.mean.return,
                             car.length = car.length)
  
  event.t.stat <- calculate.CAR.t.test(esti.window = estimation.car,
                                       ev.window = event.car)
  
  event.p.value <- calculate.p.value(t.stat = event.t.stat,
                                     estimation.car = estimation.car)
  
  event.confidence.interval <- calculate.CI(esti.window = estimation.car)
  
  return.zoo <- merge.zoo(event.ar,
                          event.car,
                          event.t.stat,
                          event.p.value,
                          event.confidence.interval)
  return.df <- data.frame(return.zoo[car.length, ])
  return.df <- rownames_to_column(return.df, 'Date')
  return.df$Date <- as.Date(return.df$Date)
  return.df$stars <- gtools::stars.pval(return.df$event.p.value)
  return.df$market.date <- event.market.date
  return(return.df)
}

# This will run the one day event study function n times to fill up an n-day event window
perform.event.study <- function(index, events, n, car.length = 11, estimation.window.length = 20, estimation.window.end = 10){
  
  full.event.study <- perform.one.day.event.study(index = index,
                                                     events = events,
                                                     n = n,
                                                     car.length = 1,
                                                     estimation.window.length = estimation.window.length,
                                                     estimation.window.end = estimation.window.end)
  for (i in 2:car.length){
    single.event.study <- perform.one.day.event.study(n = n,
                                                      index = index,
                                                      events = events,
                                                      car.length = i,
                                                      estimation.window.length = estimation.window.length,
                                                      estimation.window.end = estimation.window.end)

    full.event.study <- rbind(full.event.study, single.event.study)
  }
  full.event.study <- calculate.attack.time.delta(full.event.study)
  return(full.event.study)
}

# Function performs a one.day event study for the largest n events grouped by decade
perform.decade.event.study <- function(index, events, car.length, estimation.window.length = 20, estimation.window.end = 10){
  n.events <- nrow(events)
  decade.event.study <- perform.one.day.event.study(index = index,
                                                    events = events,
                                                    n = 1)
  for (i in 2:n.events){
    event.study <- perform.one.day.event.study(index = index,
                                               events = events,
                                               n = i)
    decade.event.study <- rbind(decade.event.study, event.study)
  }
  return(decade.event.study)
}

# This function takes every event given in events and calculates the appropriate CAR
calculate.every.CAR <- function(events, index, estimation.window.length = 20, estimation.window.end = 10, car.length = 11){
  
  all.events.CAR <- perform.one.day.event.study(index = index,
                                                events = events,
                                                n = 1, 
                                                estimation.window.length = estimation.window.length,
                                                estimation.window.end = estimation.window.end,
                                                car.length = car.length)
  for (i in 2:nrow(events)){
    temp.event.CAR <- perform.one.day.event.study(n = i,
                                                  index = index,
                                                  events = events,
                                                  estimation.window.length = estimation.window.length,
                                                  estimation.window.end = estimation.window.end,
                                                  car.length = car.length)
    all.events.CAR <- rbind(all.events.CAR, temp.event.CAR)
  }
  return(all.events.CAR)
  
}

# Now gives rolling Cumulative Average Abnormal Returns
calculate.rolling.CAAR <- function(all.events.CAR){
  all.events.CAR$rolling.CAAR <- cumsum(all.events.CAR$event.car)/seq(along = all.events.CAR$event.car)
  n <- c(1:nrow(all.events.CAR))
  all.events.CAR$n <- n
  return(all.events.CAR)
}


calculate.CI.rolling.CAAR <- function(all.events.CAR){
 all.events.CAR$df <- all.events.CAR$n - 1
 all.events.CAR <- mutate(all.events.CAR, 
                          temp.diff = (event.car - rolling.CAAR)^2,
                          rolling.sd = sqrt(cumsum(temp.diff)/df),
                          rolling.ci = qt(0.975, df = df)*rolling.sd/(sqrt(n)))
 # all.events.CAR$temp.diff <- (all.events.CAR$event.CAR - all.events.CAR$rolling.CAAR)^2
 # # all.events.CAR$rolling.sd <- sqrt(cumsum(all.events.CAR$temp.diff)/all.events.CAR$df)
 # # all.events.CAR$ci <- qt(0.975, df = df)*(all.events.CAR$rolling.sd/(sqrt(all.events.CAR$n)))
 return(all.events.CAR)                                         
}



#### Decade Results ####

# CAR10
decade.event.study.80s.CAR10 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                     events = events.80s)
decade.event.study.90s.CAR10 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                     events = events.90s)
decade.event.study.00s.CAR10 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                     events = events.00s)
decade.event.study.10s.CAR10 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                     events = events.10s)
decade.event.study.CAR10 <- rbind(decade.event.study.80s.CAR10,
                            decade.event.study.90s.CAR10,
                            decade.event.study.00s.CAR10,
                            decade.event.study.10s.CAR10)

# CAR4 N.B. car.length set to 5 as it counts event day (i.e. t=0) inclusively
decade.event.study.80s.CAR4 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                          events = events.80s,
                                                          car.length = 5)
decade.event.study.90s.CAR4 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                          events = events.90s,
                                                          car.length = 5)
decade.event.study.00s.CAR4 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                          events = events.00s,
                                                          car.length = 5)
decade.event.study.10s.CAR4 <- perform.decade.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                          events = events.10s,
                                                          car.length = 5)
decade.event.study.CAR4 <- rbind(decade.event.study.80s.CAR4,
                                 decade.event.study.90s.CAR4,
                                 decade.event.study.00s.CAR4,
                                 decade.event.study.10s.CAR4)

decade.event.study.CAR10
decade.event.study.CAR4

removal.list.decade <- c('decade.event.study.80s.CAR10',
                         'decade.event.study.90s.CAR10',
                         'decade.event.study.10s.CAR10',
                         'decade.event.study.00s.CAR10',
                         'decade.event.study.80s.CAR4',
                         'decade.event.study.90s.CAR4',
                         'decade.event.study.00s.CAR4',
                         'decade.event.study.10s.CAR4',
                         'removal.list.decade')

rm(list = removal.list.decade)

#### Largest Event Results ####
lockerbie.bombing.event.study <- perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                     events = events.top5,
                                                     n = 1)
london.7.7.bombing.event.study <- perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                      events = events.top5,
                                                      n = 2)
omagh.bombing.event.study <- perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                 events = events.top5,
                                                 n = 3)
manchester.bombing.1996.event.study <- perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                           events = events.top5,
                                                           n = 4)
droppin.well.bombing.event.study <- perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                                                        events = events.top5,
                                                        n = 5)
lockerbie.bombing.event.study
london.7.7.bombing.event.study
omagh.bombing.event.study
manchester.bombing.1996.event.study
droppin.well.bombing.event.study

# Calculating rolling CAAR of the largest n events
all.CAR.10.day.ALLSHARE <- calculate.every.CAR(events = events.sorted,
                                               index = index.zoo.UK.ALLSHARE.omitted)

all.CAR.10.day.ALLSHARE <- calculate.rolling.CAAR(all.CAR.10.day.ALLSHARE)

all.CAR.10.day.ALLSHARE <- calculate.CI.rolling.CAAR(all.CAR.10.day.ALLSHARE)

#### eventstudies Package Results ####

####
#### Need to come back to this with individual industry/firm level data rather than index level data.
####



# eventstudies.top5.constant.mean.return <- eventstudy(firm.returns = eventstudies.zoo.UK.FTSE100.omitted,
#                                 event.list = eventstudies.events.top5,
#                                 type = 'marketModel',
#                                 inference = TRUE,
#                                 inference.strategy = 'bootstrap',
#                                 model.args = list(market.returns = eventstudies.zoo.ALLSHARE[, '2']))
# 
# 
# eventstudies.top5.none <- eventstudy(firm.returns = eventstudies.zoo.ALLSHARE,
#                                      event.list = eventstudies.events.top5,
#                                      type = 'None',
#                                      inference.strategy = 'classic')
# plot(eventstudies.top5.constant.mean.return)
# plot(eventstudies.top5.none)




#### Summary Statistics Graphics ####

## Histograms ##


histogram.wounded.small <- ggplot(terror.data[(terror.data$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE) +
  xlim(0, 100) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016', subtitle = 'xlim(0, 100)') +
  theme_minimal()

histogram.wounded.large <- ggplot(terror.data[(terror.data$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016') +
  theme_minimal()

histogram.killed.at.least.1 <- ggplot(terror.data[(terror.data$nkill > 0), ], aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities | at least one person is killed') +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()

histogram.killed <- ggplot(terror.data, aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities') +
  annotate('text', x = 150, y = 1570, label = 'N.B. scale has doubled', colour = 'orange', size = 8) +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()

histogram.prop.damage.at.least.1 <- ggplot(terror.data[(terror.data$propvalue > 0), ], aes(propvalue)) +
  geom_histogram(show.legend = FALSE) +
  xlab('Recorded Property Damage | Property Damage > 0') +
  ggtitle('Property Damage from UK Terror Attacks, 1970-2016') +
  annotate('text', x = 2.5*10^9, y = 20, label = '1992 Manchester \n Bombing', colour = 'red') +
  annotate('text', x = 1.2*10^9, y = 15, label = '1996 Manchester Bombing', colour = 'red') +
  theme_minimal()

histogram.terror.intensity <- ggplot(terror.data, aes(log(terror.intensity), fill = cut(log(terror.intensity), 1000))) +
  geom_histogram(show.legend = FALSE) +
  ggtitle('Log(Terror Intensity) from 1983-2016 in the UK') +
  theme_minimal()


bar.5.largest.events <- ggplot(events.top5, aes(event.name, terror.intensity)) +
  geom_col(aes(event.name, terror.intensity, fill = -terror.intensity), show.legend = FALSE) +
  xlab('Event') +
  ylab('Terror Intensity') +
  ggtitle('Terror Intensity, \nTop 5 Events') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y = element_blank()) 


## Scatter Graphs ##

scatter.fatalities.over.time <- ggplot(terror.data, aes(Date, nkill, colour = cut(nkill, 10000))) +
  geom_point(size = 1, show.legend = FALSE, aes(size = terror.intensity)) +
  xlab('Year of Attack') +
  ylab('Number of Fatalities') +
  ggtitle('Deaths Attributed to Terror in the UK, 1970-2016') +
  theme_minimal()

scatter.log.fatalities.over.time <- ggplot(terror.data, aes(Date, nkill, colour = cut(nkill, 10000))) +
  geom_point( aes(size = terror.intensity), show.legend = FALSE) +
  scale_y_log10() +
  xlab('Year of Attack') +
  ylab('Number of Fatalities, \n logarithmic scale') +
  ggtitle('Deaths Attributed to Terror in the UK, 1970-2016') + 
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

scatter.wounded.over.time <- ggplot(terror.data, aes(Date, nwound, Terror.intensity,  colour = cut(nwound, 100))) +
  geom_point(show.legend = FALSE, aes(size = terror.intensity)) +
  ylab('Number of wounded') +
  xlab('Year of Attack') +
  ggtitle('Injuries Attributed to Terror in the UK, 1970-2016') + 
  theme_minimal()

scatter.log.wounded.over.time <- ggplot(terror.data, aes(Date, log(nwound), colour = cut(nwound, 100))) +
  geom_point(show.legend = FALSE, aes(size = terror.intensity)) +
  ylab('Log Number of wounded') +
  xlab('Year of Attack') +
  ggtitle('Injuries Attributed to Terror in the UK, 1970-2016') + 
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

scatter.terror.intensity.over.time <- ggplot(terror.data, aes(Date, terror.intensity, colour = cut(terror.intensity, 100))) +
  geom_point(show.legend = FALSE) +
  ylab('Terror Intensity') +
  xlab('Year of Attack') +
  ggtitle('Terror Intensity, UK 1970-2016') + 
  theme_minimal()

scatter.log.terror.intensity.over.time <- ggplot(terror.data, aes(as.Date(Date), log(terror.intensity), colour = cut(log(terror.intensity), 100))) +
  geom_point(show.legend = FALSE) +
  ylab('Log Terror Intensity') +
  xlab('Year of Attack') +
  ggtitle('Terror Intensity, UK 1970-2016') +
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

## Index Returns Plots

line.ALLSHARE.time <- ggplot(na.omit(index.data.UK.ALLSHARE), aes(Date, FTSE.ALL.SHARE...PRICE.INDEX)) +
  geom_line() +
  geom_vline(data = events.top5, aes(xintercept = Date), linetype = 'dotted') +
  ylab('FTSE All-Share Price') +
  xlab('Time') +
  ggtitle('FTSE All-Share Price', subtitle = 'Five largest terror attacks shown') +
  theme_minimal()






#### Largest Events Graphics ####

## @knitr lockerbie.plot
lockerbie.plot <- ggplot(lockerbie.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Lockerbie Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 21 December 1988') +
  ylim(-3, 4) +
  theme_minimal()

## @knitr london.7.7.plot
london.7.7.plot <-ggplot(london.7.7.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('London 7/7 Bombings, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 7 July 2005') +
  ylim(-3, 3.5) +
  theme_minimal() 

## @knitr omagh.plot
omagh.plot <- ggplot(omagh.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Omagh Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 15 August 1998') +
  theme_minimal()

## @knitr manchester.plot
manchester.plot <- ggplot(manchester.bombing.1996.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('1996 Manchester Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 15 June 1996') +
  ylim(-3, 3) +
  theme_minimal()

## @knitr droppin.well.plot
droppin.well.plot <- ggplot(droppin.well.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Droppin Well Disco Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 6 December 1982') +
  ylim(-5, 3) +
  theme_minimal()

# lockerbie.plot
# london.7.7.plot
# omagh.plot
# manchester.plot
# droppin.well.plot


## @knitr rolling.CAAR.plot
# Now graphing rolling CAAR
rolling.CAAR.plot <- ggplot(all.CAR.10.day.ALLSHARE, aes(n, rolling.CAAR))+
  geom_point(shape = 16, size = 1, colour = "#fdafee", show.legend = FALSE) +
  geom_line(aes(n, rolling.CAAR - rolling.ci), alpha = 0.3, linetype = 'longdash') +
  geom_line(aes(n, rolling.CAAR + rolling.ci), alpha = 0.3, linetype = 'longdash') +
  ylim(-5, 5) +
  xlab('Largest N attacks') +
  ylab('Rolling Cumulative Average Abnormal Return (%)') +
  ggtitle('Rolling mean of Cumulative Abnormal Returns', subtitle = 'UK Terror Attacks with FTSE ALLSHARE data, 1980-2016') +
  theme_minimal()

#### Decade Graphics ####


# Need to recode some variables as factors for plot to be ordered by event.car size
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date)
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date,
                                        levels = decade.event.study.CAR10$market.date[order(-decade.event.study.CAR10$event.car)])
## @knitr bar.chart.decade.event.study.by.CAR
bar.chart.decade.event.study.by.CAR <- ggplot(decade.event.study.CAR10, aes(market.date, event.car, event.p.value, alpha = -event.p.value)) +
  geom_col(fill = 'red') +
  coord_flip() +
  ylab('10 Day Cumulative Abnormal Return') +
  xlab('Market Date of Attack') + 
  theme_minimal() +
  ggtitle('10 Day Cumulative Abnormal Returns in Response to Terror Event', subtitle = 'Top 5 Events per Decade') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')
  



# This time ordering temporally
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date)
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date,
                                               levels = decade.event.study.CAR10$market.date[order(decade.event.study.CAR10$Date)])
## @knitr bar.chart.decade.event.study.by.time
bar.chart.decade.event.study.by.time <- ggplot(decade.event.study.CAR10, aes(market.date, event.car, event.p.value, alpha = -event.p.value)) +
  geom_col(fill = 'red') +
  ylab('10 Day Cumulative Abnormal Return') +
  xlab('Market Date of Attack') + 
  theme_minimal() +
  ggtitle('10 Day Cumulative Abnormal Returns in Response to Terror Event', subtitle = 'Top 5 Events per Decade') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none',
        axis.text.x = element_text(angle = 270, hjust = 1))


