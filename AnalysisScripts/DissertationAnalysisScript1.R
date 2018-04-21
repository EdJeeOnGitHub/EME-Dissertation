## Event Study Script ##

## Working Directory automatically set by project

## Clearing workspace and any graphics left over
rm(list = ls())
try(dev.off(), silent = TRUE)

# Libraries
library(tidyverse) # Data manipulation
library(zoo) # Time series manipulation
library(readxl) # Reading in excel
library(ggthemes) # Some extra themes for plotting
library(lubridate) # Date manipulation
library(rstan) # Bayesian package
library(shinystan) # Bayesian model exploration
library(boot) # Bootstrapping library
library(dynlm) # Time series regression
library(broom) # Regression output
# Clearing up masking problems where tseries::filter and dplyr::filter share the same namespace
filter <- dplyr::filter
# Stan uses max number of cores when running MCMC simulations
options(mc.cores = parallel::detectCores())
# Loading analysis functions
source('AnalysisScripts/DissertationFunctions.R')

##### Index Data Cleaning #####

# Reading in file, using projroot library so only relative path needed



raw.index.data <- read.csv('Index and Terror Data/All_indices_cleaned_test.csv')

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


index.zoo.UK.decade.list.ALLSHARE.omitted <- lapply(index.zoo.UK.decade.list.ALLSHARE, na.omit)
index.zoo.UK.decade.list.ALLSHARE.omitted <- lapply(index.zoo.UK.decade.list.ALLSHARE.omitted, prices.to.returns)

# Creating zoo indices that aren't split up into decades

# FTSE ALLSHARE
index.data.UK.ALLSHARE <- select.index(raw.index.data.UK,
                                       index.to.select = "FTSE.ALL.SHARE...PRICE.INDEX")
index.zoo.UK.ALLSHARE.omitted <-
  index.data.UK.ALLSHARE %>%
  read.zoo %>%
  na.omit %>%
  prices.to.returns

index.data.zoo.FTSE <- select.index(raw.index.data.UK,
                                    index.to.select = "FTSE.100...PRICE.INDEX") %>% 
  read.zoo %>% 
  na.omit %>% 
  prices.to.returns

index.data.zoo.MSCI <- select.index(raw.index.data.UK,
                                    index.to.select = "MSCI.UK...PRICE.INDEX") %>% 
  read.zoo %>% 
  na.omit %>% 
  prices.to.returns

index.data.zoo.FT30 <- select.index(raw.index.data.UK,
                                    index.to.select = "FT.30.ORDINARY.SHARE...PRICE.INDEX") %>% 
  read.zoo %>% 
  na.omit %>% 
  prices.to.returns


# Cleaning up unwanted variables
removal.list.index <- c(
                 'raw.index.data',
                 'raw.index.data.UK',
                 'raw.index.data.UK.90s',
                 'raw.index.data.UK.80s',
                 'raw.index.data.UK.00s',
                 'raw.index.data.UK.10s',
                 'index.data.UK.decade.list.ALLSHARE',
                 'keep.vars',
                 'index.data.UK.decade.list',
                 'removal.list.index',
                 'index.zoo.UK.decade.list.ALLSHARE')

rm(list = removal.list.index)

#### Terror Event Cleaning ####

load('Index and Terror Data/UKTerrorData.Rdata')
load('Index and Terror Data/TerrorCovariates_subtype.Rdata')
# electing relevant columns
terror.data <- subset(terror.UK.grouped, select = c(Date,
                                                      nkill,
                                                      nwound,
                                                      incident))

# Creating the weights with which to calculate terror intensity. These are identical to those used by the GTI
fatality.weight <- 3
incident.weight <- 1
injury.weight <- 0.5

# The weights used by the GTI
terror.data <- mutate(terror.data,
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

events.80s <- filter.events(event.data = terror.data, start.Date = '1979-12-31', end.Date = '1990-01-01', n.events = 5)
events.90s <- filter.events(event.data = terror.data, start.Date = '1989-12-31', end.Date = '2000-01-01', n.events = 5)
events.00s <- filter.events(event.data = terror.data, start.Date = '1999-12-31', end.Date = '2010-01-01', n.events = 5)
events.10s <- filter.events(event.data = terror.data, start.Date = '2009-12-31', end.Date = '2020-01-01', n.events = 5) 
events.all.decades <- bind_rows(events.80s,
                                events.90s,
                                events.00s,
                                events.10s)


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

all.events.filtered <- screen.overlapping.events(events.sorted) %>% 
  subset(select = -c(overlap))


# Overlapping and non-overlapping (the two are complements) events
no.overlap.dates <- all.events.filtered$Date
no.overlap <- merge(all.events.filtered, terror.covariates.subset, by.x = 'Date', by.y = 'Date', all.x = TRUE) %>% 
  as.tibble %>% 
  subset(select = -c(nkill.y,
                     nwound.y,
                     incident.y))

overlap <- merge(terror.covariates.subset, all.events.filtered, by = 'Date', all.x = TRUE) %>% 
  as.tibble
overlap <- overlap[is.na(overlap$nkill.y),]
overlap <- subset(overlap, select = -c(nkill.y,
                                       nwound.y,
                                       incident.y))

removal.list.terror <- c('fatality.weight',
                         'incident.weight',
                         'injury.weight',
                         'removal.list.terror')
rm(list = removal.list.terror)











#
#
#
#
#### Finite Moment Results ####

# Testing FTSE allshare first

all.share.prices <- na.omit(index.data.UK.ALLSHARE) %>% 
  read.zoo

pre.whitened.Allshare.residuals <- dynlm(index.zoo.UK.ALLSHARE.omitted ~ L(index.zoo.UK.ALLSHARE.omitted, 1:7))$residuals

all.share.finite.moment.pvalue <- perform.finite.fourth.moment.check(pre.whitened.Allshare.residuals)
all.share.finite.moment.pvalue

pre.whitened.FTSE100.residuals <- dynlm(index.data.zoo.FTSE ~ L(index.data.zoo.FTSE, 1:7))$residuals
FTSE100.finite.moment.pvalue <- perform.finite.fourth.moment.check(pre.whitened.FTSE100.residuals)
FTSE100.finite.moment.pvalue


#### Decade CAR Results ####

decade.event.overlap.check <- list()
for (i in 1:4){
  events.screened <- screen.overlapping.events(events.decade.list[[i]], arrange = FALSE)
  decade.event.overlap.check[[i]] <- events.screened
}
## There's no overlapping of events WITHIN the top 5 events each decade. N.B. The first event cannot overlap by construction so gives NA

# CAR10
decade.event.study.CAR10 <- events.decade.list %>% 
  map_dfr(calculate.car, index = index.zoo.UK.ALLSHARE.omitted, boot = TRUE)


# CAR4 N.B. car.length set to 5 as it counts event day (i.e. t=0) inclusively

decade.event.study.CAR4 <- events.decade.list %>% 
  map_dfr(calculate.car, index = index.zoo.UK.ALLSHARE.omitted, car.length = 5, boot = TRUE)



decade.event.study.CAR10
decade.event.study.CAR4
# Calculating CAAR split by decade

  

# All of them together
CAAR.10.by.decade <- events.decade.list %>% 
  map( calculate.car, index = index.zoo.UK.ALLSHARE.omitted) %>% 
  map(calculate.rolling.CAAR) %>% 
  map(calculate.CI.rolling.CAAR) %>% 
  map_dfr(. %>% filter(n ==5))

CAAR.10.by.decade$Decade <- CAAR.10.by.decade$Date %>%
  as.Date %>%
  floor_date(years(10)) %>%
  year

CAAR.10.by.decade <- select(CAAR.10.by.decade, c(rolling.CAAR, rolling.sd, rolling.ci, Decade))
colnames(CAAR.10.by.decade) <- c('CAAR', 'SD', 'CI', 'Decade')
CAAR.10.by.decade

# Now CAAR.4
CAAR.4.by.decade <- events.decade.list %>% 
  map( calculate.car, index = index.zoo.UK.ALLSHARE.omitted, car.length = 5) %>% 
  map(calculate.rolling.CAAR) %>% 
  map(calculate.CI.rolling.CAAR) %>% 
  map_dfr(. %>% filter(n ==5))

CAAR.4.by.decade$Decade <- CAAR.4.by.decade$Date %>%
  as.Date %>%
  floor_date(years(4)) %>%
  year

CAAR.4.by.decade <- select(CAAR.4.by.decade, c(rolling.CAAR, rolling.sd, rolling.ci, Decade))
colnames(CAAR.4.by.decade) <- c('CAAR', 'SD', 'CI', 'Decade')
CAAR.4.by.decade

#### Decade Logit Results ####

# Estimating conditional probability using three different models. A separate logit regression for each event, a pooled regression and a hierarchical model.

# Compiling the pooled and separate stan model code. This is quicker than using the stan() function and constantly recompiling the C++ code stan uses. Doesn't matter for hierarchical as only run once.
# This can often be very noisey with a ton of messages sent to the console - they're compiling messages and can be safely ignored
separate.compiled <- stan_model(file = 'Stan Files/SeparateDecade.stan')
pooled.compiled <- stan_model(file = 'Stan Files/PooledDecade.stan')
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Hierarchical
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++

stan.events.data <- prepare.stan.data(n.events = 20, events = events.all.decades, index = index.zoo.UK.ALLSHARE.omitted)
stan.pooled.data <- map(stan.events.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:20, ~mutate(.x, event = .y))


stan.hierarchical.data <- list(N = nrow(stan.pooled.data), L = 20, id = stan.pooled.data$event,
                               Y = stan.pooled.data$Y,
                               returns = stan.pooled.data$returns,
                               terror_return = unique(stan.pooled.data$terror_return))


hfit <- stan(file = 'Stan Files/HierarchicalLogit.stan',
             data = stan.hierarchical.data,
             control = list(adapt_delta = 0.9999, max_treedepth = 20))


#+++++++++++++++++++++++++++++++
## 80s - i.e. pooled and separate
#+++++++++++++++++++++++++++++++
events.80s.data <- prepare.stan.data(n.events = 5, events = events.80s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.80s.data <- map(events.80s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

# Pooled model datalist
stan.pooled.datalist.80s <- list(N = nrow(pooled.80s.data),
                             Y = pooled.80s.data$Y,
                             returns = pooled.80s.data$returns,
                             terror_return = unique(pooled.80s.data$terror_return))
# Fitting pooled model
poolfit.80s <- sampling(object = pooled.compiled,
                    data = stan.pooled.datalist.80s)

# Separate model (i.e. no pooling)
separatefit.80s <- lapply(events.80s.data, function(x) sampling(object = separate.compiled, data = x))



#++++++++++++++++++++++++++++
##  90s
#++++++++++++++++++++++++++++
events.90s.data <- prepare.stan.data(n.events = 5, events = events.90s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.90s.data <- map(events.90s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

# Pooled model
pooled.datalist.90s <- list(N = nrow(pooled.90s.data),
                        Y = pooled.90s.data$Y,
                        returns = pooled.90s.data$returns,
                        terror_return = unique(pooled.90s.data$terror_return))
poolfit.90s <- sampling(object = pooled.compiled,
                    data = pooled.datalist.90s)

# Separate model
separatefit.90s <- lapply(events.90s.data, function(x) sampling(object = separate.compiled, data = x,
                                                                control = list(adapt_delta = 0.9999, max_treedepth = 20)))



#++++++++++++++++++++++++
## 00s
#++++++++++++++++++++++++
events.00s.data <- prepare.stan.data(n.events = 5, events = events.00s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.00s.data <- map(events.00s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))


# Pooled

pooled.datalist.00s <- list(N = nrow(pooled.00s.data),
                            Y = pooled.00s.data$Y,
                            returns = pooled.00s.data$returns,
                            terror_return = unique(pooled.00s.data$terror_return))
poolfit.00s <- sampling(object = pooled.compiled,
                    data = pooled.datalist.90s)


# Separate
separatefit.00s <- lapply(events.00s.data, function(x) sampling(object = separate.compiled, data = x))


#++++++++++++++++++++++++
## 10s
#++++++++++++++++++++++++
events.10s.data <- prepare.stan.data(n.events = 5, events = events.10s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.10s.data <- map(events.10s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

# Pooled
pooled.datalist.10s <- list(N = nrow(pooled.10s.data),
                            Y = pooled.10s.data$Y,
                            returns = pooled.10s.data$returns,
                            terror_return = unique(pooled.10s.data$terror_return))
poolfit.10s <- sampling(object = pooled.compiled,
                    data = pooled.datalist.10s)


# Separately
separatefit.10s <- lapply(events.10s.data, function(x) sampling(object = separate.compiled, data = x))




## Saving fitted model so don't need to re-fit the model all the time. Should ideally have done this using map()

# save(hfit, file =  'Stanfit Objects/hierarchicaldecadefit.Rdata')
# save(poolfit.80s, file = 'Stanfit Objects/poolfit80s.Rdata')
# save(poolfit.90s, file = 'Stanfit Objects/poolfit90s.Rdata')
# save(poolfit.00s, file = 'Stanfit Objects/poolfit00s.Rdata')
# save(poolfit.10s, file = 'Stanfit Objects/poolfit10s.Rdata')
# 
# 
# save(separatefit.80s, file = 'Stanfit Objects/separatefit80s.Rdata')
# save(separatefit.90s, file = 'Stanfit Objects/separatefit90s.Rdata')
# save(separatefit.00s, file = 'Stanfit Objects/separatefit00s.Rdata')
# save(separatefit.10s, file = 'Stanfit Objects/separatefit10s.Rdata')








# Collating model results

separatefit.vector <- list(separatefit.80s,
                      separatefit.90s,
                      separatefit.00s,
                      separatefit.10s)

poolfit.vector <- list(list(poolfit.80s),
                       list(poolfit.90s),
                       list(poolfit.00s),
                       list(poolfit.10s))

decade.vector <- list(rep('1980', 5),
                   rep('1990', 5),
                   rep('2000',5),
                   rep('2010', 5))


## Pooled results of a different class to separate results - perhaps need to rewrite separate code to run it in one function
stan.separate.results <-  map2_dfr(.x = separatefit.vector, .y = decade.vector, .f = collect.stan.results, parameter = 'y_hat', model = 'separate' )
stan.pooled.results <- map2_dfr(.x = poolfit.vector, .y = decade.vector, .f = collect.stan.results, parameter = 'y_hat', model = 'pooled')

results.hfit.y_hat <- tidy(hfit, pars = 'y_hat', conf.int = TRUE) %>% 
  as.tibble
results.hfit.y_hat$decade <- unlist(decade.vector)
results.hfit.y_hat$event <- 1:5
results.hfit.y_hat$model <- 'hierarchical'

# All decade conditional probability results:
decade.cp.results <- bind_rows(stan.separate.results,
                               stan.pooled.results,
                               results.hfit.y_hat)


#### Largest Event CAR Results ####


lockerbie.bombing.event.study <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 1)
london.7.7.bombing.event.study <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 2)
omagh.bombing.event.study <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 3)
manchester.bombing.1996.event.study <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 4)
droppin.well.bombing.event.study <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 5)
lockerbie.bombing.event.study
london.7.7.bombing.event.study
omagh.bombing.event.study
manchester.bombing.1996.event.study
droppin.well.bombing.event.study


# Calculating rolling CAAR of all terror events recorded - used in the graphic for all CAAR
all.CAR.10.day.ALLSHARE <- calculate.car(events = events.sorted,
                                               index = index.zoo.UK.ALLSHARE.omitted)

all.CAR.10.day.ALLSHARE <- calculate.rolling.CAAR(all.CAR.10.day.ALLSHARE)

all.CAR.10.day.ALLSHARE <- calculate.CI.rolling.CAAR(all.CAR.10.day.ALLSHARE)

# The same but now I screen for overlapping events, significantly reducing the number of events used - used in graphic for filtered CAAR
all.CAR.10.day.ALLSHARE.no.overlap <- screen.overlapping.events(events.sorted)
all.CAR.10.day.ALLSHARE.no.overlap <- calculate.car(all.CAR.10.day.ALLSHARE.no.overlap, index.zoo.UK.ALLSHARE.omitted) %>%
  calculate.rolling.CAAR %>%
  calculate.CI.rolling.CAAR %>%
  as.tibble


# Calculating 10-day and 4-day CAAR with bootstrapped confidence intervals using every event - Used in tables for all CAAR
CAAR.all.10.day <- calculate.CAAR(events.sorted, index.zoo.UK.ALLSHARE.omitted)
CAAR.filtered.10.day <- calculate.CAAR(all.events.filtered, index.zoo.UK.ALLSHARE.omitted)
CAAR.overlap.10.day <- calculate.CAAR(overlap, index.zoo.UK.ALLSHARE.omitted)


CAAR.all.4.day <- calculate.CAAR(events.sorted, car.length = 5, index.zoo.UK.ALLSHARE.omitted)
CAAR.filtered.4.day <- calculate.CAAR(all.events.filtered, car.length = 5, index.zoo.UK.ALLSHARE.omitted)
CAAR.overlap.4.day <- calculate.CAAR(overlap, car.length = 5, index.zoo.UK.ALLSHARE.omitted)

CAAR.all.10.day$Parameter <- '10-day CAAR all'
CAAR.filtered.10.day$Parameter <- '10-day CAAR filtered'
CAAR.overlap.10.day$Parameter <- '10-day CAAR overlap'
CAAR.all.4.day$Parameter <- '4-day CAAR all'
CAAR.filtered.4.day$Parameter <- '4-day CAAR filtered'
CAAR.overlap.4.day <- '4-day CAAR overlap'


CAAR.table <- rbind(CAAR.all.10.day,
                    CAAR.filtered.10.day,
                    CAAR.overlap.10.day,
                    CAAR.all.4.day,
                    CAAR.filtered.4.day,
                    CAAR.overlap.4.day) %>% 
  as.tibble %>% 
  subset(select = -c(event.car))

# Calculating CAAR for the N largest events
largest.5.events.CAAR.allshare <- calculate.CAAR(events.top5, index.zoo.UK.ALLSHARE.omitted)
largest.10.events.CAAR <- calculate.CAAR(events.sorted[1:10,],
                                         index.zoo.UK.ALLSHARE.omitted)
largest.20.events.CAAR <- calculate.CAAR(events.sorted[1:20,],
                                         index.zoo.UK.ALLSHARE.omitted)

largest.5.events.CAAR.MSCI <- seq(11) %>% 
  map_df( ~ calculate.CAAR(events = events.top5,
                           index = index.data.zoo.MSCI,
                           car.length = .x)) %>% 
  mutate(day.CAAR = as.integer(seq(nrow(.)) - 1),
         index = 'MSCI')

largest.5.events.CAAR.ALLSHARE <- seq(11) %>% 
  map_df( ~ calculate.CAAR(events = events.top5,
                        index = index.zoo.UK.ALLSHARE.omitted,
                        car.length = .x)) %>% 
  mutate(day.CAAR = as.integer(seq(nrow(.)) - 1),
         index = 'FTSE Allshare')

largest.5.events.CAAR.FT30 <- seq(11) %>% 
  map_df(~calculate.CAAR(events = events.top5,
                         index = index.data.zoo.FT30,
                         car.length = .x)) %>% 
  mutate(day.CAAR = as.integer(seq(nrow(.)) - 1),
                        index = 'FT30')


largest.5.events.CAAR.table <- rbind(largest.5.events.CAAR.ALLSHARE,
                                     largest.5.events.CAAR.MSCI,
                                     largest.5.events.CAAR.FT30)



# The same but removing overlapping events that appear in the top 15 and 25 - N.B. not screening for tiny events occurring in window.
events.top15.no.overlap <- screen.overlapping.events(events.sorted[1:15,])
events.top25.no.overlap <- screen.overlapping.events(events.sorted[1:25,])




largest.20.no.overlap.CAAR.ALLSHARE <- seq(11) %>% 
  map_df(~calculate.CAAR(events = events.top25.no.overlap[1:20,],
                         index = index.zoo.UK.ALLSHARE.omitted,
                         car.length = .x)) %>% 
  mutate(index = 'FTSE Allshare')


largest.20.no.overlap.CAAR.MSCI <- seq(11) %>% 
  map_df(~calculate.CAAR(events = events.top25.no.overlap[1:20,],
                         index = index.data.zoo.MSCI,
                         car.length = .x)) %>% 
  mutate(index = 'MSCI')

largest.20.no.overlap.CAAR.FT30 <- seq(11) %>% 
  map_df(~calculate.CAAR(events = events.top25.no.overlap[1:20,],
                         index = index.data.zoo.FT30,
                         car.length = .x)) %>% 
  mutate( index = 'FT30')
  



largest.20.CAAR.table <- rbind(largest.20.no.overlap.CAAR.ALLSHARE,
                               largest.20.no.overlap.CAAR.MSCI,
                               largest.20.no.overlap.CAAR.FT30)

## Calculating 10- and 4- day CAR for every event observed both screened and un-screened. Subtle difference here. This function calculates the CAR for every event but doesnt aggregate up into cAARs.
# Really this is quite inefficient since just repeating steps used above to calculate CAAR

CAR.10.unfiltered <- calculate.car(events.sorted, index.zoo.UK.ALLSHARE.omitted)
CAR.10.filtered <- calculate.car(screen.overlapping.events(events.sorted), index.zoo.UK.ALLSHARE.omitted)


CAR.4.unfiltered <- calculate.car(events.sorted, index.zoo.UK.ALLSHARE.omitted)
CAR.4.filtered <- calculate.car(screen.overlapping.events(events.sorted), index.zoo.UK.ALLSHARE.omitted)

save(CAR.10.filtered, CAR.10.unfiltered, CAR.4.filtered, CAR.4.unfiltered, file = 'AnalysisOutput/CAR_Data.RData')


#### Largest Event Logit Results ####



# Seems kind of pointless as near identical events to decade study but whatever I guess
# Hierarchical
stan.largest5.pooled.data <- prepare.stan.data(n.events = 5, events = events.top5, index.zoo.UK.ALLSHARE.omitted) %>% 
  map(data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

stan.largest5.hierarchical.data <- list(N = nrow(stan.largest5.pooled.data), L = 5, id = stan.largest5.pooled.data$event,
                                       Y = stan.largest5.pooled.data$Y,
                                       returns = stan.largest5.pooled.data$returns,
                                       terror_return = unique(stan.largest5.pooled.data$terror_return))

hfit.large <- stan(file = 'Stan Files/HierarchicalLogit.stan',
                   data = stan.largest5.hierarchical.data,
                   control = list(adapt_delta = 0.99, max_treedepth = 20))

# Pooled
pooled.large.fit <- sampling(object = pooled.compiled,
                             data = stan.largest5.hierarchical.data)

# Separate
events.separate.large.data <- prepare.stan.data(n.events = 5, events = events.top5, index = index.zoo.UK.ALLSHARE.omitted)

separatefit.large <- lapply(events.separate.large.data, function(x) sampling(object = separate.compiled, data = x))



## Results
results.hfit.large <- tidy(hfit.large, conf.int = TRUE, pars = 'y_hat')
results.poolfit.large <- tidy(pooled.large.fit, conf.int = TRUE, pars = 'y_hat')
results.separatefit.large <- collect.stan.results(separatefit.large, 'y_hat', decade = 'NA', model = 'separate')


results.hfit.large$event <- 1:5
results.poolfit.large$event <- 1:5

results.hfit.large$model <- 'hierarchical'
results.poolfit.large$model <- 'pooled'
results.separatefit.large$decade <- NULL

large.cp.results <- bind_rows(results.hfit.large,
                              results.poolfit.large,
                              results.separatefit.large)















#### Misc ####



## Testing overlapping and non-overlapping data is balanced.
overlap$overlap <- 1
no.overlap$overlap <- 0
data.on.overlap <- rbind(overlap,
                         no.overlap)
data.on.overlap <- subset(data.on.overlap, select = -c(Date,
                                                       terror.UK.Date,
                                                       terror.intensity))



tests <- data.on.overlap %>% 
  summarise_at(vars(1:81),
               funs(t.test(.[overlap == 1] , .[overlap == 0])$p.value))

tests














#### End Script ####
save.image(file = 'AnalysisOutput/AnalysisOutput.Rdata')
beepr::beep()



