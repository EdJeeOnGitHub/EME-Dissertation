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

index.chosen <-  "FTSE.ALL.SHARE...PRICE.INDEX"
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

# Calculating confidence intervals

CI <- function(esti.window){
  esti.window <- na.omit(esti.window)
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  ci <- qt(0.975, df = length(esti.window) - 1)*(estimation.stdev/root.n)
  return(ci)
}


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
  
  CI.width <- CI(estimation.car)
  
  return.zoo <- merge.zoo(event.ar, event.car, t.statistic.CAR, p.value, CI.width)
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



# Creating Visualisations
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(UK.Terror.Dataset.Subset$nwound)


library(ggplot2)
library(ggthemes)



ggplot(UK.Terror.Dataset.Subset[(UK.Terror.Dataset.Subset$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE) +
  xlim(0, 100) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016') +
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset[(UK.Terror.Dataset.Subset$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016') +
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset[(UK.Terror.Dataset.Subset$nkill > 0), ], aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities | at least one person is killed') +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset, aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities') +
  annotate('text', x = 150, y = 1570, label = 'N.B. scale has doubled', colour = 'orange', size = 8) +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()


ggplot(UK.Terror.Dataset.Subset[(UK.Terror.Dataset.Subset$propvalue > 0), ], aes(propvalue)) +
  geom_histogram(show.legend = FALSE) +
  xlab('Recorded Property Damage | Property Damage > 0') +
  ggtitle('Property Damage from UK Terror Attacks, 1970-2016') +
  annotate('text', x = 2.5*10^9, y = 20, label = '1992 Manchester \n Bombing', colour = 'red') +
  annotate('text', x = 1.2*10^9, y = 15, label = '1996 Manchester Bombing', colour = 'red') +
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset, aes(log(Terror.Intensity), fill = cut(log(Terror.Intensity), 1000))) +
  geom_histogram(show.legend = FALSE) +
  ggtitle('Log(Terror Intensity) from 1983-2016 in the UK') +
  theme_minimal()




ggplot(UK.Terror.Dataset.Subset, aes(Date, nkill, colour = cut(nkill, 10000))) +
  geom_point(size = 1, show.legend = FALSE, aes(size = Terror.Intensity)) +
  xlab('Year of Attack') +
  ylab('Number of Fatalities') +
  ggtitle('Deaths Attributed to Terror in the UK, 1970-2016') +
  theme_minimal()
UK.Terror.Dataset.Subset$roll.nkill <- rollmean(UK.Terror.Dataset.Subset$nkill, k = 50, fill = NA, align = 'right') 

ggplot(UK.Terror.Dataset.Subset, aes(Date, nkill, colour = cut(nkill, 10000))) +
  geom_point( aes(size = Terror.Intensity), show.legend = FALSE) +
  scale_y_log10() +
  xlab('Year of Attack') +
  ylab('Number of Fatalities, \n logarithmic scale') +
  ggtitle('Deaths Attributed to Terror in the UK, 1970-2016') + 
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset, aes(Date, nwound, Terror.intensity,  colour = cut(nwound, 100))) +
  geom_point(show.legend = FALSE, aes(size = Terror.Intensity)) +
  ylab('Number of wounded') +
  xlab('Year of Attack') +
  ggtitle('Injuries Attributed to Terror in the UK, 1970-2016') + 
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset, aes(Date, log(nwound), colour = cut(nwound, 100))) +
  geom_point(show.legend = FALSE, aes(size = Terror.Intensity)) +
  ylab('Log Number of wounded') +
  xlab('Year of Attack') +
  ggtitle('Injuries Attributed to Terror in the UK, 1970-2016') + 
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()


ggplot(UK.Terror.Dataset.Subset, aes(Date, Terror.Intensity, colour = cut(Terror.Intensity, 100))) +
  geom_point(show.legend = FALSE) +
  ylab('Terror Intensity') +
  xlab('Year of Attack') +
  ggtitle('Terror Intensity, UK 1970-2016') + 
  theme_minimal()

ggplot(UK.Terror.Dataset.Subset, aes(as.Date(Date), log(Terror.Intensity), colour = cut(log(Terror.Intensity), 100))) +
  geom_point(show.legend = FALSE) +
  ylab('Log Terror Intensity') +
  xlab('Year of Attack') +
  ggtitle('Terror Intensity, UK 1970-2016') +
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  # scale_size_area() +
  theme_minimal()


# Now performing analysis by no longer splitting up by decade - just looking at the largest attacks on 
# UK soil ever
largest.5.events <- decade.Event.Filter(data = UK.Terror.Dataset.Subset, start.Date = '1980-01-01',
                                        end.Date = '2017-01-01',
                                        n = 5)
index.complete <- index.selector(UK.Index.Data, 1)
head(index.complete)
index.complete <- read.zoo(index.complete)
index.complete <- prices2returns(index.complete)
index.complete.na.omitted <- na.omit(index.complete)

ES.no.decade <- ES(index = index.complete.na.omitted,
                  events = largest.5.events, n = 1, car.length = 5)
lockerbie.bombing.es <- Full.ES(index.complete.na.omitted,
                             events = largest.5.events,
                             n = 1, 
                             car.length = 11)

attack.time.delta <- function(es.object){
  return.df <- rownames_to_column(es.object, var = 'days.since.attack')
  return.df$'days.since.attack' <- as.numeric(return.df$'days.since.attack')
  return(return.df)
}

# lockerbie.bombing.es <- attack.time.delta(lockerbie.bombing.es)
# ggplot(lockerbie.bombing.es, aes(Date, event.car, event.ar)) +
#   geom_smooth(size = 2, colour = 'red', se = F) +
#   geom_smooth(aes(Date, event.car + CI.width), linetype = 'longdash', se= F) +
#   geom_smooth(aes(Date, event.car - CI.width), linetype = 'longdash', se = F) +
#   ylim(-3, 3) +
#   theme_minimal()


ggplot(lockerbie.bombing.es, aes(days.since.attack, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(days.since.attack, event.car + CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(days.since.attack, event.car - CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Lockerbie Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 21 December 1988') +
  ylim(-3, 3.5) +
  theme_minimal()

london.7.7.bombings.es <- Full.ES(index = index.complete.na.omitted,
                               events = largest.5.events, 
                               n = 2,
                               car.length = 11)
london.7.7.bombings.es <- attack.time.delta(london.7.7.bombings.es)

ggplot(london.7.7.bombings.es, aes(days.since.attack, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(days.since.attack, event.car + CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(days.since.attack, event.car - CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('London 7/7 Bombings, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 7 July 2005') +
  ylim(-3, 3.5) +
  theme_minimal()

omagh.bombing.es <- Full.ES(index = index.complete.na.omitted,
                            events = largest.5.events,
                            n = 3,
                            car.length = 11)
omagh.bombing.es <- attack.time.delta(omagh.bombing.es)

ggplot(omagh.bombing.es, aes(days.since.attack, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(days.since.attack, event.car + CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(days.since.attack, event.car - CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Omagh Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 15 August 1998') +
  theme_minimal()

manchester.bombing.1996.es <- Full.ES(index = index.complete.na.omitted,
                                   events = largest.5.events,
                                   n = 4,
                                   car.length = 11)
manchester.bombing.1996.es <- attack.time.delta(manchester.bombing.1996.es)

ggplot(manchester.bombing.1996.es, aes(days.since.attack, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(days.since.attack, event.car + CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(days.since.attack, event.car - CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('1996 Manchester Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 15 June 1996') +
  ylim(-3, 3) +
  theme_minimal()

droppin.well.bombing.es <- Full.ES(index = index.complete.na.omitted,
                                   events = largest.5.events,
                                   n = 5, 
                                   car.length = 11)
droppin.well.bombing.es <- attack.time.delta(droppin.well.bombing.es)

ggplot(droppin.well.bombing.es, aes(days.since.attack, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(days.since.attack, event.car + CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(days.since.attack, event.car - CI.width), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Droppin Well Disco Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 6 December 1982') +
  ylim(-5, 3) +
  theme_minimal()
largest.5.events$name <- c('Lockerbie', 'London 7/7', 'Omagh', '1996 Manchester', 'Droppin Well')
largest.5.events$name <- factor(largest.5.events$name, levels = largest.5.events$name[order(largest.5.events$Terror.Intensity)])


ggplot(largest.5.events, aes(name, Terror.Intensity)) +
  geom_col(aes(name, Terror.Intensity, fill = -Terror.Intensity), show.legend = FALSE) +
  xlab('Event') +
  ylab('Terror Intensity') +
  ggtitle('Terror Intensity, \nTop 5 Events') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y = element_blank()
        ) 
