###########################
###UK Event study by decade
###########################

#Reading in the file
library(readxl)
path1 <- "C:/Users/nfa/Dropbox/Ed/Ed Uni work/EME/Data/Clean Data/Indices/All_indices_cleaned_test.csv"
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

#Converting to 'zoo' format as required by the eventstudies package
library(zoo)

zoo.1980 <- read.zoo(UK.1980s)
zoo.1990 <- read.zoo(UK.1990s)
zoo.2000 <- read.zoo(UK.2000s)
zoo.2010 <- read.zoo(UK.2010s)

# Now reading in UK events and formatting
path.Terror1 <- "C:/Users/nfa/Dropbox/Ed/Ed Uni work/EME/Data/Clean Data/Terror/United Kingdom.xls"
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
###################################
# From here is WIP ################ 
##################################


events.80s <- decade.Event.Filter(UK.Terror.Dataset.Subset,
                                  start.Date = '1979-12-31',
                                  end.Date =  '1990-01-01',
                                  n.events = 5)

events.80s.df <- events.80s[, 'Date']
name_list <- c( "FTSE.100...PRICE.INDEX",
                "FTSE.ALL.SHARE...PRICE.INDEX",
               "FT.30.ORDINARY.SHARE...PRICE.INDEX",
               "MSCI.UK...PRICE.INDEX",
               "UK...TO.US....WMR....EXCHANGE.RATE")
events.80s.df$name <- name_list
colnames(events.80s.df) <- c('when', 'name')

events.80s.df <- data.frame(events.80s.df)

library(eventstudies)


TerrorIndiceCAR <- lapply(1: ncol(zoo.1980), function(x){
  # 10-day window around the event
  event <- phys2eventtime(na.omit(zoo.1980[ , x,
                                                       drop = FALSE]),
                          events.80s.df,
                          10)
  # Estimate ARs
  esMean <- constantMeanReturn(event$z.e[which(attributes(event$z.e)$index
                                               %in% -30:-11), ],
                               residual = FALSE)
  ar <- event$z.e - esMean
  ar <- window(ar, start = 0, end = 10)
  # CAR
  car <- remap.cumsum(ar, base = as.numeric(ar[1, 1]))
  names(car) <- colnames(zoo.1980[ , x,
                                              drop = FALSE])
  
  
  return(car)
})
names(TerrorIndiceCAR) <- colnames(zoo.1980)
# Compile for all indices
TerrorIndiceCAR <- do.call(cbind, TerrorIndiceCAR)
# 11-day CAR
TerrorIndiceCAR[11, ]


prices2returns <- function(x) 100*diff(log(x))
zoo.1980.return <- prices2returns(zoo.1980)

es.results <- phys2eventtime(z = na.omit(zoo.1980.return[, 1, drop = FALSE]),
                             events = events.80s.df,
                             width = 5)
es.w <- window(es.results$z.e,
               start = -5,
               end = +5)
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)
inference.bootstrap(es.w = eventtime,
                  to.plot = TRUE,
                  main = '1980s, bootstrap')

inference.wilcox(es.w = eventtime,
                    to.plot = TRUE,
                 main = '1980s, Wilcox')


inference.classic(es.w = eventtime,
                 to.plot = TRUE,
                 main = '1980s, classic')



data(StockPriceReturns)
data(SplitDates)
es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e,
               start = -5,
               end = +5)
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)
inference.classic(es.w = eventtime,
                  to.plot = TRUE,
                  main = 'Stock Splits, classic')

## Trying a single event

single.Date <- as.Date('1982-12-06')
single.name <- "FTSE.ALL.SHARE...PRICE.INDEX"
single.df <- data.frame(single.Date, single.name)
colnames(single.df) <- c('when', 'name')
single.df$name <- as.character(single.df$name)
single.zoo <- as.zoo(zoo.1980.return[ , 1,
                                        drop = FALSE])
single.zoo.NA <- as.zoo(na.omit(zoo.1980.return[ , 1,
                                                    drop = FALSE]))

event <- phys2eventtime(na.omit(zoo.1980.return[ , 1,
                                            drop = FALSE]),
                          single.df,
                          10)
esMean <- constantMeanReturn(event$z.e[which(attributes(event$z.e)$index
                                             %in% -30:-11), ],
                             residual = FALSE)
ar <- event$z.e - esMean
ar <- window(ar, start = 0, end = 10)
# CAR
car <- remap.cumsum(ar, base = as.numeric(ar[1, 1]))
car2 <- rollsum(na.omit(ar), 10, fill = NA, align = 'right')
names(car) <- colnames(zoo.1980[ , 1,
                                 drop = FALSE])
car[11,]

t.test(x = car,
       y = NULL,
       alternative = 'two.sided',
       mu=0)

single.mean4 <- rollmean(single.zoo.NA, 20, fill = NA, align = 'right')

AR1 <- data.frame(single.zoo.NA, single.mean4)

library(tibble)

AR1 <- as_tibble(rownames_to_column(AR1, var= 'Date'))
AR1$Date <- as.Date(AR1$Date)
colnames(AR1) <- c('Date', 'single.zoo.NA', 'single.mean4')

AR2 <- mutate(AR1, AbR = AR1$single.zoo.NA - AR1$single.mean4)

AR2$const <- 1

CAR.ed4 <- rollapply(AR.zoo, 10 , function(x) mean(x))

AR2$const <- replace(AR2$const, 1:9, 'NA')

AR2$const <- replace(AR2$const, 10:2087, CAR.ed4 )
AR2$const <- as.numeric(AR2$const)
colnames(AR2) <- c('Date', 'Returns', 'Rolling Mean', 'Abnormal Returns', 'Cumulative Abnormal')
# 
# cars.df <- data.frame(car, car2)
# cars.df <- as_tibble(rownames_to_column(cars.df, var = 'indexer'))
# cars.df$indexer <- as.numeric(cars.df$indexer)
# head(cars.df[cars.df$indexer > 0,], 40)




edMean <- window(single.zoo.NA, start = (single.Date - 40), end = (single.Date - 5))
mean(edMean, na.rm= T)

edMean2 <- window(single.zoo.NA, start = (single.Date - 30), end = (single.Date - 11), extend = TRUE)
esMean
mean(edMean2)

default.window <- event$z.e[which(attributes(event$z.e)$index
                                  %in% -30:-11), ]
single.df <- data.frame(single.zoo)
single.df <- as_tibble(rownames_to_column(single.df, var = 'Date'))
single.df$Date <- as.Date(single.df$Date)
head(single.df)

row.indexer <- as.numeric(rownames(single.df)[single.df$Date == single.Date])
window.start <- single.df[row.indexer - 54, 'Date']
window.end <- single.df[row.indexer - 20, 'Date']

window.start2 <- '1982-11-06'
window.end2 <- '1982-11-25'

edMean3 <- window(single.zoo.NA, start = window.start,  end = window.end)
edMean4 <- window(single.zoo.NA, start = window.start2, end = window.end2)

edMean3
default.window
mean(edMean3)
esMean
### Perhaps use phys2eventime repeatedly on my dataset or write something similar myself

NA.index.no <- function(data){
  single.zoo <- data
 if (length(which(attributes(na.omit(single.zoo))$index == single.Date)) == 0){
   if (length(which(attributes(na.omit(single.zoo))$index == single.Date + 1)) == 0){
     
     no <- which(attributes(na.omit(single.zoo))$index == single.Date - 1)
   } else {
     no <- which(attributes(na.omit(single.zoo))$index == single.Date + 1)
   }
   
 } else {
   no <- which(attributes(na.omit(single.zoo))$index == single.Date)
 }
   return(no)
}

row.indexer2 <- NA.index.no(single.zoo)


window.start3 <- attributes(single.zoo.NA[row.indexer2 - 31])$index
window.end3 <- attributes(single.zoo.NA[row.indexer2 - 12])$index



edMean5 <- window(single.zoo.NA, start = window.start3,  end = window.end3)
edMean5
default.window


estimation.stdev <- sd(edMean5)
root.n <- sqrt(length(edMean5))

test.t <- function(data, x){
            test <- data[x]/(estimation.stdev/root.n)
            return(test)
}


for (x in list(1:length(ar))){
  options(scipen = 999)
  t <- test.t(ar, x)
  print(t) 
  print(ar[x])
  p <- 2*pt(-abs(t), df = length(edMean5) - 1)
  print(p)
  options(scipen = 0)
}

SE <- qt(0.975, df = 19)*(estimation.stdev/root.n)
ar.left <- ar - SE
ar.right <- ar + SE

ar.plot <- data.frame(ar.left, ar, ar.right, 0:10)
ar.plot


ggplot(ar.plot, aes(x = X0.10)) +
  geom_line(aes(y = ar.plot$X1.1, colour = 'blue')) +
  geom_line(aes(y = ar.plot$X1, colour = 'grey')) +
  geom_line(aes(y = ar.plot$X1.2, colour = 'grey')) +
  geom_hline( yintercept = 0, colour = 'red')