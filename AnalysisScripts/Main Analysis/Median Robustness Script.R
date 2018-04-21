## Script to perform robustness checks using constant median returns
rm(list = ls())


source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')


#### Decade Checks ####
decade.event.study.CAR10.median <- events.decade.list %>% 
  map_dfr(calculate.car, index = index.zoo.UK.ALLSHARE.omitted,
          use.median = TRUE)
decade.event.study.CAR4.median <- events.decade.list %>% 
  map_dfr(calculate.car, index = index.zoo.UK.ALLSHARE.omitted,
          use.median = TRUE,
          car.length = 5)

# All of them together
CAAR.10.by.decade.median <- events.decade.list %>% 
  map( calculate.car, index = index.zoo.UK.ALLSHARE.omitted, use.median = TRUE) %>% 
  map(calculate.rolling.CAAR) %>% 
  map(calculate.CI.rolling.CAAR) %>% 
  map_dfr(. %>% filter(n ==5))

CAAR.10.by.decade.median$Decade <- CAAR.10.by.decade.median$Date %>%
  as.Date %>%
  floor_date(years(10)) %>%
  year

CAAR.10.by.decade.median <- select(CAAR.10.by.decade.median, c(rolling.CAAR, rolling.sd, rolling.ci, Decade))
colnames(CAAR.10.by.decade.median) <- c('CAAR', 'SD', 'CI', 'Decade')
CAAR.10.by.decade.median

# Now CAAR.4
CAAR.4.by.decade.median <- events.decade.list %>% 
  map( calculate.car, index = index.zoo.UK.ALLSHARE.omitted, car.length = 5, use.median = TRUE) %>% 
  map(calculate.rolling.CAAR) %>% 
  map(calculate.CI.rolling.CAAR) %>% 
  map_dfr(. %>% filter(n ==5))

CAAR.4.by.decade.median$Decade <- CAAR.4.by.decade.median$Date %>%
  as.Date %>%
  floor_date(years(4)) %>%
  year

CAAR.4.by.decade.median <- select(CAAR.4.by.decade.median, c(rolling.CAAR, rolling.sd, rolling.ci, Decade))
colnames(CAAR.4.by.decade.median) <- c('CAAR', 'SD', 'CI', 'Decade')

save(decade.event.study.CAR10.median,
     decade.event.study.CAR4.median,
     CAAR.10.by.decade.median,
     CAAR.4.by.decade.median,
     file = 'AnalysisOutput/Decade Median Checks.Rdata')

#### Largest 5 Events ####


lockerbie.bombing.event.study.median <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 1,
                      use.median = TRUE)
london.7.7.bombing.event.study.median <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 2,
                      use.median = TRUE)
omagh.bombing.event.study.median <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 3,
                      use.median = TRUE)
manchester.bombing.1996.event.study.median <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 4,
                      use.median = TRUE)
droppin.well.bombing.event.study.median <-
  perform.event.study(index = index.zoo.UK.ALLSHARE.omitted,
                      events = events.top5,
                      n = 5,
                      use.median = TRUE)
#### ALL CAAR ####
# Calculating 10-day and 4-day CAAR with bootstrapped confidence intervals using every event - Used in tables for all CAAR
CAAR.all.10.day.median <- calculate.CAAR(events.sorted, index.zoo.UK.ALLSHARE.omitted, use.median = TRUE)
CAAR.filtered.10.day.median <- calculate.CAAR(all.events.filtered, index.zoo.UK.ALLSHARE.omitted, use.median = TRUE)
CAAR.overlap.10.day.median <- calculate.CAAR(overlap, index.zoo.UK.ALLSHARE.omitted, use.median = TRUE)


CAAR.all.4.day <- calculate.CAAR(events.sorted, car.length = 5, index.zoo.UK.ALLSHARE.omitted, use.median = TRUE)
CAAR.filtered.4.day <- calculate.CAAR(all.events.filtered, car.length = 5, index.zoo.UK.ALLSHARE.omitted, use.median = TRUE)
CAAR.overlap.4.day <- calculate.CAAR(overlap, car.length = 5, index.zoo.UK.ALLSHARE.omitted, use.median = TRUE)

CAAR.all.10.day.median$Parameter <- '10-day CAAR all'
CAAR.filtered.10.day.median$Parameter <- '10-day CAAR filtered'
CAAR.overlap.10.day.median$Parameter <- '10-day CAAR overlap'
CAAR.all.4.day$Parameter <- '4-day CAAR all'
CAAR.filtered.4.day$Parameter <- '4-day CAAR filtered'
CAAR.overlap.4.day$Parameter <- '4-day CAAR overlap'


CAAR.table <- rbind(CAAR.all.10.day.median,
                    CAAR.filtered.10.day.median,
                    CAAR.overlap.10.day.median,
                    CAAR.all.4.day,
                    CAAR.filtered.4.day,
                    CAAR.overlap.4.day) %>% 
  as.tibble %>% 
  subset(select = -c(event.car))
CAAR.table.median <- CAAR.table

save(CAAR.table.median,
     lockerbie.bombing.event.study.median,
     london.7.7.bombing.event.study.median,
     omagh.bombing.event.study.median,
     manchester.bombing.1996.event.study.median,
     droppin.well.bombing.event.study.median,
     file = 'AnalysisOutput/Large Median Checks.Rdata')
