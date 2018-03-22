## Calculates CARs for top twenty events per decade

rm(list = ls())
source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')



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



save(decade.event.study.CAR10,
     decade.event.study.CAR4,
     CAAR.10.by.decade,
     CAAR.4.by.decade,
     file = 'AnalysisOutput/Decade CAR Output.Rdata')

