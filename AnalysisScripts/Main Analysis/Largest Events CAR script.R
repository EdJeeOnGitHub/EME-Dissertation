
rm(list = ls())
source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')


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
save(CAAR.table,
     lockerbie.bombing.event.study,
     london.7.7.bombing.event.study,
     omagh.bombing.event.study,
     manchester.bombing.1996.event.study,
     droppin.well.bombing.event.study,
     all.CAR.10.day.ALLSHARE,
     all.CAR.10.day.ALLSHARE.no.overlap,
     largest.5.events.CAAR.table,
     largest.20.CAAR.table,
     events.top25.no.overlap,
     file = 'AnalysisOutput/Largest Events CAR Output.Rdata')
