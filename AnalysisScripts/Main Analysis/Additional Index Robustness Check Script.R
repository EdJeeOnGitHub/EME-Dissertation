## Small cap and sector index data



source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')


events.list <- list(events.sorted,
                     all.events.filtered,
                     overlap)


## Smallcap
smallcap.CAAR <- events.list %>% 
  map(calculate.CAAR(add.index.smallcap))

## Aero Space/Defence
aero.defence.CAAR <- events.list %>% 
  map(calculate.CAAR(add.index.aero.defence))
## Industrial
industrial.CAAR <- events.list %>% 
  map(calculate.CAAR(add.index.industrial))
## Retail
retail.CAAR <- events.list %>% 
  map(calculate.CAAR(add.index.retailers))



# CAAR.table <- rbind(CAAR.all.10.day,
#                     CAAR.filtered.10.day,
#                     CAAR.overlap.10.day,
#                     CAAR.all.4.day,
#                     CAAR.filtered.4.day,
#                     CAAR.overlap.4.day) %>% 
#   as.tibble %>% 
#   subset(select = -c(event.car))