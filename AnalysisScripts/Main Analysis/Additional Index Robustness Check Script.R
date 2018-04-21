## Small cap and sector index data



source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')


events.filtered.short <- all.events.filtered %>% 
  filter(Date >'1986-01-01')


## Smallcap
smallcap.CAAR <- events.filtered.short %>% 
  calculate.CAAR(add.index.smallcap)
smallcap.CAAR$Parameter <- 'FTSE smallcap'

## Aero Space/Defence
aero.defence.CAAR <- events.filtered.short %>% 
  calculate.CAAR(add.index.aero.defence)
aero.defence.CAAR$Parameter <- 'Aerospace/Defence'
## Industrial
industrial.CAAR <- events.filtered.short %>%  
  calculate.CAAR(add.index.industrial)
industrial.CAAR$Parameter <- 'Industrial'
## Retail
retail.CAAR <- events.filtered.short %>%  
  calculate.CAAR(add.index.retailers)
retail.CAAR$Parameter <- 'Retail'



filtered.robustness.check.table <- rbind(smallcap.CAAR,
                                         aero.defence.CAAR,
                                         industrial.CAAR,
                                         retail.CAAR) %>%
  as.tibble %>%
  subset(select = -c(event.car, CI.width, st.dev))

save(filtered.robustness.check.table, file = 'AnalysisOutput/Additional Index Checks.Rdata')
filtered.robustness.check.table
