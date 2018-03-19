## Testing whether overlapping and non overlapping events are balanced on observables ##
rm(list = ls())
library(tidyverse)

load('AnalysisOutput/AnalysisOutput.Rdata')
rm(list=setdiff(ls(), "all.events.filtered"))

load('Index and Terror Data/TerrorCovariates_subtype.Rdata')



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
