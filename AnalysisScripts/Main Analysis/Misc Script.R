rm(list = ls())
source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')

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
               funs(t.test(.[overlap == 1] , .[overlap == 0])$p.value)) %>% 
  gather(key = 'variable', value = 'p.value') %>% 
  mutate(sig.difference = ifelse(p.value < 0.05, TRUE, FALSE),
         bonferroni = ifelse(p.value < 0.05/81, TRUE, FALSE))

save(tests,
     file = 'AnalysisOutput/Misc Output.Rdata')