
rm(list = ls())
try(dev.off(), silent = TRUE)

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


tests
tests$bonferroni %>% 
  sum
tests$sig.difference %>% 
  sum

joint.orthog.test <- lm(overlap ~ ., data = data.on.overlap)
F.stat <- summary(joint.orthog.test)
# F.stat
F.stat$fstatistic


KW.tests <- data.on.overlap %>% 
  summarise_at(vars(1:81),
               funs(kruskal.test(list(.[overlap == 1], .[overlap ==0]))$p.value)) %>% 
  gather(key = 'variable', value = 'p.value') %>% 
  mutate(sig.difference = ifelse(p.value < 0.05, TRUE, FALSE),
         bonferroni = ifelse(p.value < 0.05/81, TRUE, FALSE))

KW.tests$bonferroni %>% 
  sum
KW.tests$sig.difference %>% 
  sum

save(KW.tests,
     tests,
     file = 'AnalysisOutput/Misc Output.Rdata')
