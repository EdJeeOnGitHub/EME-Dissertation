
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

histogram.wounded.small <- ggplot(terror.data[(terror.data$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE) +
  xlim(0, 100) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016', subtitle = 'xlim(0, 100)') +
  theme_minimal()

histogram.killed.at.least.1 <- ggplot(terror.data[(terror.data$nkill > 0), ], aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities | at least one person is killed') +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()


histogram.wounded.small

tests
tests$bonferroni %>% 
  sum
tests$sig.difference %>% 
  sum

joint.orthog.test <- lm(overlap ~ ., data = data.on.overlap)
F.stat <- summary(joint.orthog.test)
# F.stat
F.stat$fstatistic
# Questions:

# Alternatives to simple two-sided t test and joint F-test?

# Incorporate information about the distribution using bayesian approach?

# Feasible to claim lack of balance comes from small sample and skewed distribution - just got unlucky with the sample?

# Also, how best to present many many estimates.
save(tests,
     file = 'AnalysisOutput/Misc Output.Rdata')
