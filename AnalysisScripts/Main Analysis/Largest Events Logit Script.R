rm(list = ls())
source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')



separate.compiled <- stan_model(file = 'Stan Files/SeparateDecade.stan')
pooled.compiled <- stan_model(file = 'Stan Files/PooledDecade.stan')
# Seems kind of pointless as near identical events to decade study but whatever I guess
# Hierarchical
stan.largest5.pooled.data <- prepare.stan.data(n.events = 5, events = events.top5, index.zoo.UK.ALLSHARE.omitted) %>% 
  map(data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

stan.largest5.hierarchical.data <- list(N = nrow(stan.largest5.pooled.data), L = 5, id = stan.largest5.pooled.data$event,
                                        Y = stan.largest5.pooled.data$Y,
                                        returns = stan.largest5.pooled.data$returns,
                                        terror_return = unique(stan.largest5.pooled.data$terror_return))

hfit.large <- stan(file = 'Stan Files/HierarchicalLogit.stan',
                   data = stan.largest5.hierarchical.data,
                   control = list(adapt_delta = 0.99, max_treedepth = 20))

# Pooled
pooled.large.fit <- sampling(object = pooled.compiled,
                             data = stan.largest5.hierarchical.data)

# Separate
events.separate.large.data <- prepare.stan.data(n.events = 5, events = events.top5, index = index.zoo.UK.ALLSHARE.omitted)

separatefit.large <- lapply(events.separate.large.data, function(x) sampling(object = separate.compiled, data = x))



## Results
results.hfit.large <- tidy(hfit.large, conf.int = TRUE, pars = 'y_hat')
results.poolfit.large <- tidy(pooled.large.fit, conf.int = TRUE, pars = 'y_hat')
results.separatefit.large <- collect.stan.results(separatefit.large, 'y_hat', decade = 'NA', model = 'separate')


results.hfit.large$event <- 1:5
results.poolfit.large$event <- 1:5

results.hfit.large$model <- 'hierarchical'
results.poolfit.large$model <- 'pooled'
results.separatefit.large$decade <- NULL

large.cp.results <- bind_rows(results.hfit.large,
                              results.poolfit.large,
                              results.separatefit.large)

save(large.cp.results,
     file = 'AnalysisOutput/Largest Event Logit Results.Rdata')