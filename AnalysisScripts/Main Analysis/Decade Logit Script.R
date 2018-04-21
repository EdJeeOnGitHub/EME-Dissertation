
## Runs hierarchical logistic regression for conditional probability results

rm(list = ls())
source('AnalysisScripts/DissertationFunctions.R')
load('AnalysisOutput/Analysis Script Data.Rdata')

#### Decade Logit Results ####

# Estimating conditional probability using three different models. A separate logit regression for each event, a pooled regression and a hierarchical model.

# Compiling the pooled and separate stan model code. This is quicker than using the stan() function and constantly recompiling the C++ code stan uses. Doesn't matter for hierarchical as only run once.
# This can often be very noisey with a ton of messages sent to the console - they're compiling messages and can be safely ignored
separate.compiled <- stan_model(file = 'Stan Files/SeparateDecade.stan')
pooled.compiled <- stan_model(file = 'Stan Files/PooledDecade.stan')
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Hierarchical
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++

stan.events.data <- prepare.stan.data(n.events = 20, events = events.all.decades, index = index.zoo.UK.ALLSHARE.omitted)
stan.pooled.data <- map(stan.events.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:20, ~mutate(.x, event = .y))


stan.hierarchical.data <- list(N = nrow(stan.pooled.data), L = 20, id = stan.pooled.data$event,
                               Y = stan.pooled.data$Y,
                               returns = stan.pooled.data$returns,
                               terror_return = unique(stan.pooled.data$terror_return))


hfit <- stan(file = 'Stan Files/HierarchicalLogit.stan',
             data = stan.hierarchical.data,
             control = list(adapt_delta = 0.9999, max_treedepth = 20))


#+++++++++++++++++++++++++++++++
## 80s - i.e. pooled and separate
#+++++++++++++++++++++++++++++++
events.80s.data <- prepare.stan.data(n.events = 5, events = events.80s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.80s.data <- map(events.80s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

# Pooled model datalist
stan.pooled.datalist.80s <- list(N = nrow(pooled.80s.data),
                                 Y = pooled.80s.data$Y,
                                 returns = pooled.80s.data$returns,
                                 terror_return = unique(pooled.80s.data$terror_return))
# Fitting pooled model
poolfit.80s <- sampling(object = pooled.compiled,
                        data = stan.pooled.datalist.80s)

# Separate model (i.e. no pooling)
separatefit.80s <- lapply(events.80s.data, function(x) sampling(object = separate.compiled, data = x))



#++++++++++++++++++++++++++++
##  90s
#++++++++++++++++++++++++++++
events.90s.data <- prepare.stan.data(n.events = 5, events = events.90s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.90s.data <- map(events.90s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

# Pooled model
pooled.datalist.90s <- list(N = nrow(pooled.90s.data),
                            Y = pooled.90s.data$Y,
                            returns = pooled.90s.data$returns,
                            terror_return = unique(pooled.90s.data$terror_return))
poolfit.90s <- sampling(object = pooled.compiled,
                        data = pooled.datalist.90s)

# Separate model
separatefit.90s <- lapply(events.90s.data, function(x) sampling(object = separate.compiled, data = x,
                                                                control = list(adapt_delta = 0.9999, max_treedepth = 20)))



#++++++++++++++++++++++++
## 00s
#++++++++++++++++++++++++
events.00s.data <- prepare.stan.data(n.events = 5, events = events.00s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.00s.data <- map(events.00s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))


# Pooled

pooled.datalist.00s <- list(N = nrow(pooled.00s.data),
                            Y = pooled.00s.data$Y,
                            returns = pooled.00s.data$returns,
                            terror_return = unique(pooled.00s.data$terror_return))
poolfit.00s <- sampling(object = pooled.compiled,
                        data = pooled.datalist.90s)


# Separate
separatefit.00s <- lapply(events.00s.data, function(x) sampling(object = separate.compiled, data = x))


#++++++++++++++++++++++++
## 10s
#++++++++++++++++++++++++
events.10s.data <- prepare.stan.data(n.events = 5, events = events.10s, index = index.zoo.UK.ALLSHARE.omitted)
pooled.10s.data <- map(events.10s.data, data.frame) %>% 
  map2_dfr(.x = ., .y = 1:5, ~mutate(.x, event = .y))

# Pooled
pooled.datalist.10s <- list(N = nrow(pooled.10s.data),
                            Y = pooled.10s.data$Y,
                            returns = pooled.10s.data$returns,
                            terror_return = unique(pooled.10s.data$terror_return))
poolfit.10s <- sampling(object = pooled.compiled,
                        data = pooled.datalist.10s)


# Separately
separatefit.10s <- lapply(events.10s.data, function(x) sampling(object = separate.compiled, data = x))




## Saving fitted model so don't need to re-fit the model all the time. Should ideally have done this using map()

# save(hfit, file =  'Stanfit Objects/hierarchicaldecadefit.Rdata')
# save(poolfit.80s, file = 'Stanfit Objects/poolfit80s.Rdata')
# save(poolfit.90s, file = 'Stanfit Objects/poolfit90s.Rdata')
# save(poolfit.00s, file = 'Stanfit Objects/poolfit00s.Rdata')
# save(poolfit.10s, file = 'Stanfit Objects/poolfit10s.Rdata')
# 
# 
# save(separatefit.80s, file = 'Stanfit Objects/separatefit80s.Rdata')
# save(separatefit.90s, file = 'Stanfit Objects/separatefit90s.Rdata')
# save(separatefit.00s, file = 'Stanfit Objects/separatefit00s.Rdata')
# save(separatefit.10s, file = 'Stanfit Objects/separatefit10s.Rdata')








# Collating model results

separatefit.vector <- list(separatefit.80s,
                           separatefit.90s,
                           separatefit.00s,
                           separatefit.10s)

poolfit.vector <- list(list(poolfit.80s),
                       list(poolfit.90s),
                       list(poolfit.00s),
                       list(poolfit.10s))

decade.vector <- list(rep('1980', 5),
                      rep('1990', 5),
                      rep('2000',5),
                      rep('2010', 5))


## Pooled results of a different class to separate results - perhaps need to rewrite separate code to run it in one function
stan.separate.results <-  map2_dfr(.x = separatefit.vector, .y = decade.vector, .f = collect.stan.results, parameter = 'y_hat', model = 'separate' )
stan.pooled.results <- map2_dfr(.x = poolfit.vector, .y = decade.vector, .f = collect.stan.results, parameter = 'y_hat', model = 'pooled')

results.hfit.y_hat <- tidy(hfit, pars = 'y_hat', conf.int = TRUE) %>% 
  as.tibble
results.hfit.y_hat$decade <- unlist(decade.vector)
results.hfit.y_hat$event <- 1:5
results.hfit.y_hat$model <- 'hierarchical'

# All decade conditional probability results:
decade.cp.results <- bind_rows(stan.separate.results,
                               stan.pooled.results,
                               results.hfit.y_hat)

save(decade.cp.results,
     results.hfit.y_hat,
     file = 'AnalysisOutput/Decade Logit Output.Rdata')

