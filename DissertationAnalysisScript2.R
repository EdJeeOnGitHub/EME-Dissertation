## Second stage of analysis exploring heterogeneous effects
rm(list = ls())

library(tidyverse)
library(projpred)
library(rstanarm)
options(mc.cores = parallel::detectCores())
#### Merging terror covariates and return data ####
load('TerrorCovariates.Rdata')
load('AnalysisOutput/CAR_Data.RData')

covariates.u <- inner_join(x = CAR.10.unfiltered, y = terror.covariates.subset, by = c('market.date' = 'Date')) %>% 
  subset(select = -c(Date, event.ar, event.t.stat, event.p.value, event.confidence.interval, stars, market.date)) %>% 
  as.tibble


# all.dummies <- grep('dummies', colnames(covariates.u), value = TRUE)
# model <- formula(paste("event.car ~  success + suicide + nkill + nwound + nperps + claimed + " , 
#                          paste(all.dummies, collapse=" + ")))

full.model <- event.car ~ .



n <- nrow(terror.covariates.subset)
D <- ncol(terror.covariates.subset)
p0 <- 5
tau0 <- p0/(D-p0)*1/sqrt(n)
prior_coeff <- hs(df = 1, global_df = 1, global_scale = tau0)

fit2 <- stan_glm(full.model, family = gaussian(), data = covariates.u,
                 prior = prior_coeff,
                 adapt_delta = 0.999999)
beepr::beep()
launch_shinystan(fit2)
