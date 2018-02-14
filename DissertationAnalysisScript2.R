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
                 adapt_delta = 0.999, QR = TRUE, control = list(max_treedepth = 20))
beepr::beep()
save(fit2, file = 'Stan_covariate_fit.RData')

fit <- fit2
fit <- varsel(fit, method='forward')
varsel_stats(fit)

varsel_plot(fit, stats=c('mlpd', 'mse'))
fit_cv <- cv_varsel(fit, method='forward', cv_method='LOO')
fit_cv$varsel$ssize
mcmc_areas(as.matrix(fit), 
           pars = c('(Intercept)', names(fit$varsel$vind[1:3]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))

proj <- project(fit, nv = 3, ns = 800)
mcmc_areas(as.matrix(proj)) + coord_cartesian(xlim = c(-2, 2))


########
covariates.ar <- inner_join(x = CAR.10.unfiltered, y = terror.covariates.subset, by = c('market.date' = 'Date')) %>% 
  subset(select = -c(Date, event.car, event.t.stat, event.p.value, event.confidence.interval, stars, market.date)) %>% 
  as.tibble


full.model2 <- event.ar ~ .



n <- nrow(terror.covariates.subset)
D <- ncol(terror.covariates.subset)

fit3 <- stan_glm(full.model2, family = gaussian(), data = covariates.ar,
                 prior = prior_coeff,
                 adapt_delta = 0.999, QR = TRUE, control = list(max_treedepth = 20))

save(fit3, file = 'Stan_covariate_fit_ar.RData')
