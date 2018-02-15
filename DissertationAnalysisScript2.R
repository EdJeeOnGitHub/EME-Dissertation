## Second stage of analysis exploring heterogeneous effects of terror
rm(list = ls())

library(tidyverse)
library(projpred)
library(rstanarm)
options(mc.cores = parallel::detectCores())
source('DissertationFunctions.R')
#### Merging terror covariates and return data ####
load('TerrorCovariates.Rdata')
load('AnalysisOutput/CAR_Data.RData')

# CAR10s
merged.data.u.10 <- merge.and.drop.covariates(CAR.10.unfiltered, terror.covariates.subset)

merged.data.f.10 <- merge.and.drop.covariates(CAR.10.filtered, terror.covariates.subset)

# CAR4s
merged.data.u.4 <- merge.and.drop.covariates(CAR.4.unfiltered, terror.covariates.subset)
merged.data.f.4 <- merge.and.drop.covariates(CAR.4.filtered, terror.covariates.subset)

# Setting some parameters used in hierarchical shrinkage (HS) prior
n.filtered <- nrow(CAR.10.filtered)
D <- (ncol(merged.data.f.10) - 3)
p0 <- 5
tau0.filtered <- p0/(D-p0)*1/sqrt(n.filtered)
prior_coeff.filtered <- hs(df = 1, global_df = 1, global_scale = tau0.filtered)

n.unfiltered <- nrow(CAR.10.unfiltered)
D <- (ncol(merged.data.f.10) - 3)
p0 <- 5
tau0.unfiltered <- p0/(D-p0)*1/sqrt(n.unfiltered)
prior_coeff.unfiltered <- hs(df = 1, global_df = 1, global_scale = tau0.unfiltered)


## TODO: Still need to clean claimed
## Creating dataframes that only contain regressors of interest. This makes it easier to use . notation when specifying models


# These are all CARs
X.CAR10.u <- merged.data.u.10 %>% 
  subset(select = -c(event.ar,
                     returns))

X.CAR10.f <- merged.data.f.10 %>% 
  subset(select = -c(event.ar,
                     returns))

X.CAR4.u <- merged.data.u.4 %>% 
  subset(select = -c(event.ar,
                     returns))

X.CAR4.f <- merged.data.f.4 %>% 
  subset(select = -c(event.ar,
                     returns))


# Now event day ARs and event day returns
X.AR.u <- merged.data.u.10 %>% 
  subset(select = -c(returns,
                     event.car))
X.R.u <- merged.data.u.10 %>% 
  subset(select = -c(event.ar,
                     event.car))

X.AR.f <- merged.data.f.10 %>% 
  subset(select = -c(returns,
                     event.car))
X.R.f <- merged.data.f.10 %>% 
  subset(select = -c(event.ar,
                     event.car))

## Many of the filtered columns contain variables that are always switched off, so I drop these to make variable selection easier
X.R.f <-  X.R.f[, colSums(X.R.f != 0) > 0]
X.AR.f <-  X.AR.f[, colSums(X.AR.f != 0) > 0]
X.CAR10.f <- X.CAR10.f[, colSums(X.CAR10.f != 0) > 0]
X.CAR4.f <- X.CAR4.f[, colSums(X.CAR4.f != 0) > 0]

#### Creating Model Formulae #####
CAR.model <- event.car ~ .
AR.model <- event.ar ~ .
R.model <- returns ~ .

#### Horseshoe models ####
# CARs

CAR.10.f.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f,
                 prior = prior_coeff.filtered,
                 adapt_delta = 0.999, control = list(max_treedepth = 20))

save(CAR.10.f.fit, file = 'Ed/HS_CAR10_f_fit.RData')


CAR.10.u.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u,
                         prior = prior_coeff.unfiltered,
                         adapt_delta = 0.999, QR = FALSE, control = list(max_treedepth = 20))
save(CAR.10.u.fit, file = 'Ed/HS_CAR10_u_fit.Rdata')

CAR.4.f.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.f,
                        prior = prior_coeff.filtered,
                        adapt_delta = 0.999, QR = FALSE)
save(CAR.4.f.fit, file =  'Ed/HS_CAR4_f_fit.RData')

CAR.4.u.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.u,
                        prior = prior_coeff.unfiltered,
                        adapt_delta = 0.999, QR = FALSE)
save(CAR.4.u.fit, file = 'Ed/HS_CAR4_u_fit.RData')
## Now ARs
AR.u.fit <- stan_glm(AR.model, family = gaussian(), data = X.AR.u,
                     prior = prior_coeff.unfiltered,
                     adapt_delta = 0.999, QR = FALSE)
save(AR.u.fit, file = 'Ed/HS_AR_u_fit.RData')

AR.f.fit <- stan_glm(AR.model, family = gaussian(), data = X.AR.f,
                     prior = prior_coeff.filtered,
                     adapt_delta = 0.999, QR = FALSE)
save(AR.f.fit, file =  'Ed/HS_AR_f_fit.RData')

## Now returns fit
R.u.fit <- stan_glm(R.model, family = gaussian(), data = X.R.u,
                    prior = prior_coeff.unfiltered,
                    adapt_delta = 0.999,
                    QR = FALSE)
save(R.u.fit, file = 'Ed/HS_R_u_fit.RData')

R.f.fit <- stan_glm(R.model, family = gaussian(), data= X.R.f,
                    prior = prior_coeff.filtered,
                    adapt_delta = 0.999, QR = FALSE)
save(R.f.fit, file = 'Ed/HS_R_f_fit.RData')

# 
# fit <- fit2
# fit <- varsel(fit, method='forward')
# varsel_stats(fit)
# 
# varsel_plot(fit, stats=c('mlpd', 'mse'))
# fit_cv <- cv_varsel(fit, method='forward', cv_method='LOO')
# fit_cv$varsel$ssize
# mcmc_areas(as.matrix(fit), 
#            pars = c('(Intercept)', names(fit$varsel$vind[1:3]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))
# 
# proj <- project(fit, nv = 3, ns = 800)
# mcmc_areas(as.matrix(proj)) + coord_cartesian(xlim = c(-2, 2))
# 
# 
# ########
# covariates.ar <- inner_join(x = CAR.10.unfiltered, y = terror.covariates.subset, by = c('market.date' = 'Date')) %>% 
#   subset(select = -c(Date, event.car, event.t.stat, event.p.value, event.confidence.interval, stars, market.date)) %>% 
#   as.tibble
# 
# 
# full.model2 <- event.ar ~ .
# 
# 
# 
# n <- nrow(terror.covariates.subset)
# D <- ncol(terror.covariates.subset)
# 
# fit3 <- stan_glm(full.model2, family = gaussian(), data = covariates.ar,
#                  prior = prior_coeff,
#                  adapt_delta = 0.999, QR = TRUE, control = list(max_treedepth = 20))
# 
# save(fit3, file = 'Stan_covariate_fit_ar.RData')
