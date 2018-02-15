## Second stage of analysis exploring heterogeneous effects of terror
rm(list = ls())
sink(file = '~/Dropbox/Ed/AWS Output/AWS_log.txt')
library(tidyverse)
library(projpred)
library(rstanarm)
options(mc.cores = parallel::detectCores())
source('DissertationFunctions.R')

#### Merging terror covariates and return data ####
load('TerrorCovariates_subtype.Rdata')
load('AnalysisOutput/CAR_Data.RData')

# CAR10s
merged.data.u.10 <- merge.and.drop.covariates(CAR.10.unfiltered, terror.covariates.subset)

merged.data.f.10 <- merge.and.drop.covariates(CAR.10.filtered, terror.covariates.subset)

# CAR4s
merged.data.u.4 <- merge.and.drop.covariates(CAR.4.unfiltered, terror.covariates.subset)
merged.data.f.4 <- merge.and.drop.covariates(CAR.4.filtered, terror.covariates.subset)



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




#### Laplace (LASSO) models ####

# Returns

laplace.fit.R.f <- stan_glm(R.model, family = gaussian(), data = X.R.f,
                            prior = lasso())
save(laplace.fit.R.f, file = '~/Dropbox/Ed/AWS Output/LASSO_fit_R_f.Rdata')

laplace.fit.R.u <- stan_glm(R.model, family = gaussian(), data = X.R.u,
                            prior = lasso())
save(laplace.fit.R.u, file = '~/Dropbox/Ed/AWS Output/LASSO_fit_R_u.Rdata')

# CAR4s
laplace.fit.CAR4.f <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.f,
                               prior = lasso())
save(laplace.fit.CAR4.f, file = '~/Dropbox/Ed/AWS Output/LASSO_fit_CAR4_f.Rdata')

laplace.fit.CAR4.u <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.u, prior = lasso())
save(laplace.fit.CAR4.u, file =  '~/Dropbox/Ed/AWS Output/LASSO_fit_CAR4_u.Rdata')

# CAR10s
laplace.fit.CAR10.f <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f,
                               prior = lasso())
save(laplace.fit.CAR10.f, file = '~/Dropbox/Ed/AWS Output/LASSO_fit_CAR10_f.Rdata')

laplace.fit.CAR10.u <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u, prior = lasso())
save(laplace.fit.CAR10.u, file =  '~/Dropbox/Ed/AWS Output/LASSO_fit_CAR10_u.Rdata')

# ARs

laplace.fit.AR.f <- stan_glm(AR.model, family = gaussian(), data = X.AR.f,
                            prior = lasso())
save(laplace.fit.AR.f, file = '~/Dropbox/Ed/AWS Output/LASSO_fit_AR_f.Rdata')

laplace.fit.AR.u <- stan_glm(AR.model, family = gaussian(), data = X.AR.u,
                            prior = lasso())
save(laplace.fit.AR.u, file = '~/Dropbox/Ed/AWS Output/LASSO_fit_AR_u.Rdata')

#### Horseshoe models ####
# CARs

CAR.10.f.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f,
                 prior = calculate.hs.priors(X.CAR10.f),
                 adapt_delta = 0.9999, control = list(max_treedepth = 20))

save(CAR.10.f.fit, file = '~/Dropbox/Ed/AWS Output/HS_CAR10_f_fit.RData')


CAR.10.u.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u,
                         prior = calculate.hs.priors(X.CAR10.u),
                         adapt_delta = 0.9999, QR = FALSE, control = list(max_treedepth = 20))
save(CAR.10.u.fit, file = '~/Dropbox/Ed/AWS Output/HS_CAR10_u_fit.Rdata')

CAR.4.f.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.f,
                        prior =calculate.hs.priors(X.CAR4.f),
                        adapt_delta = 0.9999, QR = FALSE)
save(CAR.4.f.fit, file =  '~/Dropbox/Ed/AWS Output/HS_CAR4_f_fit.RData')

CAR.4.u.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.u,
                        prior = calculate.hs.priors(X.CAR4.u),
                        adapt_delta = 0.9999, QR = FALSE)
save(CAR.4.u.fit, file = '~/Dropbox/Ed/AWS Output/HS_CAR4_u_fit.RData')
## Now ARs
AR.u.fit <- stan_glm(AR.model, family = gaussian(), data = X.AR.u,
                     prior = calculate.hs.priors(X.AR.u),
                     adapt_delta = 0.9999, QR = FALSE)
save(AR.u.fit, file = '~/Dropbox/Ed/AWS Output/HS_AR_u_fit.RData')

AR.f.fit <- stan_glm(AR.model, family = gaussian(), data = X.AR.f,
                     prior = calculate.hs.priors(X.AR.f),
                     adapt_delta = 0.9999, QR = FALSE)
save(AR.f.fit, file =  '~/Dropbox/Ed/AWS Output/HS_AR_f_fit.RData')

## Now returns fit
R.u.fit <- stan_glm(R.model, family = gaussian(), data = X.R.u,
                    prior = calculate.hs.priors(X.R.u),
                    adapt_delta = 0.9999,
                    QR = FALSE)
save(R.u.fit, file = '~/Dropbox/Ed/AWS Output/HS_R_u_fit.RData')

R.f.fit <- stan_glm(R.model, family = gaussian(), data= X.R.f,
                    prior = calculate.hs.priors(X.R.f),
                    adapt_delta = 0.9999, QR = FALSE)
save(R.f.fit, file = '~/Dropbox/Ed/AWS Output/HS_R_f_fit.RData')
 

#### Horseshoe+ models ####

# CARs

CAR.10.f.fit.hsplus <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f,
                         prior = calculate.hsplus.priors(X.CAR10.f),
                         adapt_delta = 0.9999, control = list(max_treedepth = 20))

save(CAR.10.f.fit.hsplus, file = '~/Dropbox/Ed/AWS Output/HS_PLUS_CAR10_f_fit.RData')


CAR.10.u.fit.hsplus <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u,
                         prior = calculate.hsplus.priors(X.CAR10.u),
                         adapt_delta = 0.9999, QR = FALSE, control = list(max_treedepth = 20))
save(CAR.10.u.fit.hsplus, file = '~/Dropbox/Ed/AWS Output/HS_PLUS_CAR10_u_fit.Rdata')

CAR.4.f.fit.hsplus <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.f,
                        prior =calculate.hsplus.priors(X.CAR4.f),
                        adapt_delta = 0.9999, QR = FALSE)
save(CAR.4.f.fit.hsplus, file =  '~/Dropbox/Ed/AWS Output/HS_PLUS_CAR4_f_fit.RData')

CAR.4.u.fit.hsplus <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.u,
                        prior = calculate.hsplus.priors(X.CAR4.u),
                        adapt_delta = 0.9999, QR = FALSE)
save(CAR.4.u.fit.hsplus, file = '~/Dropbox/Ed/AWS Output/HS_PLUS_CAR4_u_fit.RData')
## Now ARs
AR.u.fit.hsplus <- stan_glm(AR.model, family = gaussian(), data = X.AR.u,
                     prior = calculate.hsplus.priors(X.AR.u),
                     adapt_delta = 0.9999, QR = FALSE)
save(AR.u.fit.hsplus, file = '~/Dropbox/Ed/AWS Output/HS_PLUS_AR_u_fit.RData')

AR.f.fit.hsplus <- stan_glm(AR.model, family = gaussian(), data = X.AR.f,
                     prior = calculate.hsplus.priors(X.AR.f),
                     adapt_delta = 0.9999, QR = FALSE)
save(AR.f.fit.hsplus, file =  '~/Dropbox/Ed/AWS Output/HS_PLUS_AR_f_fit.RData')

## Now returns fit
R.u.fit.hsplus <- stan_glm(R.model, family = gaussian(), data = X.R.u,
                    prior = calculate.hsplus.priors(X.R.u),
                    adapt_delta = 0.9999,
                    QR = FALSE)
save(R.u.fit.hsplus, file = '~/Dropbox/Ed/AWS Output/HS_PLUS_R_u_fit.RData')

R.f.fit.hsplus <- stan_glm(R.model, family = gaussian(), data= X.R.f,
                    prior = calculate.hsplus.priors(X.R.f),
                    adapt_delta = 0.9999, QR = FALSE)
save(R.f.fit.hsplus, file = '~/Dropbox/Ed/AWS Output/HS_PLUS_R_f_fit.RData')