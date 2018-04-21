## Second stage of analysis exploring heterogeneous effects of terror
rm(list = ls())




library(tidyverse)
library(rstanarm)
library(RPushbullet)
options(mc.cores = parallel::detectCores())
source('DissertationFunctions.R')

#### Merging terror covariates and return data ####
load('TerrorCovariates_subtype_media.Rdata')
load('AnalysisOutput/CAR_Data.RData')

terror.covariates.subset <- terror.covariates.subset.media
# CAR10s
merged.data.u.10 <- merge.and.drop.covariates(CAR.10.unfiltered, terror.covariates.subset)
merged.data.f.10 <- merge.and.drop.covariates(CAR.10.filtered, terror.covariates.subset)



## Creating dataframes that only contain regressors of interest. This makes it easier to use . notation when specifying models


# These are all CARs

X.CAR10.u <- merged.data.u.10 %>% 
  subset(select = -c(event.ar,
                     returns,
                     `number of articles`,
                     MA4))

X.CAR10.f <- merged.data.f.10 %>% 
  subset(select = -c(event.ar,
                     returns,
                     `number of articles`,
                     MA4))

## Many of the filtered columns contain variables that are always switched off, so I drop these to make variable selection easier
remove.constant.cols <- function(dataframe){
  df <- dataframe[, apply(dataframe, 2, var, na.rm = TRUE) != 0]
  return(df)
}


X.CAR10.f <- remove.constant.cols(X.CAR10.f)
X.CAR10.u <- remove.constant.cols(X.CAR10.u)

#### Creating Model Formulae #####
CAR.model <- event.car ~ .

pbPost('note', 'Starting CAR10 Simulations', as.character(Sys.time()))


#### Standard OLS models ####


# CAR10s
OLS.fit.CAR10.f <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f)
save(OLS.fit.CAR10.f, file = '~/Dropbox/Ed/AWS Output/CAR10/OLS_fit_CAR10_f.Rdata')

OLS.fit.CAR10.u <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u)
save(OLS.fit.CAR10.u, file =  '~/Dropbox/Ed/AWS Output/CAR10/OLS_fit_CAR10_u.Rdata')

pbPost('note', 'Model Completed', body = 'CAR10 OLS completed')

#### Laplace (LASSO) models ####



# CAR10s
laplace.fit.CAR10.f <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f,
                               prior = lasso())
save(laplace.fit.CAR10.f, file = '~/Dropbox/Ed/AWS Output/CAR10/LASSO_fit_CAR10_f.Rdata')

laplace.fit.CAR10.u <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u, prior = lasso())
save(laplace.fit.CAR10.u, file =  '~/Dropbox/Ed/AWS Output/CAR10/LASSO_fit_CAR10_u.Rdata')

pbPost('note', 'Model Completed', body = 'CAR10 LASSOs completed')
#### Horseshoe models ####
# CARs
CAR.10.f.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.f,
                        prior =calculate.hs.priors(X.CAR10.f),
                        adapt_delta = 0.999999999999, QR = FALSE)
save(CAR.10.f.fit, file =  '~/Dropbox/Ed/AWS Output/CAR10/HS_CAR10_f_fit.RData')

CAR.10.u.fit <- stan_glm(CAR.model, family = gaussian(), data = X.CAR10.u,
                        prior = calculate.hs.priors(X.CAR10.u),
                        adapt_delta = 0.999999999999, QR = FALSE)
save(CAR.10.u.fit, file = '~/Dropbox/Ed/AWS Output/CAR10/HS_CAR10_u_fit.RData')

pbPost('note', 'Model Completed', body = 'CAR10 Horseshoe Models Finished')


pbPost('note', 'Simulations Finished:' , body = as.character(Sys.time()))

