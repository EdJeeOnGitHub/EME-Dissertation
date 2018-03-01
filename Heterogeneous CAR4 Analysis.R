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
# CAR4s
merged.data.u.4 <- merge.and.drop.covariates(CAR.4.unfiltered, terror.covariates.subset)
merged.data.f.4 <- merge.and.drop.covariates(CAR.4.filtered, terror.covariates.subset)



## Creating dataframes that only contain regressors of interest. This makes it easier to use . notation when specifying models


# These are all CARs

X.CAR4.u <- merged.data.u.4 %>% 
  subset(select = -c(event.ar,
                     returns,
                     `number of articles`,
                     MA10))

X.CAR4.f <- merged.data.f.4 %>% 
  subset(select = -c(event.ar,
                     returns,
                     `number of articles`,
                     MA10))
X.R.u <- merged.data.u.4 %>% 
  subset(select = -c(event.ar,
                     event.car,
                     MA4,
                     MA10))

## Many of the filtered columns contain variables that are always switched off, so I drop these to make variable selection easier
remove.constant.cols <- function(dataframe){
  df <- dataframe[, apply(dataframe, 2, var, na.rm = TRUE) != 0]
  return(df)
}


X.CAR4.f <- remove.constant.cols(X.CAR4.f)
X.CAR4.u <- remove.constant.cols(X.CAR4.u)
X.R.u <- remove.constant.cols(X.R.u)

#### Creating Model Formulae #####
CAR.model <- event.car ~ .
R.model <- returns ~ .

pbPost('note', 'Starting CAR4 Simulations', as.character(Sys.time()))


#### Standard OLS models ####


# CAR4s
OLS.fit.CAR4.f <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.f)
save(OLS.fit.CAR4.f, file = '~/Dropbox/Ed/AWS Output/CAR4/OLS_fit_CAR4_f.Rdata')

OLS.fit.CAR4.u <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.u)
save(OLS.fit.CAR4.u, file =  '~/Dropbox/Ed/AWS Output/CAR4/OLS_fit_CAR4_u.Rdata')

pbPost('note', 'Model Completed', body = 'CAR4 OLS completed')

#### Laplace (LASSO) models ####



# CAR4s
laplace.fit.CAR4.f <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.f,
                               prior = lasso())
save(laplace.fit.CAR4.f, file = '~/Dropbox/Ed/AWS Output/CAR4/LASSO_fit_CAR4_f.Rdata')

laplace.fit.CAR4.u <- stan_glm(CAR.model, family = gaussian(), data = X.CAR4.u, prior = lasso())
save(laplace.fit.CAR4.u, file =  '~/Dropbox/Ed/AWS Output/CAR4/LASSO_fit_CAR4_u.Rdata')

pbPost('note', 'Model Completed', body = 'CAR4 LASSOs completed')


pbPost('note', 'Simulations Finished:' , body = as.character(Sys.time()))


#### Returns ####
ols.fit.R.u <- stan_glm(R.model, data = X.R.u)
save(ols.fit.R.u, file = '~/Dropbox/Ed/AWS Output/CAR4/ols_fit_R_u.Rdata')

laplace.fit.R.u <- stan_glm(R.model, family = gaussian(), data = X.R.u,
                            prior = lasso())
save(laplace.fit.R.u, file = '~/Dropbox/Ed/AWS Output/CAR4/LASSO_fit_R_u.Rdata')
