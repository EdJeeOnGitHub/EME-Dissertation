rm(list = ls())
dev.off()

library(rstanarm)
library(broom)
library(projpred)
library(bayesplot)
library(loo)
#OLS
load('AWS Output/CAR4/OLS_fit_CAR4_f.Rdata')
# load('AWS Output/CAR4/OLS_fit_CAR4_u.Rdata')
#LASSO
# load('AWS Output/CAR4/LASSO_fit_CAR4_u.Rdata')
load('AWS Output/CAR4/LASSO_fit_CAR4_f.Rdata')

# OLS and LASSO Returns
load('AWS Output/CAR4/ols_fit_R_u.Rdata')
load('AWS Output/CAR4/LASSO_fit_R_u.Rdata')
non.count.vector <- c(
  "(Intercept)",
  "multiple",
  "success",
  "guncertain1",
  "nperps",
  "nperpcap",
  "nkill",
  "nwound",
  "nwoundte" ,
  "property" ,
  "ishostkid",
  "nhostkid",
  "INT_IDEO",
  "incident",
  'MA4',
  'T'
)


## Returns Projection Prediction LASSO ####
# 
# laplace.CAR.varsel <- varsel(laplace.fit.CAR4.f)
# fit_cv <- cv_varsel(laplace.CAR.varsel, method='forward')
# laplace.R.var.selected.fit <- mcmc_areas(as.matrix(laplace.R.varsel), 
#                                     pars = c('(Intercept)', names(laplace.R.varsel$varsel$vind[1:5]))) + coord_cartesian(xlim = c(-0.05, 0.05)) +
#   ggtitle('Variables with greatest predictive power - Terror Day Returns')
# laplace.f.plot.1
# laplace.f.plot.2
# laplace.f.plot.3
# laplace.f.plot.4
# laplace.R.var.selected.fit
#####

loo.LASSO<- loo(log_lik(laplace.fit.CAR4.f))
loo.OLS <- loo(log_lik(OLS.fit.CAR4.f))

loo.LASSO
loo.OLS

compare(loo.LASSO, loo.OLS)

waic.LASSO <-  waic(log_lik(laplace.fit.CAR4.f))
waic.OLS <- waic(log_lik(OLS.fit.CAR4.f))

waic.LASSO
waic.OLS

compare(waic.LASSO, waic.OLS)