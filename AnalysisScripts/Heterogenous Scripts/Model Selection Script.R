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

laplace.CAR.varsel <- varsel(laplace.fit.CAR4.f)
fit_cv <- cv_varsel(laplace.CAR.varsel, method='forward')
laplace.CAR.var.CV.plot <- mcmc_areas(as.matrix(fit_cv),
                                    pars = c('(Intercept)', names(fit_cv$varsel$vind[1:7]))) +
  xlim(-3,3)


laplace.CAR.var.Varsel.plot <- mcmc_areas(as.matrix(laplace.CAR.varsel),
                                     pars = c('(Intercept)', names(laplace.CAR.varsel$varsel$vind[1:5]))) +
  ggtitle('Variables with greatest predictive power - 4-day CARs')

laplace.CAR.var.CV.plot
laplace.CAR.var.Varsel.plot
varsel_plot(laplace.CAR.varsel)
varsel_plot(fit_cv, stats = c('mlpd', 'mse'))

laplace.CAR.proj <- project(fit_cv, nv = 7)
projected.laplace.plot <- mcmc_areas(as.matrix(laplace.CAR.proj))



#####

loo.LASSO<- loo(log_lik(laplace.fit.CAR4.f))
loo.OLS <- loo(log_lik(OLS.fit.CAR4.f))



loo.LASSO
loo.OLS


loo.test <- compare(loo.LASSO, loo.OLS)

waic.LASSO <-  waic(log_lik(laplace.fit.CAR4.f))
waic.OLS <- waic(log_lik(OLS.fit.CAR4.f))

waic.LASSO
waic.OLS

compare(waic.LASSO, waic.OLS)