rm(list = ls())

library(rstanarm)
library(broom)
library(projpred)
library(bayesplot)
load('AWS Output/CAR4/OLS_fit_CAR4_f.Rdata')
launch_shinystan(OLS.fit.CAR4.f)

plot(OLS.fit.CAR4.f)

load('AWS Output/CAR4/LASSO_fit_CAR4_f.Rdata')
launch_shinystan(laplace.fit.CAR4.f)

plot(laplace.fit.CAR4.f)

fit_v <- varsel(laplace.fit.CAR4.f)
varsel_plot(fit_v)


fit_v$varsel$ssize


mcmc_areas(as.matrix(fit_v), 
           pars = c('(Intercept)', names(fit_v$varsel$vind[1:5]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))
load('AWS Output/CAR4/HS_CAR4_f_fit.RData')
launch_shinystan(CAR.4.f.fit)

load('AWS Output/CAR4/LASSO_fit_R_f.Rdata')
launch_shinystan(laplace.fit.R.f)


R_vs <- varsel(laplace.fit.R.f)
varsel_plot(R_vs)
mcmc_areas(as.matrix(R_vs),
           pars = c('(Intercept)', names(R_vs$varsel$vind[1:3]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))
)