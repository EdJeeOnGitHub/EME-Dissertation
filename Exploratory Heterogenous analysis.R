library(rstanarm)
library(broom)
library(projpred)
library
load('AWS Output/Current Attempt/OLS_fit_CAR4_f.Rdata')
launch_shinystan(OLS.fit.CAR4.f)

load('AWS Output/Current Attempt/LASSO_fit_CAR4_f.Rdata')
launch_shinystan(laplace.fit.CAR4.f)

fit_v <- varsel(laplace.fit.CAR4.f)
varsel_plot(fit_v)

fit_cv <- cv_varsel(laplace.fit.CAR4.f)
fit_cv
fit_cv$varsel$ssize
fit_v$varsel$ssize


mcmc_areas(as.matrix(fit_cv), 
           pars = c('(Intercept)', names(fit_cv$varsel$vind[1:3]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))
load('AWS Output/Current Attempt/HS_CAR4_f_fit.RData')
launch_shinystan(CAR.4.f.fit)

load('AWS Output/Current Attempt/LASSO_fit_R_f.Rdata')
launch_shinystan(laplace.fit.R.f)


R_vs <- varsel(laplace.fit.R.f)
varsel_plot(R_vs)
mcmc_areas(as.matrix(R_vs),
           pars = c('(Intercept)', names(R_vs$varsel$vind[1:3]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))
)