rm(list = ls())
dev.off()

library(rstanarm)
library(broom)
library(projpred)
library(bayesplot)
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

## OLS CAR4 filtered ####

CAR4.f.plot.1 <- plot(OLS.fit.CAR4.f, pars = non.count.vector) +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events')

CAR4.f.plot.2 <- plot(OLS.fit.CAR4.f, regex_pars =  c('attack', 'prov')) +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events')
CAR4.f.plot.3 <- plot(OLS.fit.CAR4.f, regex_pars = 'target') +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events')
CAR4.f.plot.4 <- plot(OLS.fit.CAR4.f, regex_pars = 'weap') +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events')


CAR4.f.plot.1
CAR4.f.plot.2
CAR4.f.plot.3
CAR4.f.plot.4

## CAR4 Filtered LASSO ####
CAR4.f.laplace.plot.1 <- plot(laplace.fit.CAR4.f, pars = non.count.vector) +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events, LASSO')

CAR4.f.laplace.plot.2 <- plot(laplace.fit.CAR4.f, regex_pars =  c('attack', 'prov')) +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events, LASSO')
CAR4.f.laplace.plot.3 <- plot(laplace.fit.CAR4.f, regex_pars = 'target') +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events, LASSO')
CAR4.f.laplace.plot.4 <- plot(laplace.fit.CAR4.f, regex_pars = 'weap') +
  ggtitle('CAR4 Determinants', subtitle = 'Filtered Events, LASSO')

CAR4.varsel <- varsel(laplace.fit.CAR4.f)
CAR4.var.selected.fit <- mcmc_areas(as.matrix(CAR4.varsel), 
                                    pars = c('(Intercept)', names(CAR4.varsel$varsel$vind[1:5]))) + coord_cartesian(xlim = c(-2, 2)) +
  ggtitle('Variables with greatest predictive power - CAR4')

CAR4.f.laplace.plot.1
CAR4.f.laplace.plot.2
CAR4.f.laplace.plot.3
CAR4.f.laplace.plot.4
CAR4.var.selected.fit


## Returns OLS ####
non.count.vector2 <- replace(non.count.vector, non.count.vector == 'MA4', '`number of articles`')

R.f.plot.1 <- plot(ols.fit.R.u, pars = non.count.vector2) +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events')

R.f.plot.2 <- plot(ols.fit.R.u, regex_pars =  c('attack', 'prov')) +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events')
R.f.plot.3 <- plot(ols.fit.R.u, regex_pars = 'target') +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events')
R.f.plot.4 <- plot(ols.fit.R.u, regex_pars = 'weap') +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events')


R.f.plot.1
R.f.plot.2
R.f.plot.3
R.f.plot.4


## Returns LASSO ####
laplace.f.plot.1 <- plot(laplace.fit.R.u, pars = non.count.vector2) +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events, LASSO')

laplace.f.plot.2 <- plot(laplace.fit.R.u, regex_pars =  c('attack', 'prov')) +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events, LASSO')
laplace.f.plot.3 <- plot(laplace.fit.R.u, regex_pars = 'target') +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events, LASSO')
laplace.f.plot.4 <- plot(laplace.fit.R.u, regex_pars = 'weap') +
  ggtitle('Terror Day Return Determinants', subtitle = 'All Events, LASSO')

laplace.R.varsel <- varsel(laplace.fit.R.u)
laplace.R.var.selected.fit <- mcmc_areas(as.matrix(laplace.R.varsel), 
                                    pars = c('(Intercept)', names(laplace.R.varsel$varsel$vind[1:5]))) + coord_cartesian(xlim = c(-0.05, 0.05)) +
  ggtitle('Variables with greatest predictive power - Terror Day Returns')
laplace.f.plot.1
laplace.f.plot.2
laplace.f.plot.3
laplace.f.plot.4
laplace.R.var.selected.fit



## Notes: Have 51 predictors and only 88 observations of filtered events.
## Variable selection techniques with CAR4 data suggest wounded and media intensity biggest determinants but don't have enough power to claim they're significantly different from 0.
## Variable selection with terror return data indicates weaptype are biggest culprits but again very close to 0 - not sure how to interpret this.
## Assuming events are iid - could possibly look into relaxing this assumption?

