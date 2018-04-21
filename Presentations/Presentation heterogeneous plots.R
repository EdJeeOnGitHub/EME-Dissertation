## Graphics for presentation 2
library(rstanarm)
library(ggplot2)

rm(list = ls())
load('AWS Output/Third Attempt/OLS_fit_CAR4_u.Rdata')
save.path <- "C:/Users/ed/Documents/R Working Directory/EME-Dissertation/Stanfit Objects/ShinyStan Output/"


#### OLS ####
attack.dummies.OLS.plot.CAR4 <- plot(OLS.fit.CAR4.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.OLS.CAR4.png', path = save.path, width = 12, height = 8)
weapon.dummies.OLS.plot.CAR4 <- plot(OLS.fit.CAR4.u, regex_pars = 'weap')
ggsave('weapon.dummies.OLS.plot.CAR4.png', width = 12, height = 8, path = save.path)
target.dummies.OLS.plot.CAR4 <- plot(OLS.fit.CAR4.u, regex_pars = 'target')
ggsave('target.dummies.OLS.plot.CAR4.png', width = 12, height = 8, path = save.path)


OLS.CAR4.ppcheck <- pp_check(OLS.fit.CAR4.u) +
  ggtitle('OLS Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('OLS.CAR4.ppcheck.png', width = 12, height = 8, path = save.path)


#### LASSO/Laplace ####
load('AWS Output/Third Attempt/LASSO_fit_CAR4_u.Rdata')
lasso.CAR4.ppcheck <- pp_check(laplace.fit.CAR4.u) +
  ggtitle('LASSO Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('laplace.ppcheck.png', path = save.path, width = 12, height = 8)

attack.dummies.laplace.plot.CAR4 <- plot(laplace.fit.CAR4.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.laplace.CAR4.png', path = save.path, width = 12, height = 8)
weapon.dummies.laplace.plot.CAR4 <- plot(laplace.fit.CAR4.u, regex_pars = 'weap')
ggsave('weapon.dummies.laplace.plot.CAR4.png', width = 12, height = 8, path = save.path)
target.dummies.laplace.plot.CAR4 <- plot(laplace.fit.CAR4.u, regex_pars = 'target')
ggsave('target.dummies.laplace.plot.CAR4.png', width = 12, height = 8, path = save.path)



#### HS ####
load('AWS Output/Third Attempt/HS_CAR4_u_fit.RData')
HS.fit.CAR4.u <- CAR.4.u.fit
attack.dummies.HS.plot.CAR4 <- plot(HS.fit.CAR4.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.HS.CAR4.png', path = save.path, width = 12, height = 8)
weapon.dummies.HS.plot.CAR4 <- plot(HS.fit.CAR4.u, regex_pars = 'weap')
ggsave('weapon.dummies.HS.plot.CAR4.png', width = 12, height = 8, path = save.path)
target.dummies.HS.plot.CAR4 <- plot(HS.fit.CAR4.u, regex_pars = 'target')
ggsave('target.dummies.plot.CAR4.png', width = 12, height = 8, path = save.path)


HS.CAR4.ppcheck <- pp_check(HS.fit.CAR4.u) +
  ggtitle('HS Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('HS.CAR4.ppcheck.png', width = 12, height = 8, path = save.path)

#### HS+ ####
load('AWS Output/Third Attempt/HS_PLUS_CAR4_u_fit.RData')
HS.plus.fit.CAR4.u <- CAR4.u.fit.hsplus
attack.dummies.HS.plus.plot.CAR4 <- plot(HS.plus.fit.CAR4.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.HS.plus.CAR4.png', path = save.path, width = 12, height = 8)
weapon.dummies.HS.plus.plot.CAR4 <- plot(HS.plus.fit.CAR4.u, regex_pars = 'weap')
ggsave('weapon.dummies.HS.plus.plot.CAR4.png', width = 12, height = 8, path = save.path)
target.dummies.HS.plus.plot.CAR4 <- plot(HS.plus.fit.CAR4.u, regex_pars = 'target')
ggsave('target.dummies.plot.CAR4.png', width = 12, height = 8, path = save.path)


HS.plus.CAR4.ppcheck <- pp_check(HS.plus.fit.CAR4.u) +
  ggtitle('HS.plus Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('HS.plus.CAR4.ppcheck.png', width = 12, height = 8, path = save.path)
launch_shinystan(HS.plus.fit.CAR4.u)



## Shiny Stan part
# launch_shinystan(OLS.fit.CAR4.u)
# launch_shinystan(laplace.fit.CAR4.u)
# launch_shinystan(CAR.4.u.fit)
