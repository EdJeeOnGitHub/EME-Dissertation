## Graphics for presentation 2
library(rstanarm)
library(ggplot2)

rm(list = ls())
load('AWS Output/Second Attempt/ols_fit_R_u.Rdata')
save.path <- "C:/Users/ed/Documents/R Working Directory/EME-Dissertation/Stanfit Objects/ShinyStan Output/"


#### OLS ####
attack.dummies.ols.plot.R <- plot(ols.fit.R.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.ols.R.png', path = save.path, width = 12, height = 8)
weapon.dummies.ols.plot.R <- plot(ols.fit.R.u, regex_pars = 'weap')
ggsave('weapon.dummies.ols.plot.R.png', width = 12, height = 8, path = save.path)
target.dummies.ols.plot.R <- plot(ols.fit.R.u, regex_pars = 'target')
ggsave('target.dummies.plot.R.png', width = 12, height = 8, path = save.path)


ols.R.ppcheck <- pp_check(ols.fit.R.u) +
  ggtitle('OLS Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('ols.R.ppcheck.png', width = 12, height = 8, path = save.path)


#### LASSO/Laplace ####
load('AWS Output/Second Attempt/LASSO_fit_R_u.Rdata')
lasso.R.ppcheck <- pp_check(laplace.fit.R.u) +
  ggtitle('LASSO Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('laplace.ppcheck.png', path = save.path, width = 12, height = 8)

attack.dummies.laplace.plot.R <- plot(laplace.fit.R.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.laplace.R.png', path = save.path, width = 12, height = 8)
weapon.dummies.laplace.plot.R <- plot(laplace.fit.R.u, regex_pars = 'weap')
ggsave('weapon.dummies.laplace.plot.R.png', width = 12, height = 8, path = save.path)
target.dummies.laplace.plot.R <- plot(laplace.fit.R.u, regex_pars = 'target')
ggsave('target.dummies.plot.R.png', width = 12, height = 8, path = save.path)



#### HS ####
load('AWS Output/Second Attempt/HS_R_u_fit.RData')
HS.fit.R.u <- R.u.fit
attack.dummies.HS.plot.R <- plot(HS.fit.R.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.HS.R.png', path = save.path, width = 12, height = 8)
weapon.dummies.HS.plot.R <- plot(HS.fit.R.u, regex_pars = 'weap')
ggsave('weapon.dummies.HS.plot.R.png', width = 12, height = 8, path = save.path)
target.dummies.HS.plot.R <- plot(HS.fit.R.u, regex_pars = 'target')
ggsave('target.dummies.plot.R.png', width = 12, height = 8, path = save.path)


HS.R.ppcheck <- pp_check(HS.fit.R.u) +
  ggtitle('HS Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('HS.R.ppcheck.png', width = 12, height = 8, path = save.path)

#### HS+ ####
load('AWS Output/Second Attempt/HS_PLUS_R_u_fit.RData')
HS.plus.fit.R.u <- R.u.fit.hsplus
attack.dummies.HS.plus.plot.R <- plot(HS.plus.fit.R.u, regex_pars = 'attack')
ggsave(filename = 'attack.dummies.HS.plus.R.png', path = save.path, width = 12, height = 8)
weapon.dummies.HS.plus.plot.R <- plot(HS.plus.fit.R.u, regex_pars = 'weap')
ggsave('weapon.dummies.HS.plus.plot.R.png', width = 12, height = 8, path = save.path)
target.dummies.HS.plus.plot.R <- plot(HS.plus.fit.R.u, regex_pars = 'target')
ggsave('target.dummies.plot.R.png', width = 12, height = 8, path = save.path)


HS.plus.R.ppcheck <- pp_check(HS.plus.fit.R.u) +
  ggtitle('HS.plus Posterior Predictive Check') +
  scale_color_manual(values = c("blue", "grey")) + # change colors
  scale_size_manual(values = c(0.5, 3)) + # change line sizes 
  scale_fill_manual(values = c(NA, NA)) +  # remove fill
  theme_minimal()
ggsave('HS.plus.R.ppcheck.png', width = 12, height = 8, path = save.path)
launch_shinystan(HS.plus.fit.R.u)
