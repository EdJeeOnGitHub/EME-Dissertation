### RBC - Epstein-Zin, No terror threat and no transformed asset price/return variable
rm(list = ls())

library(gEcon)
path <- "C:/Users/nfa/Dropbox/Ed/Ed Uni work/EME/R/DSGE GCH Files/EZ_No_T.gcn"
DSGE_model <- make_model(path)

DSGE_model <- steady_state(DSGE_model)
DSGE_model <- solve_pert(DSGE_model)

DSGE_model <- set_shock_distr_par(DSGE_model,
                                  distr_par = list( 'sd(epsilon_Z)' = 0.5))
DSGE_model <- compute_model_stats(DSGE_model, ref_var = 'Y')
get_model_stats(DSGE_model)


IRF_DSGE <- compute_irf(DSGE_model)
plot_simulation(IRF_DSGE)




summary(DSGE_model)