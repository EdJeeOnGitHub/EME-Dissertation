rm(list = ls())
library(gEcon)
path <- "C:/Users/nfa/Dropbox/Ed/Ed Uni work/EME/R/rbc_attempt1.gcn"
rbc_ic <- make_model(path)

rbc_ic <- steady_state(rbc_ic)
get_ss_values(rbc_ic, to_tex = FALSE)
get_par_values(rbc_ic, to_tex = FALSE)

rbc_ic <- solve_pert(rbc_ic)
get_pert_solution(rbc_ic, to_tex = FALSE)

rbc_ic <- set_shock_cov_mat(model = rbc_ic,
                            cov_matrix = matrix(c(0.01), 1, 1), 
                            shock_order = 'epsilon_Z')

rbc_ic <- compute_model_stats(model = rbc_ic,
                              n_leadlags = 6,
                              ref_var = 'Y')

get_model_stats(rbc_ic, 
                to_tex = FALSE)

rbc_ic_irf <- compute_irf(model = rbc_ic,
                          variables = c( 'C', 'K_s', 'Z', 'Y', 'L_s', 'I'),
                          sim_length = 40)

plot_simulation(rbc_ic_irf)
summary(rbc_ic)
summary(rbc_ic_irf)



############################################
## WIP#####################################
###########################################

dev.off()
library(gEcon.estimation)

file.copy(from = file.path(system.file('examples', package = 'gEcon.estimation'),
                           'dsge_model.gcn'), to = getwd())

dsge_model <- make_model('dsge_model.gcn')

# Solving model
dsge_model <- steady_state(dsge_model)
dsge_model <- solve_pert(dsge_model, loglin = TRUE)

# Setting random shock params

dsge_model <- set_shock_distr_par(dsge_model,
                                  distr_par = list( 'sd(epsilon_G)' = 0.01,
                                                    'sd( epsilon_Z)' = 0.01))

shock_info(model = dsge_model, all = TRUE)


set.seed(250)
series_length <- 150
observables <- c( 'Y', 'G')

# Simulating random path
dsge_simulation <- random_path(model = dsge_model,
                               sim_length = series_length,
                               variables = observables)

model_data <- get_simulation_results(dsge_simulation)

# Create data set for estimation
estimation_data <- ts(data = t(model_data)[, observables],
                               start = c(1973, 1),
                               frequency = 4,
                               names = observables)

# Demeaning
mean_var <- matrix(apply(estimation_data, 2, mean),
                   byrow = TRUE,
                   nrow = nrow(estimation_data),
                   ncol = ncol(estimation_data))

estimation_data <- estimation_data - mean_var

# Declaring Priors

dsge_prior <- gecon_prior(
  prior_list = list(
    list(par = 'sd(epsilon_Z)', type = 'inv_gamma',
         mean = 0.012, sd = 0.3, lower_bound = 0.0001,
         upper_bound = 0.9, initial = 0.0012),
    list(par = 'sd(epsilon_G)', type = 'inv_gamma',
         mean = 0.008, sd = 0.3, lower_bound = 0.0001,
         upper_bound = 0.9, initial = 0.006),
    list(par = 'omega', type = 'normal',
         mean = 1.45, sd = 0.1, lower_bound = 1,
         upper_bound = 2, initial = 1.5),
    list(par = 'phi_G', type = 'beta',
         mean = 0.88, sd = 0.03, lower_bound = 0.5,
         upper_bound = 0.999, initial = 0.95),
    list(par = 'phi_Z', type = 'beta',
         mean = 0.92, sd = 0.03, lower_bound = 0.5,
         upper_bound = 0.999, initial = 0.95)),
  model = dsge_model
  )

plot_prior(dsge_prior)


# Estimating the model (Bayesian-ly)

estimation_result <- bayesian_estimation(data_set = estimation_data,
                                         optim_options_list = list(solver = 'csminwel'),
                                         mcmc_options_list = list( chain_length = 1000,
                                                                   burn = 200,
                                                                   cores = 2, chains = 2,
                                                                   scale = rep(0.5, 5)),
                                         observables = observables,
                                         model = dsge_model,
                                         prior = dsge_prior)


plot_posterior(estimation_result)

# true model parameters were:
# sd(epsilon_Z) 0.01
# sd(epsilon_G) 0.01
# omega 1.45
# phi_G 0.9
# phi_Z 0.9
est_par <- get_estimated_par(estimation_result)
free_par <- est_par$free_par
shock_distr_par <- est_par$shock_distr_par

estimated_dsge_model <- set_free_par(dsge_model, free_par = free_par)
estimated_dsge_model <- set_shock_distr_par(estimated_dsge_model, distr_par = shock_distr_par)

estimated_dsge_model <- steady_state(estimated_dsge_model)
estimated_dsge_model <- solve_pert(estimated_dsge_model, loglin = TRUE)


# Historical shock decomposition and variable smoothing
dsge_shock_decomp <- shock_decomposition(model = estimated_dsge_model,
                                         data_set = window(estimation_data,
                                                           start = c(2004, 1),
                                                           end = c(2010, 1),
                                                           frequency = 4),
                                         observables = observables,
                                         variables = observables)

plot_shock_decomposition(dsge_shock_decomp)

# Kalman smoothing
dsge_smoothed_variables <- smoother(model = estimated_dsge_model,
                                    data_set = estimation_data,
                                    observables = c('Y', 'G'),
                                    variables = c('K', 'I', 'C'))

# dsge_smoothed_variables$smoothed_shock
# 
# dsge_smoothed_variables$smoothed_var
# 
# dsge_smoothed_variables$MSE


# Forecasting

fc_res <- forecast(model = estimated_dsge_model,
                        data_set = estimation_data,
                        observables = observables,
                        variables = c('Y', 'G'),
                        horizon = 20)

fc_res_post <- forecast_posterior(est_results = estimation_result,
                                  data_set = estimation_data,
                                  observables = observables,
                                  variables = c('Y', 'G'),
                                  horizon = 20)

plot_forecast(fc_res_post)
plot_forecast(fc_res)
