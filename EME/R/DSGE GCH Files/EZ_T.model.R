# Generated on 2018-01-18 18:30:27 by gEcon ver. 1.0.2 (2016-12-05)
# http://gecon.r-forge.r-project.org/

# Model name: EZ_T

# info
info__ <- c("EZ_T", "C:/Users/nfa/Dropbox/Ed/Ed Uni work/EME/R/DSGE GCH Files/EZ_T.gcn", "2018-01-18 18:30:27")

# index sets
index_sets__ <- list()

# variables
variables__ <- c("q__CONSUMER_1",
                 "r",
                 "C",
                 "I",
                 "K_s",
                 "U",
                 "W",
                 "Y",
                 "Z")

variables_tex__ <- c("q^{\\mathrm{CONSUMER}^{\\mathrm{1}}}",
                     "r",
                     "C",
                     "I",
                     "K^{\\mathrm{s}}",
                     "U",
                     "W",
                     "Y",
                     "Z")

# shocks
shocks__ <- c("epsilon_Z",
              "epsilon_T")

shocks_tex__ <- c("\\epsilon^{\\mathrm{Z}}",
                  "\\epsilon^{\\mathrm{T}}")

# parameters
parameters__ <- c("alpha",
                  "beta",
                  "delta",
                  "eta",
                  "phi",
                  "theta_EZ")

parameters_tex__ <- c("\\alpha",
                     "\\beta",
                     "\\delta",
                     "\\eta",
                     "\\phi",
                     "\\theta^{\\mathrm{EZ}}")

# free parameters
parameters_free__ <- c("beta",
                       "delta",
                       "eta",
                       "phi",
                       "theta_EZ")

# free parameters' values
parameters_free_val__ <- c(0.99,
                           0.025,
                           2,
                           0.95,
                           0.05)

# equations
equations__ <- c("q__CONSUMER_1[] - E[][U[1]^(1 - theta_EZ)] = 0",
                 "-r[] + alpha * Z[] * 1^(1 - alpha) * K_s[-1]^(-1 + alpha) = 0",
                 "-W[] + Z[] * (1 - alpha) * 1^(-alpha) * K_s[-1]^alpha = 0",
                 "-Y[] + Z[] * 1^(1 - alpha) * K_s[-1]^alpha = 0",
                 "-Z[] + exp(epsilon_Z[] - epsilon_T[] + phi * log(Z[-1])) = 0",
                 "beta * q__CONSUMER_1[]^(-1 + (1 - theta_EZ)^-1) * E[][(r[1] * C[1]^(-eta) + (1 - delta) * C[1]^(-eta)) * U[1]^(-theta_EZ)] - C[]^(-eta) = 0",
                 "-C[] - I[] + Y[] = 0",
                 "U[] - beta * q__CONSUMER_1[]^((1 - theta_EZ)^-1) - (-1 + C[]^(1 - eta)) * (1 - eta)^-1 = 0",
                 "-epsilon_T[] + I[] - K_s[] + K_s[-1] * (1 - delta) = 0")

# calibrating equations
calibr_equations__ <- c("-0.36 * Y[ss] + r[ss] * K_s[ss] = 0")

# variables / equations map
vareqmap__ <- sparseMatrix(i = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4,
                                 4, 5, 6, 6, 6, 6, 7, 7, 7, 8,
                                 8, 8, 9, 9),
                           j = c(1, 6, 2, 5, 9, 5, 7, 9, 5, 8,
                                 9, 9, 1, 2, 3, 6, 3, 4, 8, 1,
                                 3, 6, 4, 5),
                           x = c(2, 4, 2, 1, 2, 1, 2, 2, 1, 2,
                                 2, 3, 2, 4, 6, 4, 2, 2, 2, 2,
                                 2, 2, 2, 3),
                           dims = c(9, 9))

# variables / calibrating equations map
varcalibreqmap__ <- sparseMatrix(i = c(1, 1, 1),
                                 j = c(2, 5, 8),
                                 x = rep(1, 3), dims = c(1, 9))

# calibrated parameters / equations map
calibrpareqmap__ <- sparseMatrix(i = c(2, 3, 4),
                                 j = c(1, 1, 1),
                                 x = rep(1, 3), dims = c(9, 1))

# calibrated parameters / calibrating equations map
calibrparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(1, 1))

# free parameters / equations map
freepareqmap__ <- sparseMatrix(i = c(1, 5, 6, 6, 6, 6, 8, 8, 8, 9),
                               j = c(5, 4, 1, 2, 3, 5, 1, 3, 5, 2),
                               x = rep(1, 10), dims = c(9, 5))

# free parameters / calibrating equations map
freeparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(1, 5))

# shocks / equations map
shockeqmap__ <- sparseMatrix(i = c(5, 5, 9),
                             j = c(1, 2, 2),
                             x = rep(1, 3), dims = c(9, 2))

# steady state equations
ss_eq__ <- function(v, pc, pf)
{
    r <- numeric(9)
    r[1] = v[1] - v[6]^(1 - pf[5])
    r[2] = -v[2] + pc[1] * v[9] * 1^(1 - pc[1]) * v[5]^(-1 + pc[1])
    r[3] = -v[7] + v[9] * (1 - pc[1]) * 1^(-pc[1]) * v[5]^pc[1]
    r[4] = -v[8] + v[9] * 1^(1 - pc[1]) * v[5]^pc[1]
    r[5] = -v[9] + exp(pf[4] * log(v[9]))
    r[6] = pf[1] * (v[2] * v[3]^(-pf[3]) + (1 - pf[2]) * v[3]^(-pf[3])) * v[1]^(-1 + (1 - pf[5])^-1) * v[6]^(-pf[5]) - v[3]^(-pf[3])
    r[7] = -v[3] - v[4] + v[8]
    r[8] = v[4] - v[5] + v[5] * (1 - pf[2])
    r[9] = v[6] - pf[1] * v[1]^((1 - pf[5])^-1) - (-1 + v[3]^(1 - pf[3])) * (1 - pf[3])^-1

    return(r)
}

# calibrating equations
calibr_eq__ <- function(v, pc, pf)
{
    r <- numeric(1)
    r[1] = -0.36 * v[8] + v[2] * v[5]

    return(r)
}

# steady state and calibrating equations Jacobian
ss_calibr_eq_jacob__ <- function(v, pc, pf)
{
    jac <- numeric(30)
    jac[1] = 1
    jac[2] = -(1 - pf[5]) * v[6]^(-pf[5])
    jac[3] = -1
    jac[4] = pc[1] * v[9] * (-1 + pc[1]) * 1^(1 - pc[1]) * v[5]^(-2 + pc[1])
    jac[5] = pc[1] * 1^(1 - pc[1]) * v[5]^(-1 + pc[1])
    jac[6] = v[9] * 1^(1 - pc[1]) * v[5]^(-1 + pc[1]) + pc[1] * v[9] * log(v[5]) * 1^(1 - pc[1]) * v[5]^(-1 + pc[1])
    jac[7] = pc[1] * v[9] * (1 - pc[1]) * 1^(-pc[1]) * v[5]^(-1 + pc[1])
    jac[8] = -1
    jac[9] = (1 - pc[1]) * 1^(-pc[1]) * v[5]^pc[1]
    jac[10] = -v[9] * 1^(-pc[1]) * v[5]^pc[1] + v[9] * log(v[5]) * (1 - pc[1]) * 1^(-pc[1]) * v[5]^pc[1]
    jac[11] = pc[1] * v[9] * 1^(1 - pc[1]) * v[5]^(-1 + pc[1])
    jac[12] = -1
    jac[13] = 1^(1 - pc[1]) * v[5]^pc[1]
    jac[14] = v[9] * log(v[5]) * 1^(1 - pc[1]) * v[5]^pc[1]
    jac[15] = -1 + pf[4] * v[9]^-1 * exp(pf[4] * log(v[9]))
    jac[16] = pf[1] * (-1 + (1 - pf[5])^-1) * (v[2] * v[3]^(-pf[3]) + (1 - pf[2]) * v[3]^(-pf[3])) * v[1]^(-2 + (1 - pf[5])^-1) * v[6]^(-pf[5])
    jac[17] = pf[1] * v[1]^(-1 + (1 - pf[5])^-1) * v[3]^(-pf[3]) * v[6]^(-pf[5])
    jac[18] = pf[3] * v[3]^(-1 - pf[3]) + pf[1] * (-pf[3] * v[2] * v[3]^(-1 - pf[3]) - pf[3] * (1 - pf[2]) * v[3]^(-1 - pf[3])) * v[1]^(-1 + (1 - pf[5])^-1) * v[6]^(-pf[5])
    jac[19] = -pf[1] * pf[5] * (v[2] * v[3]^(-pf[3]) + (1 - pf[2]) * v[3]^(-pf[3])) * v[1]^(-1 + (1 - pf[5])^-1) * v[6]^(-1 - pf[5])
    jac[20] = -1
    jac[21] = -1
    jac[22] = 1
    jac[23] = 1
    jac[24] = -pf[2]
    jac[25] = -pf[1] * (1 - pf[5])^-1 * v[1]^(-1 + (1 - pf[5])^-1)
    jac[26] = -v[3]^(-pf[3])
    jac[27] = 1
    jac[28] = v[5]
    jac[29] = v[2]
    jac[30] = -0.36
    jacob <- sparseMatrix(i = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
                                4, 4, 4, 4, 5, 6, 6, 6, 6, 7,
                                7, 7, 8, 8, 9, 9, 9, 10, 10, 10),
                          j = c(1, 6, 2, 5, 9, 10, 5, 7, 9, 10,
                                5, 8, 9, 10, 9, 1, 2, 3, 6, 3,
                                4, 8, 4, 5, 1, 3, 6, 2, 5, 8),
                          x = jac, dims = c(10, 10))

    return(jacob)
}

# 1st order perturbation
pert1__ <- function(v, pc, pf)
{
    Atm1x <- numeric(5)
    Atm1x[1] = pc[1] * v[9] * (-1 + pc[1]) * 1^(1 - pc[1]) * v[5]^(-2 + pc[1])
    Atm1x[2] = pc[1] * v[9] * (1 - pc[1]) * 1^(-pc[1]) * v[5]^(-1 + pc[1])
    Atm1x[3] = pc[1] * v[9] * 1^(1 - pc[1]) * v[5]^(-1 + pc[1])
    Atm1x[4] = pf[4] * v[9]^-1 * exp(pf[4] * log(v[9]))
    Atm1x[5] = 1 - pf[2]
    Atm1 <- sparseMatrix(i = c(2, 3, 4, 5, 9),
                         j = c(5, 5, 5, 9, 5),
                         x = Atm1x, dims = c(9, 9))

    Atx <- numeric(18)
    Atx[1] = 1
    Atx[2] = -1
    Atx[3] = pc[1] * 1^(1 - pc[1]) * v[5]^(-1 + pc[1])
    Atx[4] = -1
    Atx[5] = (1 - pc[1]) * 1^(-pc[1]) * v[5]^pc[1]
    Atx[6] = -1
    Atx[7] = 1^(1 - pc[1]) * v[5]^pc[1]
    Atx[8] = -1
    Atx[9] = pf[1] * (-1 + (1 - pf[5])^-1) * (v[2] * v[3]^(-pf[3]) + (1 - pf[2]) * v[3]^(-pf[3])) * v[1]^(-2 + (1 - pf[5])^-1) * v[6]^(-pf[5])
    Atx[10] = pf[3] * v[3]^(-1 - pf[3])
    Atx[11] = -1
    Atx[12] = -1
    Atx[13] = 1
    Atx[14] = -pf[1] * (1 - pf[5])^-1 * v[1]^(-1 + (1 - pf[5])^-1)
    Atx[15] = -v[3]^(-pf[3])
    Atx[16] = 1
    Atx[17] = 1
    Atx[18] = -1
    At <- sparseMatrix(i = c(1, 2, 2, 3, 3, 4, 4, 5, 6, 6,
                             7, 7, 7, 8, 8, 8, 9, 9),
                       j = c(1, 2, 9, 7, 9, 8, 9, 9, 1, 3,
                             3, 4, 8, 1, 3, 6, 4, 5),
                       x = Atx, dims = c(9, 9))

    Atp1x <- numeric(4)
    Atp1x[1] = (-1 + pf[5]) * v[6]^(-pf[5])
    Atp1x[2] = pf[1] * v[1]^(-1 + (1 - pf[5])^-1) * v[3]^(-pf[3]) * v[6]^(-pf[5])
    Atp1x[3] = pf[1] * (-pf[3] * v[2] * v[3]^(-1 - pf[3]) - pf[3] * (1 - pf[2]) * v[3]^(-1 - pf[3])) * v[1]^(-1 + (1 - pf[5])^-1) * v[6]^(-pf[5])
    Atp1x[4] = -pf[1] * pf[5] * (v[2] * v[3]^(-pf[3]) + (1 - pf[2]) * v[3]^(-pf[3])) * v[1]^(-1 + (1 - pf[5])^-1) * v[6]^(-1 - pf[5])
    Atp1 <- sparseMatrix(i = c(1, 6, 6, 6),
                         j = c(6, 2, 3, 6),
                         x = Atp1x, dims = c(9, 9))

    Aepsx <- numeric(3)
    Aepsx[1] = exp(pf[4] * log(v[9]))
    Aepsx[2] = -exp(pf[4] * log(v[9]))
    Aepsx[3] = -1
    Aeps <- sparseMatrix(i = c(5, 5, 9),
                         j = c(1, 2, 2),
                         x = Aepsx, dims = c(9, 2))

    return(list(Atm1, At, Atp1, Aeps))
}

# create model object
gecon_model(model_info = info__,
            index_sets = index_sets__,
            variables = variables__,
            variables_tex = variables_tex__,
            shocks = shocks__,
            shocks_tex = shocks_tex__,
            parameters = parameters__,
            parameters_tex = parameters_tex__,
            parameters_free = parameters_free__,
            parameters_free_val = parameters_free_val__,
            equations = equations__,
            calibr_equations = calibr_equations__,
            var_eq_map = vareqmap__,
            shock_eq_map = shockeqmap__,
            var_ceq_map = varcalibreqmap__,
            cpar_eq_map = calibrpareqmap__,
            cpar_ceq_map = calibrparcalibreqmap__,
            fpar_eq_map = freepareqmap__,
            fpar_ceq_map = freeparcalibreqmap__,
            ss_function = ss_eq__,
            calibr_function = calibr_eq__,
            ss_calibr_jac_function = ss_calibr_eq_jacob__,
            pert = pert1__)

