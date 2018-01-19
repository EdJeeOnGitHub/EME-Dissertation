# Generated on 2018-01-17 22:32:50 by gEcon ver. 1.0.2 (2016-12-05)
# http://gecon.r-forge.r-project.org/

# Model name: rbc_T1

# info
info__ <- c("rbc_T1", "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/R/DSGE GCH Files/rbc_T1.gcn", "2018-01-17 22:32:50")

# index sets
index_sets__ <- list()

# variables
variables__ <- c("r",
                 "C",
                 "I",
                 "K_d",
                 "K_s",
                 "L_s",
                 "U",
                 "W",
                 "Y",
                 "Z")

variables_tex__ <- c("r",
                     "C",
                     "I",
                     "K^{\\mathrm{d}}",
                     "K^{\\mathrm{s}}",
                     "L^{\\mathrm{s}}",
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
                  "mu",
                  "phi")

parameters_tex__ <- c("\\alpha",
                     "\\beta",
                     "\\delta",
                     "\\eta",
                     "\\mu",
                     "\\phi")

# free parameters
parameters_free__ <- c("beta",
                       "delta",
                       "eta",
                       "mu",
                       "phi")

# free parameters' values
parameters_free_val__ <- c(0.99,
                           0.025,
                           2,
                           0.3,
                           0.95)

# equations
equations__ <- c("K_s[-1] - K_d[] = 0",
                 "-r[] + alpha * Z[] * K_d[]^(-1 + alpha) * L_s[]^(1 - alpha) = 0",
                 "-W[] + Z[] * (1 - alpha) * K_d[]^alpha * L_s[]^(-alpha) = 0",
                 "-Y[] + Z[] * K_d[]^alpha * L_s[]^(1 - alpha) = 0",
                 "-Z[] + exp(epsilon_Z[] - epsilon_T[] + phi * log(Z[-1])) = 0",
                 "beta * (mu * E[][r[1] * C[1]^(-1 + mu) * (1 - L_s[1])^(1 - mu) * (C[1]^mu * (1 - L_s[1])^(1 - mu))^(-eta)] + mu * (1 - delta) * E[][C[1]^(-1 + mu) * (1 - L_s[1])^(1 - mu) * (C[1]^mu * (1 - L_s[1])^(1 - mu))^(-eta)]) - mu * C[]^(-1 + mu) * (1 - L_s[])^(1 - mu) * (C[]^mu * (1 - L_s[])^(1 - mu))^(-eta) = 0",
                 "(-1 + mu) * C[]^mu * (1 - L_s[])^(-mu) * (C[]^mu * (1 - L_s[])^(1 - mu))^(-eta) + mu * W[] * C[]^(-1 + mu) * (1 - L_s[])^(1 - mu) * (C[]^mu * (1 - L_s[])^(1 - mu))^(-eta) = 0",
                 "I[] - K_s[] + K_s[-1] * (1 - delta) = 0",
                 "U[] - beta * E[][U[1]] - (1 - eta)^-1 * (C[]^mu * (1 - L_s[])^(1 - mu))^(1 - eta) = 0",
                 "-C[] - I[] + Y[] + K_s[-1] * r[] - r[] * K_d[] = 0")

# calibrating equations
calibr_equations__ <- c("-0.36 * Y[ss] + r[ss] * K_d[ss] = 0")

# variables / equations map
vareqmap__ <- sparseMatrix(i = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
                                 4, 4, 4, 4, 5, 6, 6, 6, 7, 7,
                                 7, 8, 8, 9, 9, 9, 10, 10, 10, 10,
                                 10, 10),
                           j = c(4, 5, 1, 4, 6, 10, 4, 6, 8, 10,
                                 4, 6, 9, 10, 10, 1, 2, 6, 2, 6,
                                 8, 3, 5, 2, 6, 7, 1, 2, 3, 4,
                                 5, 9),
                           x = c(2, 1, 2, 2, 2, 2, 2, 2, 2, 2,
                                 2, 2, 2, 2, 3, 4, 6, 6, 2, 2,
                                 2, 2, 3, 2, 2, 6, 2, 2, 2, 2,
                                 1, 2),
                           dims = c(10, 10))

# variables / calibrating equations map
varcalibreqmap__ <- sparseMatrix(i = c(1, 1, 1),
                                 j = c(1, 4, 9),
                                 x = rep(1, 3), dims = c(1, 10))

# calibrated parameters / equations map
calibrpareqmap__ <- sparseMatrix(i = c(2, 3, 4),
                                 j = c(1, 1, 1),
                                 x = rep(1, 3), dims = c(10, 1))

# calibrated parameters / calibrating equations map
calibrparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(1, 1))

# free parameters / equations map
freepareqmap__ <- sparseMatrix(i = c(5, 6, 6, 6, 6, 7, 7, 8, 9, 9,
                                     9),
                               j = c(5, 1, 2, 3, 4, 3, 4, 2, 1, 3,
                                     4),
                               x = rep(1, 11), dims = c(10, 5))

# free parameters / calibrating equations map
freeparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(1, 5))

# shocks / equations map
shockeqmap__ <- sparseMatrix(i = c(5, 5),
                             j = c(1, 2),
                             x = rep(1, 2), dims = c(10, 2))

# steady state equations
ss_eq__ <- function(v, pc, pf)
{
    r <- numeric(10)
    r[1] = -v[1] + pc[1] * v[10] * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1])
    r[2] = -v[4] + v[5]
    r[3] = -v[8] + v[10] * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1])
    r[4] = -v[9] + v[10] * v[4]^pc[1] * v[6]^(1 - pc[1])
    r[5] = -v[10] + exp(pf[5] * log(v[10]))
    r[6] = pf[1] * (pf[4] * v[1] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[4] * (1 - pf[2]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])) - pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    r[7] = (-1 + pf[4]) * v[2]^pf[4] * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[4] * v[8] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    r[8] = v[3] - v[5] + v[5] * (1 - pf[2])
    r[9] = v[7] - pf[1] * v[7] - (1 - pf[3])^-1 * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(1 - pf[3])
    r[10] = -v[2] - v[3] + v[9] - v[1] * v[4] + v[1] * v[5]

    return(r)
}

# calibrating equations
calibr_eq__ <- function(v, pc, pf)
{
    r <- numeric(1)
    r[1] = -0.36 * v[9] + v[1] * v[4]

    return(r)
}

# steady state and calibrating equations Jacobian
ss_calibr_eq_jacob__ <- function(v, pc, pf)
{
    jac <- numeric(38)
    jac[1] = -1
    jac[2] = pc[1] * v[10] * (-1 + pc[1]) * v[4]^(-2 + pc[1]) * v[6]^(1 - pc[1])
    jac[3] = pc[1] * v[10] * (1 - pc[1]) * v[4]^(-1 + pc[1]) * v[6]^(-pc[1])
    jac[4] = pc[1] * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1])
    jac[5] = v[10] * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1]) + pc[1] * v[10] * log(v[4]) * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1]) - pc[1] * v[10] * log(v[6]) * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1])
    jac[6] = -1
    jac[7] = 1
    jac[8] = pc[1] * v[10] * (1 - pc[1]) * v[4]^(-1 + pc[1]) * v[6]^(-pc[1])
    jac[9] = -pc[1] * v[10] * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-1 - pc[1])
    jac[10] = -1
    jac[11] = (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1])
    jac[12] = -v[10] * v[4]^pc[1] * v[6]^(-pc[1]) + v[10] * log(v[4]) * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1]) - v[10] * log(v[6]) * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1])
    jac[13] = pc[1] * v[10] * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1])
    jac[14] = v[10] * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1])
    jac[15] = -1
    jac[16] = v[4]^pc[1] * v[6]^(1 - pc[1])
    jac[17] = v[10] * log(v[4]) * v[4]^pc[1] * v[6]^(1 - pc[1]) - v[10] * log(v[6]) * v[4]^pc[1] * v[6]^(1 - pc[1])
    jac[18] = -1 + pf[5] * v[10]^-1 * exp(pf[5] * log(v[10]))
    jac[19] = pf[1] * pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    jac[20] = pf[1] * (-pf[3] * pf[4]^2 * v[1] * (v[2]^(-1 + pf[4]))^2 * ((1 - v[6])^(1 - pf[4]))^2 * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) - pf[3] * pf[4]^2 * (1 - pf[2]) * (v[2]^(-1 + pf[4]))^2 * ((1 - v[6])^(1 - pf[4]))^2 * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) + pf[4] * v[1] * (-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[4] * (-1 + pf[4]) * (1 - pf[2]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])) + pf[3] * pf[4]^2 * (v[2]^(-1 + pf[4]))^2 * ((1 - v[6])^(1 - pf[4]))^2 * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) - pf[4] * (-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    jac[21] = pf[1] * (pf[4] * v[1] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[4] * (-1 + pf[4]) * (1 - pf[2]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * v[1] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(-pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) - pf[3] * pf[4] * (-1 + pf[4]) * (1 - pf[2]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(-pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])) - pf[4] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[3] * pf[4] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(-pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])
    jac[22] = pf[4] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4]^2 * v[8] * (v[2]^(-1 + pf[4]))^2 * ((1 - v[6])^(1 - pf[4]))^2 * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) + pf[4] * v[8] * (-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(-pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])
    jac[23] = -pf[3] * (-1 + pf[4])^2 * (v[2]^pf[4])^2 * ((1 - v[6])^(-pf[4]))^2 * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) + pf[4] * (-1 + pf[4]) * v[2]^pf[4] * (1 - v[6])^(-1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[4] * v[8] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * v[8] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(-pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])
    jac[24] = pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    jac[25] = 1
    jac[26] = -pf[2]
    jac[27] = -pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    jac[28] = -(-1 + pf[4]) * v[2]^pf[4] * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    jac[29] = 1 - pf[1]
    jac[30] = -v[4] + v[5]
    jac[31] = -1
    jac[32] = -1
    jac[33] = -v[1]
    jac[34] = v[1]
    jac[35] = 1
    jac[36] = v[4]
    jac[37] = v[1]
    jac[38] = -0.36
    jacob <- sparseMatrix(i = c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3,
                                3, 3, 4, 4, 4, 4, 4, 5, 6, 6,
                                6, 7, 7, 7, 8, 8, 9, 9, 9, 10,
                                10, 10, 10, 10, 10, 11, 11, 11),
                          j = c(1, 4, 6, 10, 11, 4, 5, 4, 6, 8,
                                10, 11, 4, 6, 9, 10, 11, 10, 1, 2,
                                6, 2, 6, 8, 3, 5, 2, 6, 7, 1,
                                2, 3, 4, 5, 9, 1, 4, 9),
                          x = jac, dims = c(11, 11))

    return(jacob)
}

# 1st order perturbation
pert1__ <- function(v, pc, pf)
{
    Atm1x <- numeric(4)
    Atm1x[1] = 1
    Atm1x[2] = pf[5] * v[10]^-1 * exp(pf[5] * log(v[10]))
    Atm1x[3] = 1 - pf[2]
    Atm1x[4] = v[1]
    Atm1 <- sparseMatrix(i = c(1, 5, 8, 10),
                         j = c(5, 10, 5, 5),
                         x = Atm1x, dims = c(10, 10))

    Atx <- numeric(29)
    Atx[1] = -1
    Atx[2] = -1
    Atx[3] = pc[1] * v[10] * (-1 + pc[1]) * v[4]^(-2 + pc[1]) * v[6]^(1 - pc[1])
    Atx[4] = pc[1] * v[10] * (1 - pc[1]) * v[4]^(-1 + pc[1]) * v[6]^(-pc[1])
    Atx[5] = pc[1] * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1])
    Atx[6] = pc[1] * v[10] * (1 - pc[1]) * v[4]^(-1 + pc[1]) * v[6]^(-pc[1])
    Atx[7] = -pc[1] * v[10] * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-1 - pc[1])
    Atx[8] = -1
    Atx[9] = (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1])
    Atx[10] = pc[1] * v[10] * v[4]^(-1 + pc[1]) * v[6]^(1 - pc[1])
    Atx[11] = v[10] * (1 - pc[1]) * v[4]^pc[1] * v[6]^(-pc[1])
    Atx[12] = -1
    Atx[13] = v[4]^pc[1] * v[6]^(1 - pc[1])
    Atx[14] = -1
    Atx[15] = pf[3] * pf[4]^2 * v[2]^(-2 + 2 * pf[4]) * (1 - v[6])^(2 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) - pf[4] * (-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    Atx[16] = -pf[4] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[3] * pf[4] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(1 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])
    Atx[17] = pf[4] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(1 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) - pf[3] * pf[4]^2 * v[8] * v[2]^(-2 + 2 * pf[4]) * (1 - v[6])^(2 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) + pf[4] * v[8] * (-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    Atx[18] = -pf[3] * (-1 + pf[4])^2 * v[2]^(2 * pf[4]) * (1 - v[6])^(-2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3]) + pf[4] * (-1 + pf[4]) * v[2]^pf[4] * (1 - v[6])^(-1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) + pf[4] * v[8] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * v[8] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(1 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])
    Atx[19] = pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    Atx[20] = 1
    Atx[21] = -1
    Atx[22] = -pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    Atx[23] = (1 - pf[4]) * v[2]^pf[4] * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    Atx[24] = 1
    Atx[25] = -v[4] + v[5]
    Atx[26] = -1
    Atx[27] = -1
    Atx[28] = -v[1]
    Atx[29] = 1
    At <- sparseMatrix(i = c(1, 2, 2, 2, 2, 3, 3, 3, 3, 4,
                             4, 4, 4, 5, 6, 6, 7, 7, 7, 8,
                             8, 9, 9, 9, 10, 10, 10, 10, 10),
                       j = c(4, 1, 4, 6, 10, 4, 6, 8, 10, 4,
                             6, 9, 10, 10, 2, 6, 2, 6, 8, 3,
                             5, 2, 6, 7, 1, 2, 3, 4, 9),
                       x = Atx, dims = c(10, 10))

    Atp1x <- numeric(4)
    Atp1x[1] = pf[1] * pf[4] * v[2]^(-1 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3])
    Atp1x[2] = pf[1] * (pf[4] * (v[1] * (-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * v[1] * v[2]^(-2 + 2 * pf[4]) * (1 - v[6])^(2 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])) + pf[4] * (1 - pf[2]) * ((-1 + pf[4]) * v[2]^(-2 + pf[4]) * (1 - v[6])^(1 - pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * pf[4] * v[2]^(-2 + 2 * pf[4]) * (1 - v[6])^(2 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])))
    Atp1x[3] = pf[1] * (pf[4] * (v[1] * (-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * v[1] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(1 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])) + pf[4] * (1 - pf[2]) * ((-1 + pf[4]) * v[2]^(-1 + pf[4]) * (1 - v[6])^(-pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-pf[3]) - pf[3] * (-1 + pf[4]) * v[2]^(-1 + 2 * pf[4]) * (1 - v[6])^(1 - 2 * pf[4]) * (v[2]^pf[4] * (1 - v[6])^(1 - pf[4]))^(-1 - pf[3])))
    Atp1x[4] = -pf[1]
    Atp1 <- sparseMatrix(i = c(6, 6, 6, 9),
                         j = c(1, 2, 6, 7),
                         x = Atp1x, dims = c(10, 10))

    Aepsx <- numeric(2)
    Aepsx[1] = exp(pf[5] * log(v[10]))
    Aepsx[2] = -exp(pf[5] * log(v[10]))
    Aeps <- sparseMatrix(i = c(5, 5),
                         j = c(1, 2),
                         x = Aepsx, dims = c(10, 2))

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

