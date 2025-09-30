#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2025, 2do trimestre 
# Profesor: Javier García-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#


remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

source("PS3_Data.R")

rer <- log(er / pc)
ner <- log(er)

remove(pcom, er, pc)

Yl.f <- 100 * cbind(rer, ner)
Yd.f <- diff(Yl.f) # log-diff transformation

remove(rer, ner)

Yl <- window(Yl.f, start = c(2003, 01), end = c(2019, 12))
Yd <- window(Yd.f, start = c(2003, 01), end = c(2019, 12))

# VAR Estimation (Reduced Form) ####

library(vars)

Y <- Yd

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC

# Estimation
Y <- ts(Y[(pmax - p + 1):nrow(Y), ], end = end(Y), 
        frequency = frequency(Y)) # Starting in Jan-05

VAR <- VAR(Y, p = p, type = "const")

m <- VAR$K # Number of variables in the VAR
T <- VAR$obs # Number of effective sample observations, excluding "p" starting values

# Model Checking
roots(VAR, modulus = TRUE)

h.BG <- 6
serial.test(VAR, lags.bg = h.BG, type = "ES")

# SVAR Estimation (Blanchard-Quah)
SVAR <- BQ(VAR)
SVAR

# SVAR t0 Impact Matrix (Cholesky decomposition)
S.Cholesky <- t(resid(VAR)) %*% resid(VAR) / (T - m * p - 1)
P.Cholesky <- t(chol(S.Cholesky))
S.Cholesky

# SVAR t0 Impact Matrix (implied by B model)
P.SVAR <- SVAR$B
S.SVAR <- P.SVAR %*% t(P.SVAR)
S.SVAR

# SVAR Long-Run Impact Matrix
SVAR$LRIM

# Other SVAR Parameters
pars.R <- Bcoef(VAR) # Reduced Form VAR
pars.S <- solve(SVAR$A, pars.R) # Structural Form VAR
pars.R
pars.S

# SVAR Analysis ####

source("PS3_SVAR_LR.R")
source("PS3_SVAR_Plots.R")
source("PS3_SIRF_Transform.R")

library(abind)

H <- 24 # Horizon
H.ERPT <- 36 # Horizon (ERPT)

# IRF
IRF <- SVAR.sirf(SVAR, H)
plot.sirf(IRF, m, H)

# IRF (cumulative) 
IRF.c <- SVAR.sirf(SVAR, H, cumulative = TRUE)
plot.sirf(IRF.c, m, H)

# FEVD
FEVD <- SVAR.fevd(SVAR, H)
plot.fevd(FEVD, m, H)

# HD
HD <- SVAR.hd(SVAR)
plot.hd(Y, HD, m)

# ERPT
I.er <- SVAR.sirf(SVAR, H.ERPT, cumulative = TRUE)
I.pc <- trans.pw("-", I.er, 2, 1)
I <- abind(I.er, I.pc, along = 1)

ERPT <- 100 * trans.pw("/", I, 3, 2)

ERPT.R <- plot.erpt(ERPT[1, ], H.ERPT)
ERPT.N <- plot.erpt(ERPT[2, ], H.ERPT)

remove(I.er, I.pc, I, ERPT)

# HD (Consumer Prices)
HD.pc <- t(trans.pw("-", HD, 2, 1))

pc.s <- HD.pc[, 1:m]
ps.n <- HD.pc[, m + 1]
pc.d <- ts(Y[(p + 1):nrow(Y), "ner"] - Y[(p + 1):nrow(Y), "rer"], 
           end = end(Y), frequency = frequency(Y))
pc.d <- pc.d - ps.n

plot.hd.i(pc.d, pc.s, m, "PC")

# Bootstrap Inference ####

R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

# COMMENT ON THE IMPORTANTE OF MULTIVARIATE CONFIDENCE INTERVALS 

# Bootstrap Replications
Y.boot <- boot.replicate(VAR, R, type)

# IRF (Bootstrap)
IRF.boot <- SVAR.sirf.lr.boot(SVAR, H, gamma, Y.boot)
plot.sirf.boot(IRF.boot, m, H)

# Cumulative IRF (Bootstrap)
IRF.c.boot <- SVAR.sirf.lr.boot(SVAR, H, gamma, Y.boot, cumulative = TRUE)
plot.sirf.boot(IRF.c.boot, m, H)

# FEVD (Bootstrap)
FEVD.boot <- SVAR.fevd.lr.boot(SVAR, H, gamma, Y.boot)
plot.fevd.boot(FEVD.boot, m, H)

# ERPT (Bootstrap)
I.er <- SVAR.sirf.lr.boot(SVAR, H.ERPT, gamma, Y.boot, cumulative = TRUE)
I.pc <- trans.pw.boot("-", I.er, 2, 1, gamma)
I <- list(lb = abind(I.er$lb, I.pc$lb, along = 1), 
          pe = abind(I.er$pe, I.pc$pe, along = 1), 
          ub = abind(I.er$ub, I.pc$ub, along = 1), 
          boot = abind(I.er$boot, I.pc$boot, along = 2))

ERPT.boot <- trans.pw.boot("/", I, 3, 2, gamma)

ERPT.R.boot <- list(lb = 100 * ERPT.boot$lb[1, ], 
                    pe = 100 * ERPT.boot$pe[1, ], 
                    ub = 100 * ERPT.boot$ub[1, ], 
                    boot = 100 * ERPT.boot$boot[, 1, ])

ERPT.N.boot <- list(lb = 100 * ERPT.boot$lb[2, ], 
                    pe = 100 * ERPT.boot$pe[2, ], 
                    ub = 100 * ERPT.boot$ub[2, ], 
                    boot = 100 * ERPT.boot$boot[, 2, ])

remove(I.er, I.pc, I, ERPT.boot)

plot.erpt.boot(ERPT.R.boot, H.ERPT)
plot.erpt.boot(ERPT.N.boot, H.ERPT)