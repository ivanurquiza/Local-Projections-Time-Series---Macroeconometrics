#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2024, 3er trimestre 
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

Y <- Yl

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "trend")
popt
p <- popt$selection[2] # HQIC

# SVEC ####

Y <- ts(Yl[(pmax - p + 1):nrow(Yl), ], end = end(Yl), frequency = frequency(Yl)) # Starting in Jan-05

# VECM Estimation (Reduced Form) ####
VECM <- ca.jo(Y, type = "trace", ecdet = "trend", K = p, spec = "transitory")
summary(VECM)
plot(VECM)
# LR test: trend 
lttest(VECM, r= 1)

# VECM Estimation (Structural Form) ####
SR <- matrix(NA, 2, 2)
SR
LR <- matrix(NA, 2, 2)
LR[, 2] <- 0
LR

SVEC <- SVEC(VECM, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = FALSE)
summary(SVEC)

SVEC.irf <- irf(SVEC, n.ahead = 36, boot = FALSE)
SVEC.irf
plot(SVEC.irf)

irf.pc.N <- SVEC.irf$irf$ner[, 2] - SVEC.irf$irf$ner[, 1]
plot(irf.pc.N, type = "l")