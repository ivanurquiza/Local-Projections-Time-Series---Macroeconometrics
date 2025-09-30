#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2024, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#

remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

library(readr)

X <- read_csv("Data_LP.csv")
X <- ts(X, start = c(2004, 01), frequency = 4)
X <- window(X, end = c(2019, 04))

GC <- X[, "RGC"]
GS <- X[, "NGS"] / X[, "P"]
 Y <- X[, "Y"]

library(seasonal)

seas.adj <- seas(GS)
GS <- seas.adj$series$s11

remove(seas.adj)

G <- GC

X <- cbind(G, Y) # VARIABLE ORDERING IS CRUCIAL!
X <- 100 * log(X) # log transformation
X <- diff(X) # log-diff transformation

remove(GC, GS)

plot(X)

# LP Analysis ####

source("PS3_LP_Tools.R")

p <- 2 # lag length

H <- 4 # Horizon
gamma <- 0.95 # Confidence level

idx.s <- 1 # Shock: G
idx.r <- 2 # Response: Y

# IRF
LP <- lp(X, p, idx.s, idx.r, H, gamma)
plot.lp(LP)

# Cumulative IRF
LP.c <- lp(X, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
plot.lp(LP.c)

# Test
m = ncol(X)
testing <- get.shock(Y = X , p = p, m = m, idx.s = idx.s)


# LP Analysis (nonlinear) ####

D <- ifelse(X[, "G"] < 0, 1, 0)
D <- ts(D, end = end(X), frequency = frequency(X))

states <- c("Austerity", "Otherwise")

# IRF (nonlinear)
LP.pw <- lp.pw(X, D, p, idx.s, idx.r, H, gamma)
plot.lp.pw(LP.pw, states)

# Test
m = ncol(X)
testing2 <- get.shock.pw(Y = X, D = D, p = p, m = m , idx.s)

# Cumulative IRF  (nonlinear)
LP.pw.c <- lp.pw(X, D, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
plot.lp.pw(LP.pw.c, states)

# Multiplier Analysis ####

# Potential Output

# La metodologia alternativa del calculo del producto potencial se origina en el documento titulado
# "Macroeconomic Shocks and Their Propagation" de Valerie A. Ramey, en la pagina 118 (48 del PDF).
# A su vez, esto surge del paper de Ramey y Zubairy (2018).
# 
# Link: https://econweb.ucsd.edu/~vramey/research/Shocks_HOM_Ramey_published_corrected.pdf
# Link Paper: https://econweb.ucsd.edu/~vramey/research/Ramey_Zubairy_JPE_published.pdf

t <- 1:length(Y)
Z <- unname(poly(t, degree = 2, raw = TRUE, simple = TRUE))
Y.regression <- lm(log(Y) ~ Z)
Y.p <- exp(fitted(Y.regression))

par(mfrow = c(1, 1))
plot(unclass(Y)[], type = "l", main = "Quadratic Trend", xlab = "Period", ylab = "Output")
lines(Y.p, col = "blue")

G.tilde <- G / Y.p # G proportional to "potential" output
Y.tilde <- Y / Y.p # Y proportional to "potential" output
X.tilde <- cbind(G.tilde, Y.tilde)
X.tilde <- ts(tail(X.tilde, -1), end = end(X), frequency = frequency(X))

plot(X.tilde)

idx.rl <- idx.r # Cum. LHS: Y.tilde
idx.rr <- idx.s # Cum. RHS: G.tilde

# Multiplier
LP.m <- lp.multiplier(X.tilde, p, idx.s, idx.rl, idx.rr, H, gamma)
plot.lp(LP.m)

# Multiplier (nonlinear)
LP.pw.m <- lp.multiplier.pw(X.tilde, D, p, idx.s, idx.rl, idx.rr, H, gamma)
plot.lp.pw(LP.pw.m, states)