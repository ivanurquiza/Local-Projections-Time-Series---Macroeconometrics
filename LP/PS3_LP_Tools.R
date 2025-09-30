#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier García-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#

source("PS3_LP_Plots.R")

library(sandwich)

cumsum.h <-  function(x, h) {
  diff(cumsum(c(0, x)), h + 1)
}

get.shock <- function(Y, p, m, idx.s) {
  
  Xc <- embed(Y, p + 1)
  
  W1 <- Xc[, (m + 1):(m * (p + 1))]
  if (idx.s == 1) {
    W0 <- NULL
  } else {
    W0 <- Xc[, -c(idx.s:m)]
  }
  Wt <- cbind(1, W0, W1)
  
  st <- Xc[, idx.s]
  st <- unname(resid(lm(st ~ -1 + Wt))) # The -1 term removes the intercept from the regression equation
  
  list(st = st, Wt = Wt)

}

get.shock.pw <- function(Y, D, p, m, idx.s) {
  
  Dt <- D[(p + 1):length(D)]
  
  Xc <- embed(Y, p + 1)
  
  W1 <- Xc[, (m + 1):(m * (p + 1))]
  if (idx.s == 1) {
    W0 <- NULL
  } else {
    W0 <- Xc[, -c(idx.s:m)]
  }
  Wt <- cbind(1, W0, W1)
  Wt <- cbind(Dt * Wt, (1 - Dt) * Wt)
  
  St <- Xc[, idx.s]
  St <- cbind(Dt * St, (1 - Dt) * St)
  St <- unname(resid(lm(St ~ -1 + Wt)))
  
  list(St = St, Wt = Wt, Dt = Dt)
  
}

lp <- function(Y, p, idx.s, idx.r, H, gamma, cumulative = FALSE) {
  
  m <- ncol(Y)
  T <- nrow(Y)
  
  reg <- get.shock(Y, p, m, idx.s)
  
  rsp <- Y[(p + 1):T, idx.r]
  imp <- reg$st
  ctr <- reg$Wt
  
  irf.pe <- rep(NA, H + 1)
  irf.lb <- rep(NA, length(irf.pe))
  irf.ub <- rep(NA, length(irf.pe))
  
  z <- qnorm(1 - (1 - gamma) / 2)
  
  for (h in 0:H) {
    
    if (isTRUE(cumulative)) {
      yh <- cumsum.h(rsp, h)
    } else {
      yh <- rsp[(h + 1):(T - p)]
    }
    
    Wt <- ctr[1:(T - p - h), ]
    st <- imp[1:(T - p - h)]
    
    yh <- resid(lm(yh ~ -1 + Wt)) # The -1 term removes the intercept from the regression equation
    
    projection <- lm(yh ~ -1 + st)
    
    b.pe <- unname(coef(projection))
    b.se <- unname(sqrt(diag(vcovHAC(projection))))
    
    irf.pe[h + 1] <- b.pe
    irf.lb[h + 1] <- b.pe - z * b.se
    irf.ub[h + 1] <- b.pe + z * b.se
    
  }
  
  list(lb = irf.lb, pe = irf.pe, ub = irf.ub)
  
}

lp.pw <- function(Y, D, p, idx.s, idx.r, H, gamma, cumulative = FALSE) {
  
  m <- ncol(Y)
  T <- nrow(Y)
  
  reg <- get.shock.pw(Y, D, p, m, idx.s)
  
  rsp <- Y[(p + 1):T, idx.r]
  imp <- reg$St
  ctr <- reg$Wt
  
  irf.pe <- array(NA, c(H + 1, ncol(imp)))
  irf.lb <- array(NA, dim(irf.pe))
  irf.ub <- array(NA, dim(irf.pe))
  
  z <- qnorm(1 - (1 - gamma) / 2)
  
  for (h in 0:H) {
    
    if (isTRUE(cumulative)) {
      yh <- cumsum.h(rsp, h)
    } else {
      yh <- rsp[(h + 1):(T - p)]
    }
    
    Wt <- ctr[1:(T - p - h), ]
    St <- imp[1:(T - p - h), ]
    
    yh <- resid(lm(yh ~ -1 + Wt)) # The -1 term removes the intercept from the regression equation
    
    projection <- lm(yh ~ -1 + St)
    
    b.pe <- unname(coef(projection))
    b.se <- unname(sqrt(diag(vcovHAC(projection))))
    
    irf.pe[h + 1, ] <- b.pe
    irf.lb[h + 1, ] <- b.pe - z * b.se
    irf.ub[h + 1, ] <- b.pe + z * b.se
    
  }
  
  dimnames(irf.pe) <- list(0:H, c("D", "1-D"))
  dimnames(irf.lb) <- dimnames(irf.pe)
  dimnames(irf.ub) <- dimnames(irf.pe)
  
  list(lb = irf.lb, pe = irf.pe, ub = irf.ub)
  
}

lp.multiplier <- function(Y, p, idx.s, idx.rl, idx.rr, H, gamma) {
  
  m <- ncol(Y)
  T <- nrow(Y)
  
  reg <- get.shock(Y, p, m, idx.s)
  
  rsp.l <- Y[(p + 1):T, idx.rl]
  rsp.r <- Y[(p + 1):T, idx.rr]
  imp <- reg$st
  ctr <- reg$Wt
  
  irf.pe <- rep(NA, H + 1)
  irf.lb <- rep(NA, length(irf.pe))
  irf.ub <- rep(NA, length(irf.pe))
  
  z <- qnorm(1 - (1 - gamma) / 2)
  
  for (h in 0:H) {
    
    yh.l <- cumsum.h(rsp.l, h)
    yh.r <- cumsum.h(rsp.r, h)
    
    Wt <- ctr[1:(T - p - h), ]
    st <- imp[1:(T - p - h)]
    
    yh.l <- resid(lm(yh.l ~ -1 + Wt)) # The -1 term removes the intercept from the regression equation
    yh.r <- resid(lm(yh.r ~ -1 + Wt))
    
    yh.r <- fitted(lm(yh.r ~ -1 + st))
    projection <- lm(yh.l ~ -1 + yh.r)
    
    b.pe <- unname(coef(projection))
    b.se <- unname(sqrt(diag(vcovHAC(projection))))
    
    irf.pe[h + 1] <- b.pe
    irf.lb[h + 1] <- b.pe - z * b.se
    irf.ub[h + 1] <- b.pe + z * b.se
    
  }
  
  list(lb = irf.lb, pe = irf.pe, ub = irf.ub)
  
}

lp.multiplier.pw <- function(Y, D, p, idx.s, idx.rl, idx.rr, H, gamma) {
  
  m <- ncol(Y)
  T <- nrow(Y)
  
  reg <- get.shock.pw(Y, D, p, m, idx.s)
  
  rsp.l <- Y[(p + 1):T, idx.rl]
  rsp.r <- Y[(p + 1):T, idx.rr]
  imp <- reg$St
  ctr <- reg$Wt
  idx <- reg$Dt
  
  irf.pe <- array(NA, c(H + 1, ncol(imp)))
  irf.lb <- array(NA, dim(irf.pe))
  irf.ub <- array(NA, dim(irf.pe))
  
  z <- qnorm(1 - (1 - gamma) / 2)
  
  for (h in 0:H) {
    
    yh.l <- cumsum.h(rsp.l, h)
    Yh.r <- cumsum.h(rsp.r, h)
    
    Dt <- idx[1:(T - p - h)]
    Wt <- ctr[1:(T - p - h), ]
    St <- imp[1:(T - p - h), ]
    
    yh.l <- resid(lm(yh.l ~ -1 + Wt)) # The -1 term removes the intercept from the regression equation
    Yh.r <- cbind(Dt * Yh.r, (1 - Dt) * Yh.r)
    Yh.r <- resid(lm(Yh.r ~ -1 + Wt))
    
    Yh.r <- cbind(fitted(lm(Yh.r[, 1] ~ -1 + St[, 1])), fitted(lm(Yh.r[, 2] ~ -1 + St[, 2])))
    projection <- lm(yh.l ~ -1 + Yh.r)
    
    b.pe <- unname(coef(projection))
    b.se <- unname(sqrt(diag(vcovHAC(projection))))
    
    irf.pe[h + 1, ] <- b.pe
    irf.lb[h + 1, ] <- b.pe - z * b.se
    irf.ub[h + 1, ] <- b.pe + z * b.se
    
  }
  
  dimnames(irf.pe) <- list(0:H, c("D", "1-D"))
  dimnames(irf.lb) <- dimnames(irf.pe)
  dimnames(irf.ub) <- dimnames(irf.pe)
  
  list(lb = irf.lb, pe = irf.pe, ub = irf.ub)
  
}