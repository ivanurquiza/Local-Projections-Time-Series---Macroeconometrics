#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#

source("PS3_Bootstrap.R")
source("PS3_Miscellaneous.R")
source("PS3_SVAR_Tools.R")

# Point Estimates ####
SVAR.sirf <- function(SVAR, H, cumulative = FALSE) {
  
  Y <- SVAR$var$y
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- SVAR$B
  
  SIRF <- sirf(B$Phi, P, m, p, H, cumulative = cumulative)
  dimnames(SIRF)[[1]] <- toupper(colnames(Y))
  dimnames(SIRF)[[2]] <- paste("S.", 1:m, sep = "")
  dimnames(SIRF)[[3]] <- 0:H
  
  SIRF
  
}

SVAR.fevd <- function(SVAR, H) {
  
  Y <- SVAR$var$y
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- SVAR$B
  
  FEVD <- fevd(B$Phi, P, m, p, H)
  dimnames(FEVD)[[1]] <- toupper(colnames(Y))
  dimnames(FEVD)[[2]] <- paste("S.", 1:m, sep = "")
  dimnames(FEVD)[[3]] <- 0:H
  
  FEVD
  
}

SVAR.hd <- function(SVAR) {
  
  Y <- SVAR$var$datamat[, 1:m]
  E <- wash(resid(SVAR$var))
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- SVAR$B
  
  HD <- hd(B$Phi, P, Y, E, m, p, T)
  dimnames(HD)[[1]] <- toupper(colnames(Y))
  dimnames(HD)[[2]] <- c(paste("S.", 1:m, sep = ""), "NS")
  dimnames(HD)[[3]] <- 1:T
  
  HD
  
}

# SVAR.erpt <- function(SVAR, H, vx, vy, cumulative = TRUE) {
#   
#   m <- SVAR$var$K
#   p <- SVAR$var$p
#   
#   B <- VAR.coefficients(SVAR$var)
#   P <- SVAR$B
#   
#   erpt(B$Phi, P, m, p, H, vx, vy, cumulative = cumulative)
#   
# }

# Bootstrap ####
SVAR.sirf.lr.boot <- function(SVAR, H, gamma, Y.boot, cumulative = FALSE) {
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  I.boot <- array(NA, c(m, m, H + 1, R))
  var.names <- colnames(SVAR$var$y)

  for (r in 1:dim(Y.boot)[3]) {
    RVAR.boot <- boot.estimate(var.names, Y.boot[, , r], p)
    SVAR.boot <- BQ(RVAR.boot)
    I.boot[, , , r] <- SVAR.sirf(SVAR.boot, H, cumulative)
  }
  
  I.boot <- aperm(I.boot, c(4, 1, 2, 3))
  
  Ic <- SVAR.sirf(SVAR, H, cumulative)
  Il <- array(NA, dim(Ic))
  Iu <- array(NA, dim(Ic))
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        ci <- boot.ci(I.boot[, i, j, h], gamma)
        Il[i, j, h] <- ci[1]
        Iu[i, j, h] <- ci[2]
      }
    }
  }
  dimnames(Il) <- dimnames(Ic)
  dimnames(Iu) <- dimnames(Ic)
  
  list(lb = Il, pe = Ic, ub = Iu, boot = I.boot)
  
}

SVAR.fevd.lr.boot <- function(SVAR, H, gamma, Y.boot) {
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  F.boot <- array(NA, c(m, m, H + 1, R))
  var.names <- colnames(SVAR$var$y)
  
  for (r in 1:dim(Y.boot)[3]) {
    RVAR.boot <- boot.estimate(var.names, Y.boot[, , r], p)
    SVAR.boot <- BQ(RVAR.boot)
    F.boot[, , , r] <- SVAR.fevd(SVAR.boot, H)
  }
  
  F.boot <- aperm(F.boot, c(4, 1, 2, 3))
  
  Fc <- SVAR.fevd(SVAR, H)
  Fl <- array(NA, dim(Fc))
  Fu <- array(NA, dim(Fc))
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        ci <- boot.ci(F.boot[, i, j, h], gamma)
        Fl[i, j, h] <- ci[1]
        Fu[i, j, h] <- ci[2]
      }
    }
  }
  dimnames(Fl) <- dimnames(Fc)
  dimnames(Fu) <- dimnames(Fc)
  
  list(lb = Fl, pe = Fc, ub = Fu, boot = F.boot)
  
}

# SVAR.erpt.lr.boot <- function(SVAR, H, vx, vy, gamma, Y.boot, cumulative = TRUE) {
#   
#   m <- SVAR$var$K
#   p <- SVAR$var$p
#   
#   E.boot <- matrix(NA, H + 1, R)
#   var.names <- colnames(SVAR$var$y)
#   
#   for (r in 1:dim(Y.boot)[3]) {
#     RVAR.boot <- boot.estimate(var.names, Y.boot[, , r], p)
#     SVAR.boot <- BQ(RVAR.boot)
#     E.boot[, r] <- SVAR.erpt(SVAR.boot, H, vx, vy, cumulative)
#   }
#   
#   E.boot <- aperm(E.boot, c(2, 1))
#   
#   Ec <- SVAR.erpt(SVAR, H, vx, vy, cumulative)
#   El <- rep(NA, H + 1)
#   Eu <- rep(NA, H + 1)
#   for (h in 1:(H + 1)) {
#     ci <- boot.ci(E.boot[, h], gamma)
#     El[h] <- ci[1]
#     Eu[h] <- ci[2]
#   }
#   
#   list(lb = El, pe = Ec, ub = Eu, boot = E.boot)
#   
# }