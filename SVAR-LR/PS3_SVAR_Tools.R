#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier García-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#



VAR.coefficients <- function(VAR) {
  B <- wash(Bcoef(VAR))
  list(Pi = B[, m * p + 1], Phi = B[, 1:(m * p)])
}

companion <- function(Phi, m, p) {
  if (p == 1) {
    matrix(Phi, m, m)
  } else {
    rbind(matrix(Phi, m, m * p), cbind(diag(m * (p - 1)), matrix(0, m * (p - 1), m)))
  }
}

var2vma <- function(Phi, P, m, p, H) {
  F <- companion(Phi, m, p)
  F.temp <- F
  I <- array(NA, c(m, m, H + 1))
  I[, , 1] <- P
  for (h in 1:H) {
    I[, , h + 1] <- F.temp[1:m, 1:m] %*% P
    F.temp <- F.temp %*% F
  }
  I
}

sirf <- function(Phi, P, m, p, H, cumulative = FALSE) {
  I <- var2vma(Phi, P, m, p, H)
  if (isFALSE(cumulative)) {
    I
  } else {
    aperm(apply(I, c(1, 2), cumsum), c(2, 3, 1))
  }
}

fevd <- function(Phi, P, m, p, H) {
  I <- var2vma(Phi, P, m, p, H)
  M <- I[, , 1] ^ 2
  J <- tcrossprod(I[, , 1])
  W <- array(NA, c(m, m, H + 1))
  W[, , 1] <- M / diag(J)
  for (h in 2:(H + 1)) {
    M <- M + I[, , h] ^ 2
    J <- J + tcrossprod(I[, , h])
    W[, , h] <- M / diag(J)
  }
  100 * W
}

hd <- function(Phi, P, Y, E, m, p, T) {
  I <- var2vma(Phi, P, m, p, T - 1)
  U <- t(solve(P, t(E)))
  hist.decomp <- array(NA, c(m, m + 1, T))
  for (t in 1:T) {
    for (j in 1:m) {
      for (i in 1:m) {
        hist.decomp[i, j, t] <- c(crossprod(I[i, j, 1:t], U[t:1, j]))
      }
    }
  }
  non.stochastic <- Y - t(apply(hist.decomp[, 1:m, ], c(1, 3), sum))
  for (i in 1:m) {
    hist.decomp[i, m + 1, ] <- non.stochastic[, i]
  }
  hist.decomp
}

# erpt <- function(Phi, P, m, p, H, vx, vy, cumulative = TRUE) {
#   if (isTRUE(cumulative)) {
#     I <- sirf(Phi, P, m, p, H, cumulative = TRUE)
#   } else {
#     I <- sirf(Phi, P, m, p, H)
#   }
#   100 * (I[vx, vy, ] / I[vy, vy, ])
# }