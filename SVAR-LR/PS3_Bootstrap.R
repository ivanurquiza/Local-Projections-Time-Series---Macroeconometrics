#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#

boot.ci <- function(x, gamma) {
  lb <- (1 - gamma) / 2
  ub <- 1 - (1 - gamma) / 2
  ci <- quantile(x,  probs = c(lb, ub))
  unname(ci)
}

boot.draw <- function(Y, i, m, p) {
  matrix(Y[(i + 1):(i + p), ], m, p, byrow = TRUE)
}

boot.Y0 <- function(Y, p, m, T, stable, R) {
  if (isTRUE(stable)) {
    idx.y <- sample(0:T, R, replace = TRUE)
  } else {
    idx.y <- rep(0, R)
  }
  vapply(1:R, function(i) boot.draw(Y, idx.y[i], m, p), matrix(0, m, p))
}

boot.residuals.parametric <- function(E, m, k, T, R) {
  S <- tcrossprod(E) / (T - k)
  P <- t(chol(S))
  E <- array(rnorm(m * T * R), c(m, T, R))
  vapply(1:R, function(i) P %*% E[, , i], matrix(0, m, T))
}

boot.residuals.nonparametric <- function(E, m, k, T, R) {
  E <- sqrt((T / (T - k))) * E
  idx.e <- matrix(sample(1:T, T * R, replace = TRUE), T, R)
  vapply(1:R, function(i) E[, idx.e[, i]], matrix(0, m, T))
}

boot.residuals <- function(E, m, k, T, R, type) {
  switch(type,
         parametric = boot.residuals.parametric(E, m, k, T, R),
         nonparametric = boot.residuals.nonparametric(E, m, k, T, R))
}

boot.simulate <- function(Y0, E, Pi, Phi, m, p, T) {
  Y <- matrix(NA, m, p + T)
  Y[, 1:p] <- Y0
  for (t in (p + 1):(p + T)) {
    x <- Y[, (t - p):(t - 1)]
    if (p != 1) { x <- c(x[, rev(1:p)]) }
    Y[, t] <- Pi + Phi %*% x + E[, t - p]
  }
  t(Y)
}

boot.replicate <- function(VAR, R, type) {
  Y <- VAR$y
  m <- VAR$K
  k <- dim(VAR$datamat)[2] - m
  T <- VAR$obs
  B <- VAR.coefficients(VAR)
  E <- wash(t(resid(VAR)))
  stable <- ifelse(max(roots(VAR, modulus = TRUE)) < 1, TRUE, FALSE)
  Y0 <- boot.Y0(Y, p, m, T, stable, R)
  E <- boot.residuals(E, m, k, T, R, type)
  Y <- array(NA, c(p + T, m, R))
  for (r in 1:R) {
    Y[, , r] <- boot.simulate(Y0[, , r], E[, , r], B$Pi, B$Phi, m, p, T)
  }
  dimnames(Y)[[2]] <- colnames(VAR$y)
  Y
}

boot.estimate <- function(var.names, Y, p) {
  colnames(Y) <- var.names
  VAR <- VAR(Y, p = p, type = "const")
  # VAR <- VAR(Y, type = "const", lag.max = lag.max, ic = ic)
  # if (!is.null(resmat)) { VAR <- restrict(VAR, method = "man", resmat = resmat) }
}