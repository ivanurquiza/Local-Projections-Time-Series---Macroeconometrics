#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#

plot.lp <- function(I) {
  H <- length(I$pe) - 1
  par(mfrow = c(1, 1))
  plot(0:H, I$pe, type = "l", xlab = "Horizon", ylab = "", ylim = c(min(I$lb), max(I$ub)), lwd = 2)
  lines(0:H, I$lb, lty = 3, lwd = 2)
  lines(0:H, I$ub, lty = 3, lwd = 2)
}

plot.lp.pw <- function(I, titles) {
  n <- ncol(I$pe)
  H <- nrow(I$pe) - 1
  c <- palette()
  par(mfrow = c(1, n))
  for (i in 1:n) {
    plot(0:H, I$pe[, i], type = "l", main = titles[i], col = c[1 + i], xlab = "Horizon", ylab = "", ylim = c(min(I$lb), max(I$ub)), lwd = 2)
    lines(0:H, I$lb[, i], lty = 3, col = c[1 + i], lwd = 2)
    lines(0:H, I$ub[, i], lty = 3, col = c[1 + i], lwd = 2)
  }
}