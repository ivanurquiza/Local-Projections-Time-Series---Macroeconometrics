#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier García-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#


# operator: arithmetic operators        valid options are: "+", "-", "*", "/", ...
#        I: m x m x (H + 1) array       3D statistic, point estimate
#   I.boot: m x m x (H + 1) x R array   4D statistic, bootstrap distribution
#    idx.i: integer                     response variable position, first
#    idx.j: integer                     response variable position, second
#    gamma: float                       confidence level

# Transform (Point Estimate)
trans.pw <- function(operator, I, idx.i, idx.j) {
  do.call(operator, list(I[idx.i, , ], I[idx.j, , ]))
}

# Transform (Bootstrap)
trans.pw.boot <- function(operator, I, idx.i, idx.j, gamma) {
  
  T.boot <- do.call(operator, list(I$boot[, idx.i, , ], I$boot[, idx.j, , ]))
  
  Tc <- trans.pw(operator, I$pe, idx.i, idx.j)
  Tl <- array(NA, dim(Tc))
  Tu <- array(NA, dim(Tc))
  for (h in 1:dim(Tc)[2]) {
    for (j in 1:dim(Tc)[1]) {
      ci <- boot.ci(T.boot[, j, h], gamma)
      Tl[j, h] <- ci[1]
      Tu[j, h] <- ci[2]
    }
  }
  dimnames(Tl) <- dimnames(Tc)
  dimnames(Tu) <- dimnames(Tc)
  
  list(lb = Tl, pe = Tc, ub = Tu, boot = T.boot)
  
}