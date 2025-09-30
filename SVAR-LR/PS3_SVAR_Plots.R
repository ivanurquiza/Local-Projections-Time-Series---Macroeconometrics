#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#


plot.sirf.i <- function(x, H, response, shock) {
  plot(0:H, x,
       main = paste("Response of", response, "to", shock, "shock", sep = " "),
       xlab = "Horizon",
       ylab = "",
       ylim = c(min(x), max(x)),
       type = "o", lwd = 2)
  grid(NULL, NULL, lty = 1)
}

plot.fevd.i <- function(x, H, variable) {
  barplot(x, names.arg = as.character(0:H),
          main = paste("FEVD of ", variable, sep = ""),
          xlab = "Horizon", ylab = "%",
          legend.text = paste("Shock:", rownames(x), sep = " "))
}

plot.hd.i <- function(y, X, m, series.i) {
  
  X <- t(X)
  s.date <- as.Date(paste(start(y)[1], start(y)[2], 01, sep = "-"))
  t <- seq(seq.Date(s.date, length = 2, by = paste(p + 1, "months"))[2], by = "month", length.out = ncol(X))
  
  X.n <- X.p <- X
  X.n[X.n < 0] <- 0
  X.p[X.p > 0] <- 0
  
  title <- paste("Historical Decomposition of ", series.i, sep = "")
  range <- c(min(colSums(X.p) - 0.05), max(colSums(X.n)) + 0.05)
  color <- palette()
  color <- color[2:(m + 1)]
  
  bplot <- barplot(X.n,
                   ylim = range,
                   ylab = "%",
                   main = title,
                   col = color,
                   names.arg = as.yearmon(t),
                   cex.main = 0.8,
                   cex.axis = 0.8,
                   cex.names = 0.8)
  
           barplot(X.p,
                   add = TRUE,
                   main = title,
                   ylim = rev(range),
                   ylab = "%",
                   col = color,
                   names.arg = as.yearmon(t),
                   cex.main = 0.8,
                   cex.axis = 0.8,
                   cex.names = 0.8)
  
  lines(x = bplot, y = y, lwd = 3, col = "black")
  
  legend("topleft",
         inset = 0.005,
         legend = c(rownames(X), series.i),
         col = c(color, "black"),
         box.lty = 0,
         lwd = 3,
         cex = 0.8,
         horiz = TRUE)
  
  bplot
  
}

plot.sirf <- function(X, m, H) {
  for (i in 1:dim(X)[1]) {
    par(mfrow = c(dim(X)[1], 1))
    for (j in 1:dim(X)[2]) {
      plot.sirf.i(X[i, j, ], H, dimnames(X)[[1]][i], dimnames(X)[[2]][j])
    }
  }
}

plot.fevd <- function(X, m, H) {
  par(mfrow = c(1, 1))
  for (i in 1:dim(X)[1]) {
    plot.fevd.i(X[i, , ], H, dimnames(X)[[1]][i])
  }
}

plot.hd <- function(Y, X, m) {
  variable.names <- colnames(Y)
  for (i in 1:m) {
    series.i <- toupper(variable.names[i])
    X.n <- ts(c(t(X[i, m + 1, ])), end = end(Y), frequency = frequency(Y))
    X.s <- ts(t(X[i, 1:m, ]), end = end(Y), frequency = frequency(Y))
    Y.d <- ts(Y[(p + 1):nrow(Y), i], end = end(Y), frequency = frequency(Y)) - X.n
    plot.hd.i(Y.d, X.s, m, series.i)
  }
}

plot.erpt <- function(X, H) {
  par(mfrow = c(1, 1))
  plot(0:H, X,
       main = "Exchange-rate pass-through to consumer prices",
       xlab = "Horizon", ylab = "%",
       type = "o", lwd = 2)
  grid(NULL, NULL, lty = 1)
}

plot.sirf.boot <- function(X, m, H) {
  for (i in 1:m) {
    par(mfrow = c(m, 1))
    for (j in 1:m) {
      plot(0:H, X$pe[i, j, ],
           main = paste("Response of", dimnames(X$pe)[[1]][i], "to", dimnames(X$pe)[[2]][j], "shock", sep = " "),
           xlab = "Horizon", ylab = "",
           ylim = c(min(X$lb[i, j, ]), max(X$ub[i, j, ])),
           type = "o", lwd = 2)
      grid(NULL, NULL, lty = 1)
      xx <- c(0:H, H:0)
      yy <- c(c(X$lb[i, j, ]), rev(c(X$ub[i, j, ])))
      polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
    }
  }
}

plot.fevd.boot <- function(X, m, H) {
  for (i in 1:m) {
    par(mfrow = c(m, 1))
    for (j in 1:m) {
      plot(0:H, X$pe[i, j, ],
           main = paste("Contribution of", dimnames(X$pe)[[2]][j], "shock to variance of", dimnames(X$pe)[[1]][i], sep = " "),
           xlab = "Horizon", ylab = "%",
           ylim = c(0, 100),
           type = "o", lwd = 2)
      grid(NULL, NULL, lty = 1)
      xx <- c(0:H, H:0)
      yy <- c(c(X$lb[i, j, ]), rev(c(X$ub[i, j, ])))
      polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
    }
  }
}

plot.erpt.boot <- function(X, H) {
  plot(0:H, X$pe,
       main = "Exchange-rate pass-through to consumer prices",
       xlab = "Horizon", ylab = "%",
       ylim = c(min(X$lb), max(X$ub)),
       type = "o", lwd = 2)
  grid(NULL, NULL, lty = 1)
  xx <- c(0:H, H:0)
  yy <- c(c(X$lb), rev(c(X$ub)))
  polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
}