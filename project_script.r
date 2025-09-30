# ======================================================
# Macroeconometrics | UdeSA
# Quarterly data for Uruguay – transformations, SVAR, AR,
# and Local Projections for Impulse Response Functions

# Robles Urquiza, Burgermeister, Filarent, Huerta
# ======================================================

# --- Setup ---
remove(list = ls(all.names = TRUE))
options(warn = -1)

# Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(sandwich)   # robust vcov
library(ggplot2)
library(zoo)
library(seasonal) 
library(stringr)
library(mFilter)
library(vars)
library(urca)
library(tseries)
library(forecast)

# --- Load and clean data ---
df <- read_excel("data/data_uruguay.xlsx") %>%
  mutate(fecha = as.yearqtr(paste(year, qtr), format = "%Y %q")) %>%
  arrange(fecha)

# GDP decomposition
df$gdp_resto <- df$gdp_total - df$gdp_agro

# Per capita variables
df$gdp_total_pc <- df$gdp_total / df$pop
df$gdp_agro_pc  <- df$gdp_agro  / df$pop
df$gdp_resto_pc <- df$gdp_resto / df$pop
df$cons_pc      <- df$cons      / df$pop
df$invest_pc    <- df$invest    / df$pop

# --- Convert to time series ---
vars <- c(
  "gdp_total", "gdp_agro", "gdp_resto",
  "cons", "invest", "reer", "smdi",
  "gdp_socios", "pop", "employment",
  "gdp_total_pc","gdp_agro_pc","gdp_resto_pc","cons_pc","invest_pc"
)
series_ts <- list()
for (var in vars) {
  series_ts[[var]] <- ts(df[[var]], start = c(2005, 1), frequency = 4)
}

# --- Seasonal adjustment ---
vars_sa <- c("gdp_total_pc","gdp_agro_pc","gdp_resto_pc","cons_pc","invest_pc","employment")

for (var in vars_sa) {
  serie_ts <- ts(df[[var]], start = c(2005, 1), frequency = 4)
  desest <- seas(serie_ts)        
  ajuste  <- final(desest)        
  ajuste_completo <- c(as.numeric(ajuste), rep(NA, nrow(df) - length(ajuste)))
  df[[paste0(var, "_sa")]] <- ajuste_completo
}

# --- Log transformations (scaled 100*log) ---
vars_log <- c("gdp_total_pc_sa","gdp_agro_pc_sa","gdp_resto_pc_sa",
              "cons_pc_sa","invest_pc_sa","employment_sa",
              "reer","gdp_socios")

for (var in vars_log) {
  df[[paste0(var, "_log")]] <- 100 * log(df[[var]])
}

# --- HP filter (cycle component) ---
hp_cycle <- function(x) { mFilter::hpfilter(x, freq = 1600)$cycle }

cycle_vars <- paste0(vars_log, "_log") 
for (var in cycle_vars) {
  df[[paste0(var, "_c")]] <- hp_cycle(df[[var]])
}

# Final variables
vars_final <- c("gdp_total_pc_sa_log_c","gdp_agro_pc_sa_log_c","gdp_resto_pc_sa_log_c",
                "cons_pc_sa_log_c","invest_pc_sa_log_c","employment_sa_log_c",
                "reer_log_c","gdp_socios_log_c", "smdi")

series_ts_final <- list()
for (var in vars_final) {
  series_ts_final[[var]] <- ts(df[[var]], start = c(2005, 1), frequency = 4)
}

# ======================================================
# Structural VAR estimation
# ======================================================

# Unit root tests
for (var in vars_final) {
  serie_ts <- ts(df[[var]], start = c(2005, 1), frequency = 4)
  nd <- ndiffs(serie_ts)   # ADF test
  cat(var, "→", nd, "difference(s)\n")
}

# Variable ordering (recursive identification)
ord <- c("smdi",
  "gdp_socios_log_c",             
  "gdp_agro_pc_sa_log_c",         
  "gdp_resto_pc_sa_log_c",
  "cons_pc_sa_log_c",
  "invest_pc_sa_log_c",
  "reer_log_c",
  "employment_sa_log_c"
)

# Build Y matrix
to_ts <- function(x) ts(as.numeric(x), start = c(2005, 1), frequency = 4)
Y <- do.call(cbind, lapply(ord, function(v) to_ts(df[[v]])))
colnames(Y) <- ord
Y <- Y[complete.cases(Y), ]   
m <- ncol(Y)

# Lag selection
pmax <- 4
sel <- VARselect(Y, lag.max = pmax, type = "const")
sel$selection
p_HQ  <- sel$selection["HQ(n)"]   
p_AIC <- sel$selection["AIC(n)"] 
p <- as.integer(p_HQ)           

# Reduced VAR
VAR_red <- VAR(Y, p = p, type = "const")

# Block exogeneity restrictions
matC_block <- function(m, p, idx_exog = 1:2){
  Cm <- matrix(1, m, m*p + 1)     
  for (i in idx_exog) {           
    for (l in 1:p) {
      for (j in 1:m) {
        if (j != i) Cm[i, m*(l-1) + j] <- 0   
      }
    }
  }
  Cm
}
Cm <- matC_block(m, p, idx_exog = 1:2)
VAR_blk <- restrict(VAR_red, method = "man", resmat = Cm)

# Structural VAR identification
Amat <- diag(m); Amat[lower.tri(Amat)] <- NA
Bmat <- diag(m); Bmat[!diag(m)] <- 0; diag(Bmat) <- NA

idx_smdi   <- which(colnames(Y) == "smdi")
idx_socios <- which(colnames(Y) == "gdp_socios_log_c")
Amat[idx_socios, idx_smdi] <- 0

SV <- SVAR(VAR_blk, Amat = Amat, Bmat = Bmat, lrtest = FALSE, max.iter = 1000)
print(SV$A, digits = 3)  

# IRFs: drought shock
set.seed(1234)
irf_sequia <- irf(
  SV, impulse = "smdi",
  n.ahead = 50, boot = TRUE, ortho = TRUE, runs = 1000, ci = 0.9
)

# Plot IRFs
resp_names <- colnames(Y)
irf_list <- lapply(resp_names, function(v){
  data.frame(
    h  = 0:(nrow(irf_sequia$irf$smdi) - 1),
    pe = irf_sequia$irf$smdi[, v],
    lb = irf_sequia$Lower$smdi[, v],
    ub = irf_sequia$Upper$smdi[, v],
    var = v,
    stringsAsFactors = FALSE
  )
})
irf_df <- do.call(rbind, irf_list)

label_map <- c(
  smdi                     = "SMDI",
  gdp_socios_log_c         = "PIB socios",
  gdp_agro_pc_sa_log_c     = "PIB agro",
  gdp_resto_pc_sa_log_c    = "PIB resto",
  cons_pc_sa_log_c         = "Consumo",
  invest_pc_sa_log_c       = "Inversión",
  reer_log_c               = "REER",
  employment_sa_log_c      = "Empleo"
)
irf_df$var_pretty <- label_map[irf_df$var]
irf_df$var_pretty <- factor(irf_df$var_pretty, levels = label_map[ord])

ggplot(irf_df, aes(h, pe)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) +
  geom_line(size = 1, color = "blue") +
  facet_wrap(~ var_pretty, scales = "free_y", ncol = 2) +
  labs(x = "Quarters", y = "Response") +
  theme_minimal(base_size = 12)

# ======================================================
# AR Model and Residuals
# ======================================================

acf(df$smdi, main="ACF | smdi")

pmax <- 10
VARselect(df$smdi, lag.max = pmax, type = "const")

modelo_ar <- arima(df$smdi, order = c(2, 0, 0))
summary(modelo_ar)

residuos_ar <- residuals(modelo_ar)
Box.test(residuos_ar, lag = 12, type = "Ljung-Box")
adf.test(df$smdi)

sd_residuos <- sd(residuos_ar)
df$error_normalizado <- residuos_ar / sd_residuos

plot(df$smdi, type = "l", col = "blue", ylab = "", 
     xlab = "Quarter", main = "", lwd = 2)
lines(residuos_ar, col = "red", lwd = 2)
legend("topright", legend = c("Original series", "Residuals"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

# ======================================================
# Local Projections (LP) and Extensions
# ======================================================
# NOTE: Functions and extensions (LP, piecewise shocks, regime-dependent shocks)
# should follow the same cleaned style. See script for implementation.
