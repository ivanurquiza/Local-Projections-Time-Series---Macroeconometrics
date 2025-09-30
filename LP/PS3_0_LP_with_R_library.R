#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometría
# 2023, 3er trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

# Material basado en código de Luis Libonatti (usado en versiones anteriores de 
# la materia)
#------------------------------------------------------------------------------#

remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

source("PS3_Data.R")

Yl.f <- cbind(er, pc) # Raw data
Yl.f <- log(Yl.f) # log transformation
Yd.f <- 100 * diff(Yl.f) # Raw data in log-differences

Yl <- window(Yl.f, start = c(2003, 01), end = c(2019, 12))
Yd <- window(Yd.f, start = c(2003, 01), end = c(2019, 12))

# Local projections ####
#install.packages("lpifrfs")
library(lpirfs)

L <- 12 # Endogenous lags

IC <- "AICc" # Information criteria (lag selection)
pmax <- 12 # Maximum lag length for IC

H <- 12 # Horizon
CL <- 1.96 # 95% confidence level

# LP standard IRFs ####

Yd.df <- data.frame(Yd) # Endogenous data
shock <- data.frame(Yd.df$er) # Exogenous shock, variable to shock with

# Local projection IRFs
linear <- lp_lin_iv(Yd.df, shock = shock, lags_endog_lin = L, trend = 0, confint = CL, hor = H)
# linear <- lp_lin_iv(Yd.df, shock = shock, lags_endog_lin = NaN, lags_criterion = IC, max_lags = pmax, trend = 0, confint = CL, hor = H)
plot(linear)

# Local projection IRFs (with IV)
IV <- data.frame(Yd.df$er) # Set of instrumental variables
linear.IV <- lp_lin_iv(Yd.df, shock = shock, instrum = IV, lags_endog_lin = L, trend = 0, confint = CL, hor = H)
# linear.IV <- lp_lin_iv(Yd.df, shock = shock, instrum = IV, lags_endog_lin = NaN, lags_criterion = IC, max_lags = pmax, trend = 0, confint = CL, hor = H)
plot(linear.IV)

# LP cumulative IRFs ####

# Data
Yl.df <- data.frame(Yl) # Endogenous data (levels)
shock.lvl <- data.frame(Yl.df$er) # Exogenous shock, variable to shock with (levels)

# Local projection IRFs in levels
linear.lvl <- lp_lin_iv(Yl.df, shock = shock.lvl, lags_endog_lin = L, trend = 0, confint = CL, hor = H)
# linear.lvl <- lp_lin_iv(Yl.df, shock = shock.lvl, lags_endog_lin = NaN, lags_criterion = IC, max_lags = pmax, trend = 0, confint = CL, hor = H)
plot(linear.lvl)

# Local projection IRFs in levels (with IV)
IV.lvl <- data.frame(Yl.df$er) # Set of instrumental variables (levels)
linear.IV.lvl <- lp_lin_iv(Yl.df, shock = shock.lvl, instrum = IV.lvl, lags_endog_lin = L, trend = 0, confint = CL, hor = H)
# linear.IV.lvl <- lp_lin_iv(Yl.df, shock = shock.lvl, instrum = IV.lvl, lags_endog_lin = NaN, lags_criterion = IC, max_lags = pmax, trend = 0, confint = CL, hor = H)
plot(linear.IV.lvl)