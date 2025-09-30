# Macroeconomic Shocks with Local Projections and VAR models

This project studies the effects of climate-related shocks on the Uruguayan economy using quarterly data.  
Methods include structural VARs, autoregressive models, and local projection techniques.  

## Requirements
- R (>= 4.2)
- Packages: `readxl`, `dplyr`, `tidyr`, `ggplot2`, `zoo`, `seasonal`, `mFilter`, `vars`, `urca`, `tseries`, `forecast`, `sandwich`

## Data
Place the dataset in the `data/` folder (e.g. `data/data_uruguay.xlsx`).

## Outputs
- Impulse response functions (SVAR and LP)  
- Tests for stationarity and AR models  
- Extensions with asymmetric and regime-dependent shocks
