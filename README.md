## 📊 Local Projections and VAR (Time Series) Approach to Macroeconomic Impact of Droughts in Uruguay

This project replicates and extends the analysis of **Clevy & Evans (2025)** on the macroeconomic effects of droughts in Uruguay.  
We use quarterly data (2005–2023) to study how climate shocks, captured by the **Soil Moisture Deficit Index (SMDI)**, affect output, consumption, investment, exchange rates, and employment.  

## Methods
- **Data preparation**: per capita transformations, seasonal adjustment, log-differences, and HP filtering.  
- **Structural VAR (SVAR)**: identification with exogenous blocks (climate index and trade partners’ GDP).  
- **Local Projections (LP)**: alternative specifications with HAC/Newey-West standard errors.  
- **Extensions**:  
  - Asymmetric shocks (positive vs. negative).  
  - Large vs. small shocks.  
  - Regime-dependent effects (dry vs. wet states).  

## Data
The dataset (`data_uruguay.xlsx`) was provided as part of the coursework. It includes macroeconomic series from Uruguay and partner countries, as well as the **SMDI** index.  

## References
- Clevy, M., & Evans, P. (2025). *The Macroeconomic Impact of Droughts in Uruguay: A General Equilibrium Analysis Using the Soil Moisture Deficit Index*. IMF Working Paper 2025-004. [DOI link](https://doi.org/10.5089/9798400298059.001)
  
Course: *Macroeconometrics*, Universidad de San Andrés (2025).  

