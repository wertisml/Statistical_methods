# Distributed Lag Non-Linear Models

## Set-up
- The files needed are located in the folder Data, make sure to get your working directories set up correctly.

## DLNM
- In this script you will calculate the DLNM for each of your specific areas.

## Pooled Effect
- In this script you will calculate the pooled effect from all of your area.

## Small_Area
### Analysis using Distributed Lag Non-linear Models (DLNM) and Generalized Non-linear Models (GNM) to estimate the relationship between mental health outcomes and temperature in different regions.

## Packages Required
- data.table
- dplyr
- dlnm
- mixmeta
- tsModel
- splines
- lubridate
- gnm
- scales

## Steps
1. Load required packages
2. Read data from "Sheps_Temp_Regions.csv" file into a data table using the fread() function from data.table package.
3. Perform data wrangling using dplyr package to add new variables, rename variables and select variables of interest.
4. Define splines of day of the year using the onebasis() function from dlnm package.
5. Define cross-basis for temperature using the crossbasis() function from dlnm package.
6. Define the strata using the paste() function and factor() function from the base package.
7. Run GNM model using the gnm() function from gnm package and the cross-basis for temperature, splines of day of the year, day of the week and the strata.
8. Calculate cross-predicted values using crosspred() function from dlnm package.
9. Plot the results using the plot() function and the cross-predicted values and the observed temperature data.
10. Calculate risk ratios using crosspred() function and the predicted values from GNM model at the 2.5th, 50th and 97.5th percentiles of temperature.
11. Output a data frame with the risk ratios at the 2.5th, 50th, and 97.5th percentiles of temperature.
