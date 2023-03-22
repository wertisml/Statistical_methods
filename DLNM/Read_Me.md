# Distributed Lag Non-Linear Models

## Set-up
- The files needed are located in the folder Data, make sure to get your working directories set up correctly.

# DLNM
- Analysis using Distributed Lag Non-linear Models (DLNM) and Generalized Non-linear Models (GNM) to estimate the relationship between mental health outcomes and temperature in different regions.

## Packages Required
- data.table
- mixmeta
- dlnm
- dplyr
- scales
- tsModel
- splines
- lubridate

## Steps
1. Import necessary libraries for data manipulation, analysis, and modeling.
2. Set the working directory to "~/DLNM".
3. Read in the "Sheps_Temp_Cities.csv" file using the `fread` function from the `data.table` package, and store it in the `Data` variable.
4. Read in the "NC_Cities.csv" file and store it in the `cities` variable. Arrange the rows of `cities` based on the `order` column.
5. Join the `cities` and `Data` tables by the `order` and `loc` columns, respectively, and create a new `month` column based on the `Date` column. Filter the rows to include only months from June to September, and rename the `Mental_Health` column to `Outcome`. Select only the `Date`, `TAVG`, `RH`, `Outcome`, and `loc` columns.
6. Convert the `Outcome` column to numeric and remove rows with missing values.
7. Initialize several empty lists to store results later in the script.
8. Loop through each row of the `cities` table and perform the following steps for each city:
    a. Filter the `Data` table to include only rows corresponding to the current city.
    b. Create a cross-basis matrix for temperature (`cbtmean`) using the `crossbasis` function from the `dlnm` package. The matrix includes 3 knots for the crossbasis lag space, which are equally spaced in the log scale of lags to allow for more flexible lag effects of shorter days. The function also takes the lag, degree of freedom (df), and knots as input arguments.
    c. Run a generalized linear model (GLM) using the `glm` function. The dependent variable is `Outcome`, and the independent variables are the `cbtmean` cross-basis matrix, a natural cubic spline of relative humidity (`RH`) with 2 degrees of freedom, a natural cubic spline of day of the year (`yday(Date)`) with 7*4 degrees of freedom, and a factor variable for day of the week (`wday(Date, label = TRUE)`). The `family` argument specifies a quasi-Poisson distribution.
    d. Reduce the cross-basis matrix to obtain a reduced prediction matrix (`redpred`) using the `crossreduce` function. The function takes the cross-basis matrix, the fitted model, and the mean temperature as input arguments. The function centers the cross-basis matrix at the mean temperature and applies the model to obtain a reduced prediction of the outcome variable.
    e. Store the parameters (coefficient and vectorized covariance) in a list (`tmeanparlist`) for later use.
    f. Store the `redpred$cen` value in a list (`TAVGlist`) for later use.
    g. Create a plot of the `redpred` matrix, with lines indicating the 2.5th, 50th, and 97.5th percentiles of the temperature data, and a vertical line indicating the center of the reduced prediction matrix.
    h. Create a prediction matrix (`pred`) using the `crosspred` function. The function takes the cross-basis matrix, the fitted model, the mean temperature, and the temperatures at which to predict the outcome variable as input arguments.
    i. Store the 14th, 15th, and 16th rows of the `pred`

# Pooled Effect
- In this script you will calculate the pooled effect from all of your area.

## Packages Required
- mixmeta
- dlnm
- dplyr
- scales

## Steps
1. Loads the necessary packages.
2. Sets the working directory to where the necessary files are located.
3. Reads in the coefficient and vectorized covariance (coef/vcov) from first-stage models and links them with the census data.
4. Runs the models and tests heterogeneity using mixmeta with no meta-predictor.
5. Plots the average exposure-response relationships using crosspred, where the spline transformation originally used in the first-stage models is defined.
6. Defines the centering point and predicts the association using crosspred.
7. Labels the plot and stores the 14th, 15th, and 16th rows of the prediction matrix in a data frame.

# Alternative Aproach for Small Areas

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
1. Load required packages.
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
