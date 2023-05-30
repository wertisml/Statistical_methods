library(tidyverse)  # for data manipulation and visualization
library(forecast)   # for time series forecasting
library(tseries)    # for time series analysis
library(zoo)        # for time series data structures and methods
library(TSA)        # for time series analysis
library(lubridate)  # for working with date and time objects
library(tfarima)    # for advanced ARIMA modeling
library(lmtest)     # for running statistical tests
library(gridExtra)  # for combining multiple plots into single figures

# Set working directory
setwd("~/GRAM/ARIMA/Texas")

#==============================================================================#
# Initilize Functions
#==============================================================================#

# Function to preprocess data
#' Add time column to the data and filter based on start and end date
#'
#' @param file_path The path of the input CSV file
#' @param date_col The name of the date column in the input file
#' @param start_date The start date for filtering the data (optional)
#' @param end_date The end date for filtering the data (optional)
#' @param event_date The date of the intervention
#' @return A data frame with an additional Time column and filtered data based on start and end date

add_time_column <- function(file_path, date_col, start_date = NULL, end_date = NULL, event_date) {
  Data <- read_csv(file_path)
  
  # Set the original date as a character string
  original_date <- Data[[date_col]]
  
  # Convert the original date to a date object
  date_object <- as.IDate(original_date)
  
  Data <- Data %>%
    # Convert date column to desired format
    mutate(date_col = format(date_object, "%Y-%m-%d")) %>%
    # Filter data based on start and end date
    filter(date_col >= start_date) %>%
    filter(date_col <= end_date) %>%
    # Add Time column with 1 for dates after the intervention and 0 for dates before the intervention
    mutate(Time = ifelse(date_col > as.Date(event_date), 1, 0))
  
  return(Data)
}

# Creating the time series
create_ts <- function(Data, myvars, Year, extent) {
  # Subset the input data using the provided myvars parameter
  dat <- subset(Data)
  dat <- dat[myvars]
  # Create a sequence of dates starting from the first date in the input data, 
  # up to the extent parameter, incremented by a day each time
  inds <- seq(as.Date(Data$Date[1]), as.Date(Data$Date[extent]), by = "day")
  # Create a time series object from the subset of the input data using the specified start year, 
  # the day of the year of the first date in the sequence inds, and a frequency of 365 days per year
  dat <- ts(dat, start = c(Year, as.numeric(format(inds[1], "%j"))), frequency = 365)
  # Return the resulting time series object
  return(dat)
}


# Test Ramp, Step, and Pulse for Auto.Arima 
test_Auto.Arima <- function(Data, event_day, forecast, interventions = NULL) {
  
  # Step change (also called a level shift): A sudden, sustained change where 
  #the time series is shifted either up or down by a given value immediately 
  #following the intervention. The step change variable takes the value of 0 prior
  #to the start of the intervention, and 1 afterwards.
  step <- as.numeric(as.yearmon(time(dat))>=as.numeric(as.yearmon(Data$Date[event_day-1])))
  # Ramp: A change in slope that occurs immediately after the intervention. 
  #The ramp variable takes the value of 0 prior to the start of the intervention 
  #and increases by 1 after the date of the intervention.
  ramp <- append(rep(0, (event_day-1)), seq(1,forecast,1))
  #Pulse: A sudden, temporary change that is observed for one or more time points 
  #immediately after the intervention and then returns to baseline level. The pulse 
  #variable takes the value of 1 on the date of the intervention, and 0 otherwise.
  pulse <- InterventionVar(as.yearmon(time(dat)), (event_day - 1))
  
  xreg <- NULL
  if ("step" %in% interventions) {
    xreg <- cbind(xreg, step)
  }
  
  if ("ramp" %in% interventions) {
    xreg <- cbind(xreg, ramp)
  }
  
  if ("pulse" %in% interventions) {
    xreg <- cbind(xreg, pulse)
  }
  
  model <- auto.arima(dat, seasonal = FALSE, xreg = xreg, stepwise = FALSE, trace = TRUE)
  
  return(list(model, xreg))
}

# Test Ramp, Step, and Pulse for Arima 
test_Arima <- function(Data, event_day, forecast, interventions = NULL, p, d, q) {
  
  # Step change (also called a level shift): A sudden, sustained change where 
  #the time series is shifted either up or down by a given value immediately 
  #following the intervention. The step change variable takes the value of 0 prior
  #to the start of the intervention, and 1 afterwards.
  step <- as.numeric(as.yearmon(time(dat))>=as.numeric(as.yearmon(Data$Date[event_day-1])))
  # Ramp: A change in slope that occurs immediately after the intervention. 
  #The ramp variable takes the value of 0 prior to the start of the intervention 
  #and increases by 1 after the date of the intervention.
  ramp <- append(rep(0, (event_day-1)), seq(1,forecast,1))
  #Pulse: A sudden, temporary change that is observed for one or more time points 
  #immediately after the intervention and then returns to baseline level. The pulse 
  #variable takes the value of 1 on the date of the intervention, and 0 otherwise.
  pulse <- InterventionVar(as.yearmon(time(dat)), (event_day - 1))
  
  xreg <- NULL
  if ("step" %in% interventions) {
    xreg <- cbind(xreg, step)
    colnames(xreg)[ncol(xreg)] <- "step"
  }
  
  if ("ramp" %in% interventions) {
    xreg <- cbind(xreg, ramp)
    colnames(xreg)[ncol(xreg)] <- "ramp"
  }
  
  if ("pulse" %in% interventions) {
    xreg <- cbind(xreg, pulse)
    colnames(xreg)[ncol(xreg)] <- "pulse"
  }
  
  model <- Arima(dat, xreg = xreg, order = c(p, d, q))
  
  return(model)
}

my_forecast <- function(model, use_xreg = TRUE) {
  
  # Create regressor variable if use_xreg is true
  if(use_xreg){
    fc <- forecast(model, xreg = xreg, level = c(80, 90, 95)) 
    fc.ts <- ts(as.numeric(fc$mean[event_day:extent]), start=c(ts_year, doy), frequency=365) #The length needs to be the length of the forecasted amount
    dat.ts.2 <- ts.union(dat, fc.ts)
  }
  else{
    fc <- forecast(model, h = forecast, level = c(80, 90, 95))
    fc.ts <- ts(as.numeric(fc$mean), start=c(ts_year, doy), frequency=365)
    dat.ts.2 <- ts.union(dat, fc.ts)
  }
  
  # Create dataframe with forecasts
  data <- data.frame(dat.ts.2) %>%
    drop_na(dat) %>%
    mutate(Date = seq(extent)) 
  
  # Create empty dataframe for upper and lower bounds
  fore <- data.frame(matrix(ncol = 2, nrow = forecast)) #nrow is the days ahead
  
  # Provide column names for upper and lower bounds
  colnames(fore) <- c('upper', 'lower')
  
  # Extract upper bounds and add to dataframe
  upper <- as.matrix(fc[["upper"]])
  upper <- data.frame(upper[1:forecast,])
  fore$upper90 <- upper$X90.
  fore$upper95 <- upper$X95.
  
  # Extract lower bounds and add to dataframe
  lower <- as.matrix(fc[["lower"]])
  lower <- data.frame(lower[1:forecast,])
  fore$lower90 <- lower$X90.
  fore$lower95 <- lower$X95.
  
  # Add date information to the forecast dataframe
  fore$Date <- seq(1,forecast)+event_day #change numbers, 1, value of h, + days used as pre event data 
  
  # Merge the data and forecast dataframes by date
  data<-merge(data,fore, by="Date", all.x=T)
  
  # Return a list containing the data and forecast dataframes
  return(list(data = data, fore = fore))
}

# Creates the plot function
plot_forecast <- function(data, fc.ts, fore, ylab_text) {
  ggplot(data = data, aes(x=as.Date(Date + (as.numeric(Data[1,1])-1)))) + 
    # Create a line plot for the original data with the x-axis being dates and y-axis being "dat"
    geom_line(aes(y=dat), color="darkred", size = 0.5) +
    # Create a line plot for the forecasted data with the x-axis being dates and y-axis being "fc.ts"
    geom_line(aes(y=fc.ts), color="orangered", linetype="twodash", size = 1.) +
    # Create a ribbon plot for the upper and lower bounds of the forecasted data
    geom_ribbon(data=fore, aes(ymin=(lower95), ymax=(upper95)),alpha=0.5, fill="steelblue")+
    # Use a white background theme
    theme_bw() + 
    # Remove legend
    theme(legend.position="none") +
    # Label x-axis as "Date"
    xlab("Date") + 
    # Label y-axis  
    ylab(ylab_text) + # use input ylab_text here
    # Rotate y-axis label by 0 degrees and adjust vertical justification to 1
    theme(axis.title.y=element_text(angle=0, vjust=1))+
    #ggtitle("                                         Crisis text volume for stress and anxiety over time") +
    # Set linetypes for the line plots
    scale_linetype_manual(values=c("twodash", "dotted"))+
    # Set color range for the line plots
    scale_color_grey(start=0.8, end=0.2)
}

#==============================================================================#
# Run the functions to create perform the analysis
#==============================================================================#

# Call the function with file_path = "example.csv", date_col = "date", and event_date = "2022-03-01"
# Regardless of the formatting of you date column you need to specify the event times in YYYY-MM-DD format
Data <- add_time_column(file_path = "All_Calls_Texas_Ice_Storm.csv", 
                        date_col = "Date", 
                        start_date = "2021-01-01",
                        end_date = "2021-12-10",
                        event_date = "2021-02-10")

# Time variables
Year <- year(Data$Date[1]) #This represents the year that the data starts at
time <- which(Data$Time == 1, arr.ind = T) #Creates a list of the dataframe positions of all entries that occurred after the event
event_day = time[1] #the position of the first post event day
doy <- as.numeric(strftime(Data$Date[event_day], format = "%j")) #the day of the year the event takes place
extent = nrow(Data) #The entire length of the dataframe
forecast = length(time) #The forecasted amount of days, this is all days in the dataframe after the event
ts_year <- year(Data$Date[event_day]) #The year that the event occured in
last_day = time[length(time)]
end_day <- as.numeric(strftime(Data$Date[last_day], format = "%j"))
last_year <- year(Data$Date[last_day])

# Creating the time series, specify which outcome you want to examine within the c("")
dat <- create_ts(Data, c("sumCTL"), Year, extent)

# You will want to test multiple aproaches to determine the best result between auto.arima and Arima
# Test Ramp, Step, and Pulse for Auto.Arima, 
# If you don't need an intervention, remove the intervention call
result <- test_Auto.Arima(Data = Data, 
                          event_day, forecast, 
                          interventions = c("step"))
model1 <- result[[1]]
coeftest(model1)
xreg <- result[[2]]

# Test Ramp, Step, and Pulse for Arima 
# If you don't need an intervention, remove the intervention call
model2 <- test_Arima(Data = Data, 
                     event_day, forecast, 
                     interventions = c("step"), 
                     p = 1, d = 1, q = 1)
coeftest(model2)
xreg <- model2$xreg

# you will want to change the model being used based on best AIC performance
# If you do not use an intervention term then use_xreg needs to be FALSE
result <- my_forecast(model = model1, use_xreg = TRUE)
data <- result$data
fore <- result$fore

#==============================================================================#
#Plot the ARIMA model + forecast
#==============================================================================#

Plot <- plot_forecast(data, fc.ts, fore, ylab_text = "b)")
Plot

#==============================================================================#
# Create grid for the ARIMA model + forecast
#==============================================================================#

pdf(file = paste0("ARIMA_Plots", ".pdf"), width = 6, height = 6)
grid.arrange(sumCTL, substance, suicide, stress_anxiety, bereavement, ncol = 2, padding = 20)
dev.off()

