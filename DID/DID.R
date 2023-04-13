library(tidyverse)
library(lubridate)
library(data.table)
library(did)

setwd("~/Statistical_methods/DID/Data")

#==============================================================================#
# Set-up functions
#==============================================================================#


# This function takes in a file path, a date column name, optional start and end dates, an event date,
# an outcome column name, and an area code column name. It reads in a CSV file and mutates the data
# by adding a new "Time" column, which indicates whether the date is before or after the event date.
# The function then groups the data by date and area code and calculates the sum of the outcome column,
# the log of the outcome column plus one, and the number of observations. The function returns a
# data frame with the following columns: "Date", "areacode", "Outcome", "Time", and "log_Outcome".

# Returns:
# - Date: the date in YYYY-MM-DD format
# - areacode: the area code as a character string
# - Outcome: the sum of the outcome column for the date and area code
# - Time: a binary variable indicating if the date is before or after the event date
# - log_Outcome: the natural logarithm of the sum of the outcome column plus one

add_time_column <- function(file_path, date_col, start_date = NULL, end_date = NULL, event_date, outcome_column = NULL, area_code_col = NULL) {
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
    rename(Outcome = all_of(outcome_column),
           areacode = all_of(area_code_col)) %>%
    mutate(areacode = as.character(areacode)) %>%
    group_by_if(is.numeric %>% Negate) %>%
    summarize_all(sum) %>%
    mutate(log_Outcome = log(Outcome+1)) %>%
    select(Date, areacode, Outcome, log_Outcome) %>%
    as.data.frame()
  
  return(Data)
}

#This function takes in a dataset, a list of impacted area codes, a list of unaffected area codes,
#and an event date. It then mutates the dataset by adding new variables based on the inputs,
#and returns the mutated dataset.

# Returns:
# - areacode: the numeric value of the areacode column
# - exposed: a binary variable indicating if the area code was impacted by the event
# - numbered_daily: the numeric value of the Date column (which is assumed to be in YYYY-MM-DD format)
# - treat: the date of the event (as a numeric value) for impacted areas, 0 for unaffected areas.

impacted_locations <- function(data_set, impacted_areacodes, unimpcted_areacodes, event_date){
  
  data_set <- data_set %>%
    mutate(areacode = as.numeric(areacode),
           exposed = ifelse(areacode %in% impacted_areacodes, 1,
                            ifelse(areacode %in% unimpcted_areacodes, 0, "")),
           numbered_daily = as.numeric(as.Date(Date)),
           treat = ifelse(exposed == 1, as.numeric(as.Date(event_date)), 0)) 
  
  return(data_set)
  
}

# At this point the user needs to alter the code to meet the needs of the user,
# you can add and remove event periods as needed, just be sure to take into account
# these changes when adding/removing closing parentheses.
# There always needs to be two pre event periods in the function.

Create_temporal_periods <- function(data_frame, event_date) { 
  
  Data <- Data %>%
    mutate(Date_elapsed = fifelse(numbered_daily %between% c(18629, as.numeric(as.Date(event_date))-1), 18700, #pre-storm
                                  fifelse(numbered_daily %between% c(as.numeric(as.Date(event_date)), as.numeric(as.Date(event_date))+28), 18868, #4-weeks
                                          fifelse(numbered_daily %between% c(as.numeric(as.Date(event_date)), as.numeric(as.Date(event_date)) + 92), 18897, #3 months
                                                  fifelse(numbered_daily %between% c(as.numeric(as.Date(event_date)), as.numeric(as.Date(event_date)) + 123), 18961, #4 months
                                                          0)))))#very first day of data
}

#==============================================================================#
# Set-up data for analysis
#==============================================================================#

# Set up filters for the time frame of the study and what outcome you are interested in
Data <- add_time_column(file_path = "Example.csv", # a character string representing the file path of the CSV file
                        date_col = "Date", # a character string representing the name of the date column in the CSV file
                        start_date = "2021-01-01", # a character string representing the start date in YYYY-MM-DD format
                        end_date = "2021-12-29", # a character string representing the end date in YYYY-MM-DD format 
                        event_date = "2021-08-29", # a character string representing the event date in YYYY-MM-DD format
                        outcome_column = "outcome1", # a character string representing the name of the outcome column in the CSV file
                        area_code_col = "areacode") # a character string representing the name of the area code column in the CSV file

# Specify the impacted regions in the study and state the date of the event
Data <- impacted_locations(data_set = Data,
                           impacted_areacodes = c(1, 2, 3, 4), # a vector of numeric area codes that were impacted by an event
                           unimpcted_areacodes = c(5), # a vector of numeric area codes that were unaffected by an event
                           event_date = "2021-08-29") # a character string representing the date of the event in YYYY-MM-DD format

Data <- Create_temporal_periods(data_frame = Data,
                                 event_date = "2021-08-29") # # a character string representing the date of the event in YYYY-MM-DD format

#==============================================================================#
# Generate results using the below
#==============================================================================#

MP <- att_gt(yname= "log_Outcome", #outcome results
             tname = "Date_elapsed", #Time period I am interested in
             idname = "areacode", #Unique identifier 
             gname = "treat", #when the event occurred
             data = Data,
             panel = FALSE)

set.seed(24)
atts <- att_gt(yname = "log_Outcome", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "areacode", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "areacode", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

simple <- aggte(MP, type = "simple")

dynamic <- aggte(atts, type = "dynamic", bstrap = TRUE, clustervars = "areacode")
ggdid(dynamic)

