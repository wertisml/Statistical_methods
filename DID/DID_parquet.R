library(tidyverse)
library(lubridate)
library(data.table)
library(did)
library(arrow)

setwd("~/Statistical_methods/DID/Data")

#==============================================================================#
# Set-up functions
#==============================================================================#


# This function takes in a file path, a date column name, optional start and end dates, an event date,
# an outcome column name, and an area code column name. It reads in a parquet file and mutates the data
# by adding a new "Time" column, which indicates whether the date is before or after the event date.
# The function then groups the data by date and area code and calculates the sum of the outcome column,
# the log of the outcome column plus one, and the number of observations. The function returns a
# data frame with the following columns: "Date", "identifier", "Outcome", "Time", and "log_Outcome".

# Returns:
# - Date: the date in YYYY-MM-DD format
# - identifier: the area code as a character string
# - Outcome: the sum of the outcome column for the date and area code
# - Time: a binary variable indicating if the date is before or after the event date
# - log_Outcome: the natural logarithm of the sum of the outcome column plus one

add_time_column <- function(file_path, date_col, start_date = NULL, end_date = NULL, event_date, outcome_column = NULL, location_identifier_col = NULL) {

  Data <- open_dataset(file_path) %>%
    rename(Outcome = all_of(outcome_column),
           identifier = all_of(location_identifier_col),
           Date = date_col) %>%
    collect() %>%
    mutate(Date = as.IDate(Date)) %>%
    filter(Date >= start_date,
           Date <= end_date) %>%
    mutate(Date = as.IDate(format(Date, "%Y-%m-%d")),
           identifier = as.character(identifier)) %>%
    group_by_if(is.numeric %>% Negate) %>%
    summarize_all(sum) %>%
    mutate(log_Outcome = log(Outcome+1)) %>%
    select(Date, identifier, Outcome, log_Outcome) %>%
    as.data.frame()
  
  return(Data)
}

#This function takes in a dataset, a list of impacted area codes, a list of unaffected area codes,
#and an event date. It then mutates the dataset by adding new variables based on the inputs,
#and returns the mutated dataset.

# Returns:
# - identifier: the numeric value of the identifier column
# - exposed: a binary variable indicating if the area code was impacted by the event
# - numbered_daily: the numeric value of the Date column (which is assumed to be in YYYY-MM-DD format)
# - treat: the date of the event (as a numeric value) for impacted areas, 0 for unaffected areas.

impacted_locations <- function(data_set, impacted_identifiers, unimpcted_identifiers, event_date){
  
  data_set <- data_set %>%
    mutate(identifier = as.numeric(identifier),
           exposed = ifelse(identifier %in% impacted_identifiers, 1,
                            ifelse(identifier %in% unimpcted_identifiers, 0, "")),
           numbered_daily = as.numeric(as.Date(Date)),
           treat = ifelse(exposed == 1, as.numeric(as.Date(event_date)), 0)) 
  
  return(data_set)
  
}

# At this point the user needs to alter the code to meet the needs of the user,
# you can add and remove event periods as needed, just be sure to take into account
# these changes when adding/removing closing parentheses.
# There always needs to be two pre event periods in the function.

Create_temporal_periods <- function(data_frame, event_date, start_date, elapsed_time) { 
  
  start <- as.numeric(as.Date(start_date))
  event <- as.numeric(as.Date(event_date))
  
  Data <- Data %>%
    mutate(Date_elapsed = fifelse(numbered_daily %between% c((start+1), event - 1), (event - 1), #pre-storm
                            fifelse(numbered_daily %between% c(event, event + elapsed_time[1]), event, #4-weeks
                              fifelse(numbered_daily %between% c(event, event + elapsed_time[2]), (event + elapsed_time[1]+1), #3 months
                                fifelse(numbered_daily %between% c(event, event + elapsed_time[3]), (event + elapsed_time[2] + 1), #4 months
                                  0)))))#very first day of data
}

#==============================================================================#
# Set-up data for analysis
#==============================================================================#

# Set up filters for the time frame of the study and what outcome you are interested in
Data <- add_time_column(file_path = "Example.parquet", # a character string representing the file path of the CSV file
                        date_col = "Date", # a character string representing the name of the date column in the CSV file
                        start_date = "2021-01-01", # a character string representing the start date in YYYY-MM-DD format
                        end_date = "2021-12-29", # a character string representing the end date in YYYY-MM-DD format 
                        event_date = "2021-08-29", # a character string representing the event date in YYYY-MM-DD format
                        outcome_column = "outcome1", # a character string representing the name of the outcome column in the CSV file
                        location_identifier_col = "areacode") # a character string representing the name of the area code column in the CSV file

# Specify the impacted regions in the study and state the date of the event
Data <- impacted_locations(data_set = Data,
                           impacted_identifiers = c(1, 2, 3, 4), # a vector of numeric area codes that were impacted by an event
                           unimpcted_identifiers = c(5), # a vector of numeric area codes that were unaffected by an event
                           event_date = "2021-08-29") # a character string representing the date of the event in YYYY-MM-DD format

Data <- Create_temporal_periods(data_frame = Data,
                                start_date = "2021-01-01", # a character string representing the start date in YYYY-MM-DD format
                                event_date = "2021-08-29", # a character string representing the date of the event in YYYY-MM-DD format
                                elapsed_time = c(28, 92, 123)) # a vector of numeric days that elapse between observations

#==============================================================================#
# Generate results using the below
#==============================================================================#

MP <- att_gt(yname= "log_Outcome", #outcome results
             tname = "Date_elapsed", #Time period I am interested in
             idname = "identifier", #Unique identifier 
             gname = "treat", #when the event occurred
             data = Data,
             panel = FALSE)

set.seed(24)
atts <- att_gt(yname = "log_Outcome", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "identifier", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "identifier", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

simple <- aggte(MP, type = "simple")

dynamic <- aggte(atts, type = "dynamic", bstrap = TRUE, clustervars = "identifier")
ggdid(dynamic)