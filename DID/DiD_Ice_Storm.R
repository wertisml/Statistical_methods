library(data.table)
library(dplyr)
library(plyr)
library(did)
library(tidyverse)
library(lubridate)
library(padr)

setwd("~/Texas_Ice/DID")

All_Calls <- read.csv("C:\\Users\\wertisml\\Documents\\Texas_Ice\\ARIMA\\All_Calls_Texas_Ice_Storm.csv")

#==============================================================================#
#
#==============================================================================#

Impacted <- All_Calls %>%
  subset(Date <= "2022-01-10")%>%
  subset(Date >= "2021-01-01")%>%
  group_by(Date)%>%
  arrange(Date)%>%
  mutate(day = seq(1,by = 1,length.out = nrow(.)),
         treatment = 1,
         Location = 1) %>%
  dplyr::select(Date, sumCTL, stress_anxiety, depressed, suicide, day, treatment, Location)

#==============================================================================#
#
#==============================================================================#

Control <- All_Calls %>%
  subset(Date <= "2020-01-10")%>%
  subset(Date >= "2019-01-01")%>%
  group_by(Date)%>%
  arrange(Date)%>%
  mutate(day = seq(1,by = 1,length.out = nrow(.)),
         treatment = 0,
         Location = 0) %>%
  dplyr::select(Date, sumCTL, stress_anxiety, depressed, suicide, day, treatment, Location)

#==============================================================================#
#
#==============================================================================#

Ice_Storm <- rbind(Impacted, Control)
Data <- data.frame(Ice_Storm)
Data$log_All <- log(Data$sumCTL+1)
Data$log_Stress <- log(Data$stress_anxiety+1)
Data$log_depressed <- log(Data$depressed+1)
Data$log_suicide <- log(Data$suicide+1)

Data$treat[Data$treatment == 1] <- 3 #This is the day of initial impact, August 29, 2021
Data$treat[is.na(Data$treat)] <- 0

#Power outage
#Data$Date_elapsed <- fifelse(Data$day %between% c(2, 40), 1, #pre-storm
#                      fifelse(Data$day %between% c(41, 58), 2, #Feb 10-27
#                        fifelse(Data$day %between% c(59, 86), 3, #4-weeks
#                          fifelse(Data$day %between% c(87, 100), 4, #6 weeks
#                            fifelse(Data$day %between% c(101, 148), 5, #3 months
#                              fifelse(Data$day %between% c(149, 238), 6, #6 months
#                               fifelse(Data$day %between% c(239, 328), 7, #9 months
#                                  fifelse(Data$day %between% c(315, 388), 8, #11 months
#                                 0))))))))#very first day of dat

#Ice storm
#Data$Date_elapsed <- fifelse(Data$day %between% c(2, 40), 1, #pre-storm
#                      fifelse(Data$day %between% c(41, 48), 2, #Feb 10-17
#                        fifelse(Data$day %between% c(49, 76), 3, #4-weeks
#                          fifelse(Data$day %between% c(77, 90), 4, #6 weeks
#                            fifelse(Data$day %between% c(91, 138), 5, #3 months
#                              fifelse(Data$day %between% c(139, 228), 6, #6 months
#                                fifelse(Data$day %between% c(229, 318), 7, #9 months
#                                  fifelse(Data$day %between% c(319, 378), 8, #11 months
#                                    0))))))))#very first day of dat

#Ice storm
Data$Date_elapsed <- fifelse(Data$day %between% c(2, 40), 1, #pre-storm
                       fifelse(Data$day %between% c(41, 51), 2, #Feb 10-20
                         fifelse(Data$day %between% c(52, 79), 3, #4-weeks
                           fifelse(Data$day %between% c(80, 93), 4, #6 weeks
                             fifelse(Data$day %between% c(94, 141), 5, #3 months
                              fifelse(Data$day %between% c(142, 231), 6, #6 months
                                fifelse(Data$day %between% c(232, 321), 7, #9 months
                                  fifelse(Data$day %between% c(322, 381), 8, #11 months
                                     0))))))))#very first day of dat

#==============================================================================#
#log_all
#==============================================================================#

atts_All <- att_gt(yname = "log_All", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "Location", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated"
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               #clustervars = "Location", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

All <- aggte(atts_All, type = "dynamic", bstrap = TRUE)
#ggdid(All)

#==============================================================================#
#log_stress
#==============================================================================#

atts_stress <- att_gt(yname = "log_Stress", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "Location", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated"
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               #clustervars = "Location", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

Stress <- aggte(atts_stress, type = "dynamic", bstrap = TRUE)
#ggdid(Stress)

#==============================================================================#
#log_depressed
#==============================================================================#

atts_depressed <- att_gt(yname = "log_depressed", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "Location", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated"
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               #clustervars = "Location", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

Depressed <- aggte(atts_depressed, type = "dynamic", bstrap = TRUE)
#ggdid(Depressed)

#==============================================================================#
#log_suicide
#==============================================================================#

atts_suicide <- att_gt(yname = "log_suicide", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "Location", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated"
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               #clustervars = "Location", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

Suicide <- aggte(atts_suicide, type = "dynamic", bstrap = TRUE)
#ggdid(Suicide)

#==============================================================================#
#Combine
#==============================================================================#

All
Stress
Suicide
Depressed

atts_All
atts_depressed
atts_stress
atts_suicide





