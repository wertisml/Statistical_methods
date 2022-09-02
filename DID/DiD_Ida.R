library(data.table)
library(dplyr)
library(plyr)
library(did)
library(tidyverse)
library(lubridate)
library(padr)

setwd("~/ARIMA/DiD_Analysis")
Ida <- fread("C:\\Users\\wertisml\\Documents\\ARIMA\\All_Calls\\Ida_For_DiD.csv")

Immediate <- Ida %>%
  subset(Date <= "2021-12-29")%>%
  subset(Date >= "2021-01-01")%>%
  group_by(Date)%>%
  arrange(Date)%>%
  select(Date, areacode, sumCTL, stress_anxiety, substance, suicide, bereavement)

Immediate$npa <- as.character(Immediate$areacode)

Data <- Immediate%>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)

Data <- data.frame(Data)

Data$log_All <- log(Data$sumCTL+1) 
Data$log_Stress <- log(Data$stress_anxiety+1) 
Data$log_Substance <- log(Data$substance+1) 
Data$log_suicide <- log(Data$suicide+1) 
Data$log_bereavement <- log(Data$bereavement+1) 

Data$npa <- as.numeric(Data$npa)

Data$exposed[Data$npa == 318] <- 0 
Data$exposed[is.na(Data$exposed)] <- 1

Data$exposed <- fifelse(Data$exposed >=1, 1, 0)

Data$numbered_daily <- as.numeric(as.Date(Data$Date))

Data$treat[Data$exposed == 1] <- 18868 #This is the day of initial impact, August 29, 2021
Data$treat[is.na(Data$treat)] <- 0

Data$Date_elapsed <- fifelse(Data$numbered_daily %between% c(18629, 18867), 18700, #pre-storm
                       fifelse(Data$numbered_daily %between% c(18868, 18896), 18868, #4-weeks
                         fifelse(Data$numbered_daily %between% c(18897, 18960), 18897, #3 months
                           fifelse(Data$numbered_daily %between% c(18961, 18990), 18961, #4 months
                             0))))#very first day of data


MP <- att_gt(yname= "log_bereavement", #outcome results
             tname = "Date_elapsed", #Time period I am interested in
             idname = "npa", #Unique identifier 
             gname = "treat", #when the event occurred
             data = Data,
             panel = FALSE)


atts <- att_gt(yname = "log_bereavement", # LHS variable
               tname = "Date_elapsed", # time variable
               idname = "npa", # id variable
               gname = "treat", # first treatment period variable
               data = Data, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "npa", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

#ggdid(MP)

simple <- aggte(MP, type = "simple")
#simple
dynamic <- aggte(atts, type = "dynamic", bstrap = TRUE, clustervars = "npa")
ggdid(dynamic)


#ggdid(dynamic)
simple <- tidy(simple)
write.table(simple, "simple_DID.csv", append = TRUE, sep = ",", row.names = FALSE)
dynamic <- tidy(dynamic)
write.table(dynamic, "dynamic_DID.csv", append = TRUE, sep = ",", row.names = FALSE)