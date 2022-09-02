rm(list = ls())
library(tableone)
library(gmodels)
library(dplyr)
library(lubridate)
library(gtsummary)
library(tidyverse)
library(data.table)
library(dplyr)
library(knitr)

setwd("~/ARIMA/All_Calls")

#All_Calls <- fread("C:\\Users\\wertisml\\Documents\\ARIMA\\All_Calls\\areacode_225.csv")
#All_Calls <- fread("C:\\Users\\wertisml\\Documents\\ARIMA\\All_Calls\\areacode_337.csv")
#All_Calls <- fread("C:\\Users\\wertisml\\Documents\\ARIMA\\All_Calls\\areacode_504.csv")
All_Calls <- fread("C:\\Users\\wertisml\\Documents\\ARIMA\\All_Calls\\areacode_985.csv")

#dat <- subset(All_Calls, Date <= "2021-11-27") #to make 60-90-120 day
All_Calls <- All_Calls[-c(364:365),] #set currently for 4 months
#All_Calls <- All_Calls[-c(241:269),] 
myVars <- c("sumCTL","substance","suicide","stress_anxiety","bereavement")#,"GestHtn","Drug","Pre_Eclampsia")
## Vector of categorical variables that need transformation
catVars <- c("racism", "gender", "race", "sexual_preferance") #,"GestHtn", "Drug","Pre_Eclampsia")
strata<-c("Time")


## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, strata=strata, data = All_Calls, factorVars = catVars)
summary(tab2) #detailed information including missing values
tab3Mat <-print(tab2, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd=TRUE)
write.csv(tab3Mat, file = "pre_post_Ida_985.csv")
###############################################################################################


p <- print(tab2, printToggle = FALSE, noSpaces = TRUE)
kable(p, format = "latex")

