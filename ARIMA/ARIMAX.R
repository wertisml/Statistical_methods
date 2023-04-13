rm(list = ls())
library(forecast)
library(tidyverse)
library(gridExtra)
library(xts)
library(lubridate)
library(astsa)
library(reshape2)
library(padr)
library(zoo)
library(TSA)
library(data.table)
library(urca)
library(fpp2)


setwd("~/GRAM/ARIMA/Texas")

#covariates could be the average cold temp, power outage, or covid cases
Power <- fread("C:\\Users\\owner\\Documents\\CTL_COVID_TX_ICE.csv") #Has both power outage percent and covid cases and deaths
Temperature <- fread("C:\\Users\\owner\\Documents\\GRAM\\ARIMA\\Texas\\Texas_Weather_2021.csv") #temperature data for Texas

#==============================================================================#
#Normalize COVID cases
#==============================================================================#

Cases <- Power %>%
  group_by(countyFIPS) %>%
  arrange(Date2) %>%
  filter(countyFIPS > 1) %>%
  filter(Date2 >= "2020-08-01") %>% #First data point
  filter(Date2 <= "2022-01-10") %>% #Change this date to fit the temporal period
  mutate(Case_diff = Cases - lag(Cases, default = first(Cases)),
         Death_diff = Deaths - lag(Deaths, default = first(Deaths)),
         Time = fifelse(Date2 > "2021-02-10", 1, 0)) #When the event occurred

Cases$Case_diff <- ifelse(Cases$Case_diff < 0, 0, Cases$Case_diff)
Cases$Death_diff <- ifelse(Cases$Death_diff < 0, 0, Cases$Death_diff)

#==============================================================================#
#Join CTL + Covid with Weather
#==============================================================================#

Texas <- left_join(Cases, Temperature, by= c("countyFIPS" = "region_code", "Date2" = "date"))

#==============================================================================#
#Set up
#==============================================================================#

Data <- Texas %>%
  group_by(Date2) %>%
  summarize(sumCTL = sum(sumCTL),
            suicide = sum(suicide),
            stress = sum(stress_anxiety),
            depression = sum(depressed),
            Covid_Cases = round(mean(Case_diff),2),
            Covid_Deaths = round(mean(Death_diff),2),
            power_out = round(mean(Outage_percent),2),
            Tavg = round(mean(TAVG),2),
            Time = mean(Time))

# Time variables
Year <- year(Data$Date2[1]) #This represents the year that the data starts at
time <- which(Data$Time == 1, arr.ind = T) #Creates a list of the dataframe positions of all entries that occurred after the event
event_day = time[1] #the position of the first post event day
doy <- as.numeric(strftime(Data$Date2[event_day], format = "%j")) #the day of the year the event takes place
extent = nrow(Data) #The entire length of the dataframe
forecast = length(time) #The forecasted amount of days, this is all days in the dataframe after the event
ts_year <- year(Data$Date2[event_day]) #This represents the year that the event starts at

#==============================================================================#
# Creating time series
#==============================================================================#

dat <- subset(Data) 
myvars <- c("sumCTL") # The variable that you are looking at
dat <- dat[myvars]

## Create a daily Date object - from the start to the end of the data set
inds <- seq(as.Date(Data$Date2[1]), as.Date(Data$Date2[extent]), by = "day")
# Uses the date object to create a time series based off the dataframe
dat <- ts(dat,
           start = c(Year, as.numeric(format(inds[1], "%j"))),
           frequency = 366)

#==============================================================================#
#Create covariates
#==============================================================================#

Covid_Cases <- Data$Covid_Cases
Covid_Deaths <- Data$Covid_Deaths
Power_Outage <- Data$power_out
TAVG <- Data$Tavg

#==============================================================================#
#Check for stationarity and look at ACF & PACF
#If test-statistic less then the critical value? if not you fail to reject the null hypothesis
#==============================================================================#

Covid_Cases %>%
  ur.kpss() %>%
  summary()

Covid_Deaths %>%
  ur.kpss() %>%
  summary()

Power_Outage %>%
  ur.kpss() %>%
  summary()

TAVG %>%
  ur.kpss() %>%
  summary()

dat %>%
  ur.kpss() %>%
  summary()

# Check ACF and PACF of your variable

acf2(Data$sumCTL)

#==============================================================================#
#Run ARIMAX model
#==============================================================================#

model2 <- auto.arima(dat, seasonal=F, xreg=cbind(Covid_Cases, Covid_Deaths, Power_Outage, TAVG), stepwise=F, trace=T)

#checkresiduals(model2)

#==============================================================================#
#Building for plot
#==============================================================================#

fc <- forecast(model2, xreg = cbind(Covid_Cases, Covid_Deaths, Power_Outage, TAVG)) 

fc.ts <- ts(as.numeric(fc$mean[event_day:extent]), start=c(ts_year, doy), frequency=366) #The length needs to be the length of the forecasted amount
dat.ts.2 <- ts.union(dat, fc.ts)

data <- data.frame(dat.ts.2) %>%
  drop_na(dat) %>%
  mutate(Date = seq(extent)) ####This needs to be changed as the dates being looked at change, this is the total extent of dataframe being used

fore <- data.frame(matrix(ncol = 2, nrow = forecast)) #nrow is the days ahead

# Provide column names
colnames(fore) <- c('upper', 'lower')
upper <- as.matrix(fc[["upper"]])
upper <- data.frame(upper[1:forecast,])
fore$upper80 <- upper$X80.
fore$upper95 <- upper$X95.
lower <- as.matrix(fc[["lower"]])
lower <- data.frame(lower[1:forecast,])
fore$lower80 <- lower$X80.
fore$lower95 <- lower$X95.

fore$Date <- seq(1,forecast)+event_day #change numbers, 1, value of h, + days used as pre event data 

data<-merge(data,fore, by="Date", all.x=T)

#==============================================================================#
#Plot the ARIMA model + forecast
#==============================================================================#

sumCTL <- ggplot(data = data, aes(x=(as.Date(Date + (as.numeric(Data[1,1])-1))))) + 
  geom_line(aes(y=dat), color="darkred", size = 0.75) +
  geom_line(aes(y=fc.ts), color="orangered", linetype="twodash", size = 1.25) + 
  geom_ribbon(data=fore, aes(ymin=(lower95), ymax=(upper95)),alpha=0.15, fill="steelblue")+
  theme_bw() + 
  theme(legend.position="none") +
  xlab("Date") + 
  ylab("Predicted CTL- Conversations")+
  ggtitle("                  Crisis text volume for emotional abuse over time") +
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_grey(start=0.8, end=0.2)
sumCTL

#==============================================================================#
#making a single picture of the multiple ARIMA models
#==============================================================================#

png(file = 'gridExtra.png', width = 10, height = 6, units = 'in', res = 600)
grid.arrange(sumCTL, depressed, suicide, stress_anxiety, ncol = 2, padding = 20)
dev.off()

#==============================================================================#
#ARIMAX model alternative method
#==============================================================================#

Data_ts <- ts(Data)

# A look at what the covariates look like over time
autoplot(Data_ts[,6:9], facets = TRUE) +
  xlab("Time") +
  ylab("instences") +
  ggtitle("instences per day")

# A look at all the converastation concerns over time 
autoplot(Data_ts[,2:5], facets = TRUE) +
  xlab("Time") +
  ylab("instences") +
  ggtitle("instences per day")

#Step 1: Check for stationarity
#Is test-statistic less then the critical value? if not you fail to reject the null hypothesis

Data_ts[, "Case_diff"] %>% 
  ur.kpss() %>%
  summary()

Data_ts[, "Death_diff"] %>%
  ur.kpss() %>%
  summary()

Data_ts[, "sumCTL"] %>%
  ur.kpss() %>%
  summary()

#Step 2: Fit the model
fit <- auto.arima(Data_ts[, "sumCTL"],
                  seasonal = F,
                  xreg = cbind(Data_ts[, "Case_diff"], Data_ts[,"Death_diff"]),
                  stepwise = F,
                  trace = T)

#Step 3: Check residuals

checkresiduals(fit)

#Step 4: Foracst the model

fcast <- fit %>%
  forecast(xreg = cbind(Data_ts[, "Case_diff"], Data_ts[,"Death_diff"]))

#Step 6: plot the model

autoplot(fcast) +
  xlab("Time") +
  ylab("instences")



