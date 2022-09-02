rm(list = ls())
library(forecast)
library(ggplot2)
library(gridExtra)
library(xts)
library(dplyr)
library(lubridate)
library(astsa)
library(reshape2)
library(padr)
library(zoo)
library(tfarima)
library(lmtest)
library(gridExtra)
library(tidyr)
library(data.table)

setwd("~/GRAM/ARIMA/Texas")

All_Calls <- read.csv("C:\\Users\\owner\\Documents\\GRAM\\ARIMA\\Texas\\All_Calls_Texas_Ice_Storm.csv")

#==============================================================================#
#Set up
#==============================================================================#

Data <- All_Calls %>%
  filter(Date >= "2021-01-01") %>%
  filter(Date <= "2021-12-10") %>%
  mutate(Time = fifelse(Date > "2021-02-10", 1, 0))

Data$Date <- as.Date(Data$Date)

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
#==============================================================================#
# Creating the time series
#==============================================================================#

dat <- subset(Data) 
myvars <- c("sumCTL")
#myvars <- c("suicide") 
#myvars <- c("stress_anxiety")
#myvars <- c("depressed")#can change if we want to look at different variables
dat <- dat[myvars]

## Create a daily Date object - from the start to the end of the data set
inds <- seq(as.Date(Data$Date[1]), as.Date(Data$Date[extent]), by = "day")
# Uses the date object to create a time series based off the dataframe
dat <- ts(dat,
          start = c(Year, as.numeric(format(inds[1], "%j"))),
          frequency = 365)

#==============================================================================#
# Test Ramp, Step, and Pulse
#==============================================================================#

# Step change (also called a level shift): A sudden, sustained change where 
#the time series is shifted either up or down by a given value immediately 
#following the intervention. The step change variable takes the value of 0 prior
#to the start of the intervention, and 1 afterwards.
step <- as.numeric(as.yearmon(time(dat))>=as.numeric(as.yearmon(Data$Date[event_day-1]))) #
#step

# Ramp: A change in slope that occurs immediately after the intervention. 
#The ramp variable takes the value of 0 prior to the start of the intervention 
#and increases by 1 after the date of the intervention.
ramp <- append(rep(0, (event_day-1)), seq(1,forecast,1)) #first is 0s for the number of days pre event and second is number of increasing days post event
#ramp  

#Pulse: A sudden, temporary change that is observed for one or more time points 
#immediately after the intervention and then returns to baseline level. The pulse 
#variable takes the value of 1 on the date of the intervention, and 0 otherwise.
pulse <- InterventionVar(as.yearmon(time(dat)), (event_day - 1))
#pulse

#==============================================================================#
# Test ARIMA models
#==============================================================================#

# Use automated algorithm to identify p/q parameters
# Specify first difference = 1 and seasonal difference = 1

#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(ramp), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(step), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(pulse), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(step, pulse), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(ramp, pulse), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(step, ramp), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, xreg=cbind(ramp, step, pulse), stepwise=F, trace=T)
#model1 <- auto.arima(dat, seasonal=F, stepwise=F, trace=T)
#coeftest(model1)

#code <- model1[["arma"]]

#p <- code[1]
#q <- code[2]
#d <- code[6]

model2 <- Arima(window(dat, end=c(last_year, (1+ end_day))), xreg = step, order=c(3,0,0))

#model2 <- Arima(window(dat, end=c(Year, extent)), xreg = step, order=c(3,0,0))

#coeftest(model2)
#sqrt(diag(vcov(model2)))

#==============================================================================#
#Building for plot
#==============================================================================#

# When not using xreg

fc <- forecast(model2, h = forecast, level = c(80, 90, 95))
fc.ts <- ts(as.numeric(fc$mean), start=c(ts_year, doy), frequency=365)
dat.ts.2 <- ts.union(dat, fc.ts)

# When using xreg

fc <- forecast(model2, xreg = step, level = c(80, 90, 95)) 
fc.ts <- ts(as.numeric(fc$mean[event_day:extent]), start=c(ts_year, doy), frequency=365) #The length needs to be the length of the forecasted amount
dat.ts.2 <- ts.union(dat, fc.ts)


data <- data.frame(dat.ts.2) %>%
  drop_na(dat) %>%
  mutate(Date = seq(extent)) ####This needs to be changed as the dates being looked at change, this is the total extent of dataframe being used

fore <- data.frame(matrix(ncol = 2, nrow = forecast)) #nrow is the days ahead

# Provide column names
colnames(fore) <- c('upper', 'lower')
upper <- as.matrix(fc[["upper"]])
upper <- data.frame(upper[1:forecast,])
fore$upper90 <- upper$X90.
fore$upper95 <- upper$X95.
lower <- as.matrix(fc[["lower"]])
lower <- data.frame(lower[1:forecast,])
fore$lower90 <- lower$X90.
fore$lower95 <- lower$X95.

fore$Date <- seq(1,forecast)+event_day #change numbers, 1, value of h, + days used as pre event data 

data<-merge(data,fore, by="Date", all.x=T)

#==============================================================================#
#Plot the ARIMA model + forecast
#==============================================================================#

stress_anxiety <- ggplot(data = data, aes(x=as.Date(Date + (as.numeric(Data[1,1])-1)))) + 
  geom_line(aes(y=dat), color="darkred", size = 0.5) +
  geom_line(aes(y=fc.ts), color="orangered", linetype="twodash", size = 1.) + 
  geom_ribbon(data=fore, aes(ymin=(lower95), ymax=(upper95)),alpha=0.5, fill="steelblue")+
  theme_bw() + 
  theme(legend.position="none") +
  xlab("Date") + 
  ylab("c)")+
  theme(axis.title.y=element_text(angle=0, vjust=1))+
  #ggtitle("                                         Crisis text volume for stress and anxiety over time") +
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_grey(start=0.8, end=0.2)

#==============================================================================#
# Create grid for the ARIMA model + forecast
#==============================================================================#

png(file = 'gridExtra.png', width = 10, height = 6, units = 'in', res = 600)
grid.arrange(sumCTL, suicide, stress_anxiety, depressed, ncol = 2, padding = 20)
dev.off()

#==============================================================================#
#
#==============================================================================#

#total value of forecasted
sum(data$fc.ts, na.rm = TRUE)
#Total value of actual
sum(data$dat, na.rm = TRUE)
#total upper confidence
sum(data$upper95, na.rm = TRUE)
sum(data$upper90, na.rm = TRUE)
#Total lower confidence 
sum(data$lower95, na.rm = TRUE)
sum(data$lower90, na.rm = TRUE)


