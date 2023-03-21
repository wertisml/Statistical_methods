library(data.table)
library(dplyr)
library(dlnm) 
library(mixmeta)
library(tsModel) 
library(splines) 
library(lubridate)
library(gnm)
library(scales)

setwd("~/GRAM/Gasparini/On_My_Own/Files/Regions")

Data <- fread("Sheps_Temp_Regions.csv")

Data <- Data %>%
  mutate(month = month(Date)) %>%
  #filter(sex == "F") %>%
  #filter(Age == 4) %>%
  #filter(race == 5) %>%
  arrange(Zip) %>%
  mutate(loc = cumsum(c(1,as.numeric(diff(Zip))!=0)),
         Region = "Mountains",
         doy = yday(Date),
         year = year(Date),
         month = month(Date),
         dow = wday(Date)) %>%
  rename(Outcome = Mental_Health,
         temp = TAVG) %>%
  dplyr::select(Date, temp, RH, Outcome, Zip, loc, Region, doy, year, month, dow) 

Data$Outcome <- as.numeric(Data$Outcome)

Data <- Data[complete.cases(Data),]

#==============================================================================#
# Build DLNM and pooled model
#==============================================================================#

# DEFINE SPLINES OF DAY OF THE YEAR
spldoy <- onebasis(Data$doy, "ns", df=3)

# DEFINE THE CROSS-BASIS FOR TEMPERATURE FROM THE EXPOSURE HISTORY MATRIX
# NB: USE group TO IDENTIFY LACK OF CONTINUITY IN SERIES BY MSOA AND YEAR
range <- round(range(Data$temp, na.rm = T),0)
knots <- range[1] + (range[2]-range[1])/4*1:3

argvar <- list(fun="ns", knots=knots)
arglag <- list(fun="ns", knots=1)
group <- factor(paste(Data$Zip, Data$year, sep="-"))
cbtmean <- crossbasis(Data$temp, lag=7, argvar=argvar, arglag=arglag,
                      group=group)
summary(cbtmean)

# DEFINE THE STRATA 
Data[, stratum:=factor(paste(Zip, year, month, sep=":"))]

# RUN THE MODEL
# NB: EXCLUDE EMPTY STRATA, OTHERWISE BIAS IN gnm WITH quasipoisson
Data[,  keep:=sum(Outcome)>0, by=stratum]
modfull <- gnm(Outcome ~ cbtmean + spldoy:factor(year) + factor(dow), 
               eliminate=stratum, data=Data, family=quasipoisson, subset=keep)

cpfull <- crosspred(cbtmean, modfull, cen=mean(Data$temp, na.rm=T))

#==============================================================================#
# Plot data  
#==============================================================================#

redpred <- crossreduce(cbtmean, modfull, cen=mean(Data$temp, na.rm=T))
lines <- quantile(Data$temp, c(2.5,50,97.5)/100, na.rm=T)
col <- c("darkgoldenrod3", "aquamarine3")

plot(cpfull, "overall", ylim=c(0.8,1.8), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
abline(v=c(lines[1], lines[3]), lty=2, col=grey(0.8))
abline(v = redpred$cen, lty = 2)

#==============================================================================#
# RR Results
#==============================================================================#

pred <- crosspred(cbtmean, modfull, cen = mean(Data$temp, na.rm=T),
                   at=c(lines[1],redpred$cen,lines[3]))
#plot(pred)
data.frame(pred[14:16])
 
