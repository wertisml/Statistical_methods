library(data.table)
library(dplyr)
library(dlnm) 
library(mixmeta)
library(tsModel) 
library(splines) 
library(lubridate)

setwd("~/DLNM")

Data <- fread("Sheps_Temp_Cities.csv")
cities <- read.csv("NC_Cities.csv")
cities <- cities %>%
  arrange(order)

Data <- Data %>%
  left_join(cities, by = c("loc" = "order")) %>%
   mutate(month = month(Date)) %>%
   filter(month >= 6, month <= 9) %>%
  # filter(sex == "F") %>%
  rename(Outcome = Mental_Health) %>%
  dplyr::select(Date, TAVG, RH, Outcome, loc) 

Data$Outcome <- as.numeric(Data$Outcome)

Data <- Data[complete.cases(Data),]

#==============================================================================#
#
#==============================================================================#

tmeanparlist <- tmeansumlist <- RHparlist <- TAVGlist <- predictlist <- vector("list", nrow(cities))

# RUN THE LOOP
for(i in seq(nrow(cities))) {
    
  # PRINT CITY
  cat(cities$cityname[i],"")
  
  data <- Data %>%
    filter(loc == i)
  
  #============================================================================#
  # ANALYSIS OF TEMPERATURE - Mental Health (SUMMER-ONLY)
  #============================================================================#
  
  #Creates 3 knots for the crossbasis lag space and are equally spaced in the log
  #scale of lags to allow for more flexible lag effects of shorter days
  range <- round(range(data$TAVG, na.rm = T),0)
  knots <- range[1] + (range[2]-range[1])/4*1:3

    # DEFINE CROSS-BASIS FOR TEMPERATURE
  cbtmean <- crossbasis(data$TAVG, lag=7, #the number of days of lag in the model
                        argvar=list(fun="bs", df = 4),
                        arglag = list(fun="integer"),
                        knots = knots)

  # RUN THE MODEL AND EXTRACT REDUCED PRED (RE-CENTRED LATER)
  model <- glm(Outcome ~ cbtmean + ns(RH, df = 2) + ns(yday(Date), df=7*4) 
               + wday(Date, label = TRUE), #Should this be included?
               data=data, family=quasipoisson())
  
  #Test AIC of the glm
   # -2*sum( dpois( model$y, model$fitted.values, log=TRUE))+
   #   2*summary(model)$df[3]*summary(model)$dispersion

   redpred <- crossreduce(cbtmean, model, cen=mean(data$TAVG, na.rm=T)) 
  # 
   lines <- quantile(data$TAVG, c(2.5,50,97.5)/100, na.rm=T)
  
  plot(redpred, ylim=c(0.5,1.5), xlab="Temperature (C)", ylab="RR", main=paste0(cities$cityname[i]))
  abline(v=c(lines[1], lines[3]), lty=2, col=grey(0.8))
  abline(v = redpred$cen, lty = 2)

  #pred <- crosspred(cbtmean, model, cumul=T)
  
    pred <- crosspred(cbtmean, model, cen = mean(data$TAVG, na.rm=T),
                      at=c(range[1]:range[2],lines[1],redpred$cen,lines[3]))
  # plot(pred)
  data.frame(pred[14:16])
   
  #plot(pred, ptype = "overall", 
  #     ylim=c(0.75,1.3), xlab="Temperature (C)", ylab="RR", main=paste0(cities$city[i]))

  # STORE PARAMETERS (COEF + VECTORIZED VCOV)
  ncoef <- length(coef(redpred))
  par <- c(coef(redpred), vechMat(vcov(redpred)))
  names(par) <- c(paste0("coef", seq(ncoef)),
                  paste0("vcov", seq(ncoef*(ncoef+1)/2)))
  tmeanpar <- data.frame(cities[i, c("city", "cityname", "state", "statename")],
                         t(par), row.names=i)
  tmeanparlist[[i]] <- tmeanpar
  TAVGlist[[i]] <- redpred$cen

  #==============================================================================#
  # ANALYSIS OF Relative Humidity - NON-EXTERNAL MORTALITY (FULL YEAR)
  #==============================================================================#
  
  # # DEFINE MOVING AVERAGE OF OZONE AT LAG 0-1
  # tmean <- runMean(data$TAVG, 0:1)
  # 
  # # DEFINE CROSS-BASIS FOR TEMPERATURE
  # cbtmean <- crossbasis(data$TAVG, lag=7, #the number of days of lag in the model
  #                       argvar=list(fun="bs", df = 4),
  #                       arglag = list(fun="strata"),
  #                       knots = knots)
  # 
  # # RUN THE MODEL AND EXTRACT PAR (ONLY IF ENOUGH NON-MISSING)
  # par <- if(nrow(na.omit(cbind(tmean, cbtmean))) < 300 ) c(NA,NA) else {
  #   model <- glm(Outcome ~ tmean + cbtmean + wday(Date, label = TRUE) + ns(Date, df=4*7),
  #                data=data, family=quasipoisson)
  #   c(coef(model)["tmean"]*2, vcov(model)["tmean","tmean"]*2)
  # }
  # names(par) <- c("coef", "vcov")
  # 
  # # STORE PARAMETERS (COEF + VCOV FOR 10-UNIT INCREASE)
  # RHpar <- data.frame(cities[i, c("city", "cityname", "state", "statename")],
  #                     t(par), row.names=i)
  # RHparlist[[i]] <- RHpar
  
  #============================================================================#
  # TEMPERATURE DISTRIBUTION (SUMMER ONLY)
  #============================================================================#
  
  # DEFINE PERCENTILES
  per <- c(1,2,2.5, 3:97, 97.5, 98,99,100)/100
  tmeansumlist[[i]] <- quantile(data$TAVG, per, na.rm=T)

} 

#==============================================================================#
# PREPARE AND STORE
#==============================================================================#

# RBIND COEF/VCOV TOGETHER IN DATAFRAMES
tmeanpar <- do.call(rbind, tmeanparlist)
RHpar <- do.call(rbind, RHparlist)
TAVG <- do.call(rbind, TAVGlist)

# Create state-average summer temperature distribution
avgtmeansum <- data.frame(perc=names(tmeansumlist[[1]]), 
                          tmean=apply(do.call(cbind, tmeansumlist), 1, mean))

# Write off the data sets
setwd("~/DLNM/Data")
fwrite(tmeanpar, "tmeanpar.csv")
#fwrite(RHpar, "RHpar.csv")
fwrite(avgtmeansum, "avgtmeansum.csv")
fwrite(TAVG, "tmean.csv")

