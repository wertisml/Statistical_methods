#==============================================================================#
# POOLING COMPLEX MULTI-PARAMETER ASSOCIATIONS
#==============================================================================#

# LOAD PACKAGES
library(mixmeta) ; library(dlnm) ; library(scales)
library(dplyr)

# LOAD COEF/VCOV FROM FIRST-STAGE MODELS
setwd("~/GRAM/Gasparini/On_My_Own/Files/Cities")
tmeanpar <- read.csv(file="tmeanpar.csv")
coef <- as.matrix(tmeanpar[,grep("coef", names(tmeanpar))])
vcov <- as.matrix(tmeanpar[,grep("vcov", names(tmeanpar))])

# LINK WITH CENSUS DATA
cityind <- tmeanpar[,1:4,]
citycensus <- read.csv("NC_Cities.csv")
city_info <- read.csv("Census_info.csv")

citycensus <- left_join(citycensus, city_info, by = c("order" = "loc")) %>%
   mutate(Percent_HS_Degree = 1 - Percent_No_HS_Diploma) %>%
   select(city, Total_Pop, Percent_Unemployment, Percent_HS_Degree)

cityind <- merge(cityind, citycensus, by="city") 

cityind <- cityind[complete.cases(cityind),]

#==============================================================================#
# Set-up
#==============================================================================#

# cityind <- cityind %>%
#   mutate(Percent_Unemployment = Percent_Unemployment/100)

#==============================================================================#
# RUN THE MODELS
#==============================================================================#

# MODEL WITH NO META-PREDICTOR
model0 <- mixmeta(coef~1, vcov, data=cityind, method="ml")

# SUMMARY AND HETEROGENEITY TEST
summary(model0)
qtest(model0)

# # MODELS WITH A SINGLE META-PREDICTOR
# # PREDICTORS: TOTAL POP, % OF PEOPLE WITH HIGH-SCHOOL DEGREE, % OF UNEMPLOYED
# model1 <- update(model0, .~Total_Pop)
# model2 <- update(model0, .~Percent_HS_Degree)
# model3 <- update(model0, .~Percent_Unemployment)
# 
# # FULL MODEL
# model4 <- update(model0, .~Total_Pop+Percent_HS_Degree+Percent_Unemployment)
# summary(model4)
# 
# # MODEL COMPARISON AND TESTS
# AIC(model0, model1, model2, model3, model4)
# BIC(model0, model1, model2, model3, model4)
# drop1(model4, test="Chisq")
# 
# # MODEL SELECTION (STEP FORWARD)
# stats::step(model0, .~Total_Pop+Percent_HS_Degree+Percent_Unemployment)

#==============================================================================#
# PLOT THE AVERAGE EXPOSURE-RESPONSE RELATIONSHIPS
#==============================================================================#

# LOAD AVERAGE TEMPERATURE DISTRIBUTION ACROSS CITIES
avgtmeansum <- read.csv("avgtmeansum.csv")
TAVG <- read.csv("tmean.csv")
tmean <- avgtmeansum$tmean

# DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
bvar <- onebasis(tmean, df=4, fun="bs") #same degrees as in the first stage 

# DEFINE THE CENTERING POINT (AT POINT OF MINIMUM RISK)
cen <- sum(TAVG)/nrow(TAVG)

# PREDICT THE ASSOCIATION
cp <- crosspred(bvar, coef=coef(model0), vcov=vcov(model0), model.link="log",
  at=tmean, cen=cen)

# PLOTTING LABELS
# PLOT
plot(cp, ylim=c(0.75,1.3), xlab="Temperature (C)", ylab="RR", main="North Carolina")
abline(v=cen, lty=2)
abline(v=c(tmean[3], tmean[99]), lty=3, col=grey(0.8)) 

# RR locations
pred <- crosspred(bvar, coef=coef(model0), vcov=vcov(model0), model.link="log",
                at=c(tmean[3], cen, tmean[99]), cen=cen)

predictlist <- data.frame(pred[14:16])



