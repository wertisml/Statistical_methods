library(forecast)
library(ggplot2)
library(gridExtra)
library(xts)
library(dplyr)
library(lubridate)
library(astsa)
library(reshape2)
library(padr)
library(data.table)
library(plyr)

setwd("~/ARIMA/All_Calls")#or your favorite working directory
MOV_3 <- data.frame(fread("C:\\Users\\wertisml\\Documents\\CTL\\CTL_Race_and_Gender_By_Population.csv")) #CTL file
HIP_Control <- read.csv("HIP-Control.csv", header=T, sep = ",") #or this df <- data.frame(areacode = c(318))

MOV_3$Date<-as.Date(MOV_3$Date, "%Y-%m-%d")
#head(MOV_3$conversation_start_time_utc,4)
head(MOV_3$Date,4)
MOV_3$Year<-year(MOV_3$Date)
MOV_3$month<-month(MOV_3$Date)
MOV_3$areacode <- MOV_3$area_code

fullCTL <- MOV_3 %>%
  group_by(Date)%>%
  mutate(sumCTL = (Sample), areacode = area_code, Year = Year, substance=(substance_abuse), depressed=(depressed),
         suicide=(suicidal_thoughts),self_harm= (self_harm), stress_anxiety = (anxiety_and_stress),
         relationship=(relationship),bereavement=(bereavement), bully=(bullying), isolated=(isolation),
         eating=(eating),abuse=(issue_yes_abusephysical),lgbtq=(sexual_identity),other=(Other),
         racism=(Racism),covid_19=(COVID_19),sexual_abuse =(sexual_abuse), emotional_abuse = (emotional_abuse),
         uspecified_abuse = (abuse_unspecified), gender = (gender_agg), race = (race_agg_no_H), sexual_preferance = (sexuality_agg),
         age = (age_agg))%>%
  arrange(Date)%>%
  select(Date, sumCTL, areacode, Year, substance, depressed, suicide, self_harm, stress_anxiety,
         relationship,bereavement, bully, isolated, eating,abuse,lgbtq, other, racism, covid_19, sexual_abuse,
         emotional_abuse, uspecified_abuse, gender, race, sexual_preferance, age)


fullCTL$Date <- as.Date(fullCTL$Date, "%Y-%m-%d")
fullCTL<-data.frame(fullCTL)
fullCTL<-fullCTL %>% pad()#fills in any day that someone might not have called the CTL
fullCTL <- fullCTL %>% mutate_if(is.integer, ~replace(., is.na(.), 0))

c <- HIP_Control$areacode

Data<- fullCTL %>%
  dplyr::filter(Year==2021) %>%
  dplyr::filter(areacode %in% c) %>%
  pad()%>%
  ddply("Date", numcolwise(sum))

Data$Time <- fifelse(Data$Date <= "2021-08-30", "Before", "After")
Data[is.na(Data)]<-0
ARIMA <- Data[, 1:2]

fwrite(Data, paste0("Control_All_Calls_Ida.csv"))
fwrite(ARIMA, paste0("Control_timeseries_hurricane_base_Ida.csv"))
