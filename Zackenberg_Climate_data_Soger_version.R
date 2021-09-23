.libPaths() #Tjek hvilken library path der bruges naar pakker installeres
lib = "C:/Users/au511627/Documents/R/win-library/4.1"
.libPaths(lib)  #kald denne i starten af din session.
#Der kan opstaa problemer med installering af pakker, hvis pathway til installation er forkert (f.eks. er common) 

#rm(list=ls())
#Removes all objects from the workspace.
library(tidyverse)
library(readxl) 
library(lubridate) #Functions to work with date-time data.
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("hms")
library(hms)#Makes it easier to store and format time-of-day values based on the difftime class.
#install.packages("strucchange")
library(strucchange) #Testing, monitoring and dating structural changes in linear regression models.
#install.packages("gvlma")
library(gvlma) #For assessing linear model assumptions. 
#install.packages("reshape2")
library(reshape2) #Outdated, better to use Tidyr. Makes it easier to transform data between wide and long formats.
#install.packages("data.table")
library(data.table) #Extension of data.frame. Fast aggregation of large data.

#install.Rtools()

#Read file: Air Temperature and Soil Temperature. Provide full path to file
dfair <- read.csv("Data/Climate_data_Zackenberg/Air_temperature/View_ClimateBasis_Zackenberg_Data_Air_temperature_Air_temperature__200cm_@_60min_sample__DegreesC070620211141103262.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
dfsoil1 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_0cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__0cm__60min_average__DegreesC07062021115034361.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
dfsoil2 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_5cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__5cm__60min_average__DegreesC070620211206482173.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
dfsoil3 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_10cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__10cm__60min_average__DegreesC070620211210219033.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)

dfair = dfair %>% rename("Date"="ï..Date","HourTemp"="Air.temperature..200cm...60min.average..Â.C.") #Fejl vises ved Date. 
dfsoil1 = dfsoil1 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..0cm...60min.average..Â.C.") #Fejl vises ved Date. 
dfsoil2 = dfsoil2 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..5cm...60min.average..Â.C.")
dfsoil3 = dfsoil3 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..10cm...60min.average..Â.C.")

colnames(dfair) #Når script åbnes påny laver den uforstaaelige bogstaver om til ?

#Add sensor column
dfair$Sensor<-"Air"
dfsoil1$Sensor<-"Soil0"
dfsoil2$Sensor<-"Soil5"
dfsoil3$Sensor<-"Soil10"

dfair$HourTemp <- as.numeric(dfair$HourTemp)
dfsoil1$HourTemp <- as.numeric(dfsoil1$HourTemp)
dfsoil2$HourTemp <- as.numeric(dfsoil2$HourTemp)
dfsoil3$HourTemp <- as.numeric(dfsoil3$HourTemp)


#combine datafiles
#Bind_rows samler tabellerne under hinanden
df<-bind_rows(dfair,dfsoil1,dfsoil2,dfsoil3)

#Add columns: Hour, DOY, MONTH, Year
df %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":")->df

df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <-NA
df$HourTemp <-as.numeric(df$HourTemp)

#Seasonal variation in soil temperature across all three soil temp series
#Middeljordtemperatur for DOY plottes
df%>%
  subset(Sensor!="Air")%>%
  group_by(Sensor,Year,DOY) %>% 
  summarize(DOYTemp=mean(HourTemp,na.rm=T))%>%
  ggplot(aes(x=DOY, y=DOYTemp,colour=Sensor)) + 
  geom_line() +
  ylab("Mean daily temperature (degrees C)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap(~Year)

#----------------------------------------------------------- 
# Find mean soil temperature across the 3 soil sensors for 
# each hour-day-month-year
df.soil <- df[df$Sensor!="Air",]
df.soil.agg <- aggregate(df.soil[c("HourTemp")], 
                         by = df.soil[c("Hour", "DOY", "Month", "Year")], FUN=mean, na.rm=T)
#Beregner middelværdien af de tre soil temp sensorer baseret på time, DOY, måned og Year
#----------------------------------------------------------- 

#Seasonal variation in soil temperature
df.soil.agg %>%
  group_by(Year,Month,DOY) %>% 
  summarize(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T),
            Dayofyear=mean(DOY,na.rm=T),
            FTCycleSpring=ifelse(Min<0&Max>0&Dayofyear<200,1,0),
            FTCycleFall=ifelse(Min<0&Max>0&Dayofyear>200,1,0),
            FTCycle=ifelse(Min<0&Max>0,1,0),
            Growingseason=ifelse(DOYTemp>2,DOY,NA))->df1
##Der er problemer med NaN værdier i 337 punkter (se nedenstående kode). Dette er foraarsaget af NA værdier i det oprindelige datasæt, hvor vi har lavet alle -9999 om til NA.
which(df.soil.agg$HourTemp == "NaN")

#Seasonal temperature for mean soil temp data
df1%>%
  ggplot(aes(x=DOY, y=DOYTemp)) + 
  geom_line() +
  ylab("Mean daily temperature (degrees C)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap(~Year)


#####Plot number of FT(freeze-thaw events) Fall####
df1%>%
  group_by(Year)%>%
  summarize(StartWinter=max(Growingseason,na.rm=T),
            EndWinter=min(Growingseason,na.rm=T),
            FTEventsSpring=sum(FTCycleSpring,na.rm=T),
            FTEventsFall=sum(FTCycleFall,na.rm=T))%>%
  ggplot(aes(x=Year, y=FTEventsFall)) +geom_point()


#Plot number of FT eventsFall
df1%>%
  group_by(Year)%>%
  summarize(StartWinter=max(Growingseason,na.rm=T),
            EndWinter=min(Growingseason,na.rm=T),
            FTEventsSpring=sum(FTCycleSpring,na.rm=T),
            FTEventsFall=sum(FTCycleFall,na.rm=T))%>%
  #  ggplot(aes(x=Year, y=FTEventsSpring+FTEventsFall)) +geom_point()
  ggplot(aes(x=Year, y=EndWinter)) +geom_point()

#Summarize number of FT events
df1%>%
  group_by(Year)%>%
  summarize(StartWinter=max(Growingseason,na.rm=T),
            EndWinter=min(Growingseason,na.rm=T),
            FTEventsSpring=sum(FTCycleSpring,na.rm=T),
            FTEventsFall=sum(FTCycleFall,na.rm=T))->df2

#Show how FT events vary across the season for each year
df1%>%
  group_by(Year,DOY)%>%
  summarize(FTEvents=sum(FTCycle,na.rm=T))%>%
  ggplot(aes(x=DOY, y=FTEvents)) + 
  geom_line() +
  ylab("Number of FTevents")+
  facet_grid(vars(Year,))

#Lag  for Winter Duration calculations if lag() is in mutate(), it creates NAs
df2$StartWin_Lag<-lag(df2$StartWinter)
df2$DurationW<-(365-df2$StartWin_Lag)+df2$EndWinter


####Add seasons to the df1 dataframe####
## Summer=JJA, Spring=April-May, Fall=Sep-Oct., Winter=Nov to March like in the RSOS paper
Seasons<-data.frame(Month=c(seq(1:12)),Season=c(rep("Winter",3),rep("Spring",2),rep("Summer",3),rep("Autumn",2),rep("Winter",2)))
df1$Season<-Seasons$Season[match(df1$Month,Seasons$Month)]

df1%>%
  subset(Season!='Winter')%>%
  group_by(Year,Season)%>%
  summarize(SeasonTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Season,value=SeasonTemp)->df7

###Calculate mean Temp for May, June, July, August, September
df1%>%
  subset(5==Month | 6==Month | 7==Month | 8==Month | 9==Month)%>%
  group_by(Year,Month)%>%
  summarize(MonthTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Month,value=MonthTemp)%>%
  setnames(., old =c('5', '6', '7', '8', '9') , new = c('May', 'June', 'July', 'Aug', 'Sep'))->df9


### Calculate mean Winter Temp per year

df1%>%
  subset(Season=='Winter')%>%
  group_by(Year,Month)%>%
  summarize(MonthTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Month,value=MonthTemp)%>%
  setnames(., old =c('1', '2', '3', '11', '12') , new = c('Jan', 'Feb', 'Mar', 'Nov', 'Dec'))->df8

### Lag winter months from previous year for calculations
df8$prevNov<-lag(df8$Nov)
df8$prevDec<-lag(df8$Dec)
df8<-select(df8,-c('Nov', 'Dec'))
### Calculate mean winter temp per year
df8$Winter<- rowMeans( df8[ , 2:6])


#Air temperature calculations
df %>%
  subset(Sensor=="Air")%>%
  group_by(Year,Month,DOY) %>% 
  summarize(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T),
            Dayofyear=mean(DOY,na.rm=T),
            FTCycleSpring=ifelse(Min<0&Max>0&Dayofyear<200,1,0),
            FTCycleFall=ifelse(Min<0&Max>0&Dayofyear>200,1,0),
            FTCycle=ifelse(Min<0&Max>0,1,0),
            Growingseason=ifelse(DOYTemp>2,DOY,NA))->dfair1

dfair1%>%
  group_by(Year)%>%
  summarize(StartWinter_A=max(Growingseason,na.rm=T),
            EndWinter_A=min(Growingseason,na.rm=T),
            FTEventsSpring_A=sum(FTCycleSpring,na.rm=T),
            FTEventsFall_A=sum(FTCycleFall,na.rm=T))->dfair2

### Lag  for Winter Duration calculations if lag() is in mutate(), it creates NAs
dfair2$StartWin_A_Lag<-lag(dfair2$StartWinter_A)
dfair2$DurationW_A<-(365-dfair2$StartWin_A_Lag)+dfair2$EndWinter_A

dfair1$Season<-Seasons$Season[match(dfair1$Month,Seasons$Month)]

dfair1%>%
  subset(Season!='Winter')%>%
  group_by(Year,Season)%>%
  summarize(SeasonTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Season,value=SeasonTemp)->dfair2a

dfair1%>%
  subset(Season=='Winter')%>%
  group_by(Year,Month)%>%
  summarize(MonthTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Month,value=MonthTemp)%>%
  setnames(., old =c('1', '2', '3', '11', '12') , new = c('Jan', 'Feb', 'Mar', 'Nov', 'Dec'))->dfair2b

### Lag winter months from previous year for calculations
dfair2b$prevNov<-lag(dfair2b$Nov)
dfair2b$prevDec<-lag(dfair2b$Dec)
dfair2b<-select(dfair2b,-c('Nov', 'Dec'))

### Calculate mean winter temp per year
dfair2b$Winter<- rowMeans( dfair2b[ , 2:6])

dfair<-bind_cols(dfair2,dfair2a,dfair2b)
### Lag variables to get Previous fall temp for a particular year
dfair3 <- mutate(dfair, prevFall_temp_A = lag(Autumn),FTevents_LateSeason_A=lag(FTEventsFall_A),FTevents_A=FTevents_LateSeason_A+FTEventsSpring_A,Winter_A=Winter,Spring_A=Spring,Summer_A=Summer)
dfair4<-select(dfair3, c('Year...1', 'prevFall_temp_A', 'FTevents_A', 'DurationW_A','Winter_A','Spring_A', 'Summer_A'))

colnames(dfair3)
#Fejl da der ikke er en enlig Year kolonne.

####For luft temp er der beregnet middeltemp for hver sæson. Beregnes for måned hvis aftale med Toke####



#Kombinere variable i et datasæt. Skal tilpasses til både jord og luft temperatur
#dfall<-bind_cols(df2,df7,df4, df8)

parameters<-select(df7, c('Year', 'Summer'))
#parameters<-select(dfall1, c('Year', 'prevFall_temp', 'FTevents', 'DurationW','Winter','Spring', 'Summer','prevSummerPrecip'))
parameters_all<-bind_cols(parameters,dfair4)
parameters_all$Year...3<-NULL
dfall$Year2<-NULL
dfall$Year3<-NULL

#env.data<-subset(parameters_all,Year!=1995)
#env.data$Year1<-NULL
#write.csv(env.data,"Data/Climate_parameters_Zackenberg_200314.csv")

#Plots
#install.packages("cowplot")
library(cowplot)
p1<-ggplot(data=parameters_all,aes(Year...1,Summer))+geom_point()+geom_smooth(method="lm")+theme_classic()
p1
p2<-ggplot(data=parameters_all,aes(Year...1,Summer_A))+geom_point()+geom_smooth(method="lm")+theme_classic()
p2

p2<-ggplot(data=env.data,aes(Year,FTevents))+geom_line()+geom_smooth(method="lm")+theme_classic()
p3<-ggplot(data=env.data,aes(Year,DurationW))+geom_line()+geom_smooth(method="lm")+theme_classic()
p4<-ggplot(data=env.data,aes(Year,Winter))+geom_line()+geom_smooth(method="lm")+theme_classic()
p5<-ggplot(data=env.data,aes(Year,Spring))+geom_line()+geom_smooth(method="lm")+theme_classic()
p6<-ggplot(data=env.data,aes(Year,Summer))+geom_line()+geom_smooth(method="lm")+theme_classic()
p7<-ggplot(data=env.data,aes(Year,prevSummerPrecip))+geom_line()+geom_smooth(method="lm")+theme_classic()

lm_summersoil<-lm(Summer~Year...1, data=parameters_all)
summary(lm_summersoil)
gvlma(lm_summersoil)

lm_summerair<-lm(Summer_A~Year...1, data=parameters_all)
summary(lm_summerair)
gvlma(lm_summerair)
