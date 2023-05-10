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
library(zoo) #Calculating a rolling mean
library(cowplot)
library(sjPlot)

#install.Rtools()

#Read file: Air Temperature and Soil Temperature. Provide full path to file
dfair <- read.csv("Data/Climate_data_Zackenberg/Air_temperature/View_ClimateBasis_Zackenberg_Data_Air_temperature_Air_temperature__200cm_@_60min_sample__DegreesC260520221737533841.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
#dfsoil1 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_0cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__0cm__60min_average__DegreesC121020211635024106.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
#dfsoil2 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_5cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__5cm__60min_average__DegreesC121020211636280909.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
#dfsoil3 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_10cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__10cm__60min_average__DegreesC121020211637203249.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)

dfsoil1 <- read.table("Data/Climate_data_Zackenberg/Soil_temperature_0cm/ClimateBasisZackenberg_Climate_mainAWS_ET02cm000_60min.dat",sep="\t",stringsAsFactors = FALSE, header = FALSE, skip=3)
dfsoil2 <- read.table("Data/Climate_data_Zackenberg/Soil_temperature_5cm/ClimateBasisZackenberg_Climate_mainAWS_ET01cm005_60min.dat",sep="\t",stringsAsFactors = FALSE, header = FALSE, skip=3)
dfsoil3 <- read.table("Data/Climate_data_Zackenberg/Soil_temperature_10cm/ClimateBasisZackenberg_Climate_mainAWS_ET01cm010_60min.dat",sep="\t",stringsAsFactors = FALSE, header = FALSE, skip=3)


head(dfsoil1)

dfair = dfair %>% rename("Date"="AT...C.","HourTemp"="AT...C.") #Fejl vises ved Date. 
#dfsoil1 = dfsoil1 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..0cm...60min.average..Â.C.") #Fejl vises ved Date. 
#dfsoil2 = dfsoil2 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..5cm...60min.average..Â.C.")
#dfsoil3 = dfsoil3 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..10cm...60min.average..Â.C.")

dfsoil1 = dfsoil1 %>% rename("Date"="V1", "Time"="V2", "HourTemp" = "V3") #Fejl vises ved Date. 
dfsoil2 = dfsoil2 %>% rename("Date"="V1", "Time"="V2", "HourTemp" = "V3")
dfsoil3 = dfsoil3 %>% rename("Date"="V1", "Time"="V2", "HourTemp" = "V3")

colnames(dfair) #Når script åbnes påny laver den uforstaaelige bogstaver om til ?

#Add sensor column. Inden datasættene forbindes skal der tilføjes en kolonne som angiver hvilke type data der er tale om.
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
df%>%
  subset(Sensor!="Air")%>%
  group_by(Sensor,Year,DOY)%>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T))%>%
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
#Beregner middelværdien af de tre soil temp sensorer baseret på time, DOY, måned og Year. Tager højde for NA værdier
which(df.soil.agg$HourTemp == "NaN")
#----------------------------------------------------------- 

#Seasonal variation in soil temperature
#Middeljordtemperatur for DOY plottes. Ikke grupperet for sensor
df.soil.agg %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T),
            Dayofyear=mean(DOY,na.rm=T),
            FTCycleSpring=ifelse(Min<0&Max>0&Dayofyear<200,1,0),
            FTCycleFall=ifelse(Min<0&Max>0&Dayofyear>200,1,0),
            FTCycle=ifelse(Min<0&Max>0,1,0),
            Growingseason=ifelse(DOYTemp>2,DOY,NA))->df1
##Der er problemer med NaN værdier i 337 punkter (se nedenstående kode). Dette er foraarsaget af NA værdier i det oprindelige datasæt, hvor vi har lavet alle -9999 om til NA.
which(df.soil.agg$HourTemp == "NaN")


#Calculating mean soil temperature per day
df.soil.agg %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T)) -> df_mean_temp

#Problem with NA values in 1996 day 224, 1999 day 223 and 2006 day 220 and 221
#Solve this by calculating a mean temperature from 7 days before and after
Temp <- c(6.8575,7.1022,5.1704,5.3754,5.9958,5.0339,4.5875,8.324,6.2072,5.7031,6.0521,6.0428,5.6092,4.9833)
df_mean_1996_224 <- data.frame(Temp)
mean(Temp)
Temp1 <- c(6.8651,7.9425,8.4319,7.2511,9.1071,7.5021,5.0082,5.7095,4.7331,4.7675,5.144,4.4225,4.2894,3.5032)
df_mean_1999_223 <- data.frame(Temp1)
mean(Temp1)
Temp2 <- c(9.645,10.6308,11.0878,9.3067,8.9128,7.8844,6.9271,7.2387,7.3506,7.3561,8.6874,9.1965,9.0997,8.2647)
df_mean_2006_220 <- data.frame(Temp2)
mean(Temp2)

df_mean_temp$DOYTemp[which(df_mean_temp$Year == "1996"&df_mean_temp$DOY == "224")] <- 5.931743
df_mean_temp$DOYTemp[which(df_mean_temp$Year == "1999"&df_mean_temp$DOY == "223")] <- 6.048371
df_mean_temp$DOYTemp[which(df_mean_temp$Year == "2006"&df_mean_temp$DOY == "220")] <- 8.684879
df_mean_temp$DOYTemp[which(df_mean_temp$Year == "2006"&df_mean_temp$DOY == "221")] <- 8.684879

#Calculating the rolling mean with 30 days
df_mean_temp$doymean<-rollmean(df_mean_temp$DOYTemp,k=30,fill=NA,align="right")

ggplot(df_mean_temp, aes(x=DOY, y=doymean)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#Calculating the rolling mean with 20 days
df_mean_temp$doymean_20<-rollmean(df_mean_temp$DOYTemp,k=20,fill=NA,align="right")

#Calculating the rolling mean with 20 days
df_mean_temp$doymean_60<-rollmean(df_mean_temp$DOYTemp,k=60,fill=NA,align="right")

write.csv(df_mean_temp, file="Data/Climate_data_Zackenberg\\df_mean_temp.csv", row.names = FALSE)

ggplot(df_mean_temp, aes(x=DOY, y=DOYTemp)) + 
  geom_line() +
  ylab("Mean temperature (degrees C)") +
  facet_wrap(~Year)

#Focus on temp between 120 and 200
df_mean_temp%>%
  subset(!DOY<120&!DOY>200) -> df_mean_temp_red

ggplot(df_mean_temp_red, aes(x=DOY, y=DOYTemp)) + 
  geom_line() +
  ylab("Mean temperature (degrees C)") +
  geom_hline(yintercept = 5, color = "red")+
  geom_vline(xintercept = 160)+
  facet_wrap(~Year)

write.csv(df_mean_temp_red, file="Data/Climate_data_Zackenberg\\df_mean_temp_red.csv", row.names = FALSE)


#Air temperature calculations
df %>%
  subset(Sensor=="Air")%>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T)) -> dfair1

ggplot(dfair1, aes(x=DOY, y=Max)) + 
  geom_line() +
  ylab("Max temperature (degrees C)") +
  facet_wrap(~Year)

#Calculating the rolling mean (air temp)
dfair1$doymean<-rollmean(dfair1$DOYTemp,k=30,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

write.csv(dfair1, file="Data/Climate_data_Zackenberg\\dfair1_mean_temp.csv", row.names = FALSE)

#Seasonal temperature for mean soil temp data
df1%>%
  ggplot(aes(x=DOY, y=DOYTemp)) + 
  geom_line() +
  ylab("Mean daily temperature (degrees C)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap(~Year)


#####Plot number of FT(freeze-thaw events) Fall####
df1%>%
  group_by(Year)%>%
  summarise(StartWinter=max(Growingseason,na.rm=T),
            EndWinter=min(Growingseason,na.rm=T),
            FTEventsSpring=sum(FTCycleSpring,na.rm=T),
            FTEventsFall=sum(FTCycleFall,na.rm=T))%>%
  ggplot(aes(x=Year, y=FTEventsFall)) +geom_point()


#Plot number of FT eventsFall
df1%>%
  group_by(Year)%>%
  summarise(StartWinter=max(Growingseason,na.rm=T),
            EndWinter=min(Growingseason,na.rm=T),
            FTEventsSpring=sum(FTCycleSpring,na.rm=T),
            FTEventsFall=sum(FTCycleFall,na.rm=T))%>%
  ggplot(aes(x=Year, y=FTEventsSpring+FTEventsFall)) +geom_point()
  #ggplot(aes(x=Year, y=EndWinter)) +geom_point()

#summarise number of FT events
df1%>%
  group_by(Year)%>%
  summarise(StartWinter=max(Growingseason,na.rm=T),
            EndWinter=min(Growingseason,na.rm=T),
            FTEventsSpring=sum(FTCycleSpring,na.rm=T),
            FTEventsFall=sum(FTCycleFall,na.rm=T))->df2

#Show how FT events vary across the season for each year
df1%>%
  group_by(Year,DOY)%>%
  summarise(FTEvents=sum(FTCycle,na.rm=T))%>%
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
  summarise(SeasonTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Season,value=SeasonTemp)->df7

###Calculate mean Temp for summer months
df1%>%
  subset(4==Month | 5==Month | 6==Month | 7==Month | 8==Month | 9==Month | 10==Month)%>%
  group_by(Year,Month)%>%
  summarise(MonthTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Month,value=MonthTemp)%>%
  setnames(., old =c('4', '5', '6', '7', '8', '9', '10') , new = c('April','May', 'June', 'July', 'Aug', 'Sep', 'Oct'))->dfmonth

dfair1%>%
  subset(4==Month | 5==Month | 6==Month | 7==Month | 8==Month | 9==Month | 10==Month)%>%
  group_by(Year,Month)%>%
  summarise(MonthTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Month,value=MonthTemp)%>%
  setnames(., old =c('4', '5', '6', '7', '8', '9', '10') , new = c('April','May', 'June', 'July', 'Aug', 'Sep', 'Oct'))->dfmonthair

ggplot(dfmonthair, aes(Year, July))+
  geom_point()+
  geom_smooth(method = "lm")

lm <- lm(July ~ Year, data=dfmonthair)
summary(lm)

write.csv(dfmonth, file = "Data/Climate_data_Zackenberg\\Temperature_monthly.csv", row.names=FALSE)

#Checking min and max temp
dfair1%>%
  subset(4==Month | 5==Month | 6==Month | 7==Month | 8==Month | 9==Month | 10==Month)%>%
  group_by(Year,Month)%>%
  summarise(MaxMonthTemp=max(Max,na.rm=T))%>%
  spread(key=Month,value=MaxMonthTemp)%>%
  setnames(., old =c('4', '5', '6', '7', '8', '9', '10') , new = c('April','May', 'June', 'July', 'Aug', 'Sep', 'Oct'))->dfmaxmonthair

ggplot(dfmaxmonthair, aes(Year, July))+
  geom_point()+
  geom_smooth(method = "lm")

#Checking min and max temp
dfair1%>%
  subset(4==Month | 5==Month | 6==Month | 7==Month | 8==Month | 9==Month | 10==Month)%>%
  group_by(Year,Month)%>%
  summarise(MinMonthTemp=min(Min,na.rm=T))%>%
  spread(key=Month,value=MinMonthTemp)%>%
  setnames(., old =c('4', '5', '6', '7', '8', '9', '10') , new = c('April','May', 'June', 'July', 'Aug', 'Sep', 'Oct'))->dfminmonthair

ggplot(dfminmonthair, aes(Year, July))+
  geom_point()+
  geom_smooth(method = "lm")



### Calculate mean Winter Temp per year

df1%>%
  subset(Season=='Winter')%>%
  group_by(Year,Month)%>%
  summarise(MonthTemp=mean(DOYTemp,na.rm=T))%>%
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
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T),
            Dayofyear=mean(DOY,na.rm=T),
            FTCycleSpring=ifelse(Min<0&Max>0&Dayofyear<200,1,0),
            FTCycleFall=ifelse(Min<0&Max>0&Dayofyear>200,1,0),
            FTCycle=ifelse(Min<0&Max>0,1,0),
            Growingseason=ifelse(DOYTemp>2,DOY,NA))->dfair1

dfair1%>%
  group_by(Year)%>%
  summarise(StartWinter_A=max(Growingseason,na.rm=T),
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
  summarise(SeasonTemp=mean(DOYTemp,na.rm=T))%>%
  spread(key=Season,value=SeasonTemp)->dfair2a

dfair1%>%
  subset(Season=='Winter')%>%
  group_by(Year,Month)%>%
  summarise(MonthTemp=mean(DOYTemp,na.rm=T))%>%
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
#dfair4<-select(dfair3, c('Year...1', 'prevFall_temp_A', 'FTevents_A', 'DurationW_A','Winter_A','Spring_A', 'Summer_A'))
head(dfair)
dfair4<-dplyr::select(dfair, c('Year...1','Spring', 'Summer'))
dfair5<-select(dfair4,c('Year...1', 'Summer_A'))
dfair4%>%
  rename(Year = Year...1)->dfair6
dfair4%>%
  rename(Year = Year...1)->dfair4

write.csv(dfair4, file = "Data/Climate_data_Zackenberg\\Air_seasonal.csv", row.names=FALSE)

dfair4 <- read.csv("Data/Climate_data_Zackenberg\\Air_seasonal.csv",sep=",", header = TRUE)

dfair4%>%
  rename(Year = Year...1)->dfair4

####For luft temp er der beregnet middeltemp for hver sæson. Beregnes for måned hvis aftale med Toke####
dfair4 <- dfair4[complete.cases(dfair4), ]  

air_summer <- ggplot(dfair4, aes(Year, Summer))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", size = 2)+
  ylab("June-August average air temp (°C)")+
  xlab("")+
  ylim(2.5,7.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1), axis.title=element_text(size=15,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  geom_hline(yintercept = c(3,4,5,6,7), linetype="dashed")

lm <- lm(Summer~Year, dfair4)
summary(lm)

air_spring <- ggplot(dfair4, aes(Year, Spring))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", size = 2, linetype = "dashed")+
  ylab("April-May average air temp (°C)")+
  xlab("")+
  ylim(-13,-5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title=element_text(size=15,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  geom_hline(yintercept = c(-12,-10,-8,-6), linetype="dashed")

lm <- lm(Spring~Year, dfair4)
summary(lm)

plot_grid(air_spring, air_summer, labels = "AUTO")

#Combined plot

Temp <- ggplot(data=dfair4, aes(Year, Summer))+
  geom_point(size = 3, color = "blue")+
  geom_smooth(method = "lm", size = 2, se = FALSE)+
  geom_line(size = 1, color = "blue")+
  geom_point(data = dfair4, aes(x=Year, y=Spring), size = 3, color = "black")+
  geom_line(data = dfair4, aes(x=Year, y=Spring), size = 1, color = "black")+
  geom_smooth(data = dfair4, aes(x=Year, y=Spring),method = "lm", size = 2, linetype = "dashed", color = "black", se = FALSE)+
  ylab("Air temperature (°C)")+
  xlab("")+
  ylim(-16,11)+
  theme_bw()+
  annotate("text", x = 2004, y = 10, label = "Summer temperature", size = 7)+
  annotate("text", x = 2004, y = -3, label = "Spring temperature", size = 7)+
  scale_y_continuous(breaks = c(-14,-10,-6,-2,2,6,10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=20, color = "black"), axis.text.x = element_text(angle = 45, hjust = 1, color ="black"), 
        axis.title=element_text(size=20,face="bold", color = "black"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))
  #geom_hline(yintercept = c(-12,-6,0,6), linetype="dashed")

cowplot::plot_grid(Snow, Temp, labels = c("(a)", "(b)"), label_size = 20, align = c("hv"))


#Kombinere variable i et datasæt. Skal tilpasses til både jord og luft temperatur
dfall<-bind_cols(df2,df7,df8)
dfall$Year...8<-NULL
dfall$Year...12<-NULL
dfall%>%
  rename(Year = Year...1)->dfall1

### Lag variables to get Previous fall temp for a particular year
dfall2 <- mutate(dfall1, prevFall_temp = lag(Autumn), FTevents_LateSeason=lag(FTEventsFall),FTevents=FTevents_LateSeason+FTEventsSpring)


parameters<-select(dfall2, c('Year', 'prevFall_temp', 'FTevents', 'DurationW','Winter','Spring', 'Summer'))
parameters_all<-bind_cols(parameters,dfair6)
parameters_all$Year...8<-NULL
parameters_all%>%
  rename(Year = Year...1)->parameters_all

env.data<-subset(parameters,Year!=1995)
#env.data$Year1<-NULL
write.csv(env.data, file = "Data/Climate_data_Zackenberg\\Climate_parameters_Zackenberg.csv", row.names=FALSE)
#write.csv(env.data,"Data/Climate_parameters_Zackenberg_200314.csv")

#Plots
#install.packages("cowplot")
library(cowplot)
p1<-ggplot(data=env.data,aes(Year,prevFall_temp))+ylab("Previous Fall Temperature (°C)")+geom_line(size = 1)+geom_smooth(method="lm", linetype = "dashed")+theme_classic()+
  annotate("text", x = 2002, y = 0, label = "Rsq = 0.077, F = 1.92, P = 0.18")+theme(axis.text=element_text(size=12),axis.title.y = element_text(size=15))
p2<-ggplot(data=env.data,aes(Year,FTevents))+geom_line(size = 1)+geom_smooth(method="lm", linetype = "dashed")+theme_classic()
p3<-ggplot(data=env.data,aes(Year,DurationW))+geom_line(size = 1)+geom_smooth(method="lm", linetype = "dashed")+theme_classic()
p4<-ggplot(data=env.data,aes(Year,Winter))+ylab("Winter Temperature (°C)")+geom_line(size = 1)+geom_smooth(method="lm", linetype = "dashed")+theme_classic()+
  annotate("text", x = 2002, y = -10, label = "Rsq = 0.016, F = 0.38, P = 0.54")+theme(axis.text=element_text(size=12),axis.title.y = element_text(size=15))
p5<-ggplot(data=env.data,aes(Year,Spring))+ylab("Spring Temperature (°C)")+geom_line(size = 1)+geom_smooth(method="lm", linetype = "dashed")+theme_classic()+
  annotate("text", x = 2002, y = -7, label = "Rsq = 0.058, F = 1.42, P = 0.25")+theme(axis.text=element_text(size=12),axis.title.y = element_text(size=15))
p6<-ggplot(data=env.data,aes(Year,Summer))+ylab("Summer Temperature (°C)")+geom_line(size = 1)+geom_smooth(method="lm", linetype = "dashed")+theme_classic()+
  annotate("text", x = 2002, y = 9, label = "Rsq = 0.004, F = 0.098, P = 0.76")+theme(axis.text=element_text(size=12),axis.title.y = element_text(size=15))
#p7<-ggplot(data=env.data,aes(Year,prevSummerPrecip))+geom_line()+geom_smooth(method="lm")+theme_classic()


plot_grid(p4,p1,p5,p6)

#Linear regression analysis of temperature data
Spring <- lm(Spring ~ Year, env.data)
summary(Spring)

Summer <- lm(Summer ~ Year, env.data)
summary(Summer)

Fall <- lm(prevFall_temp ~ Year, env.data)
summary(Fall)

Winter <- lm(Winter~Year, env.data)
summary(Winter)


p1A<-ggplot(data=env.data,aes(Year,prevFall_temp_A))+geom_line()+geom_smooth(method="lm")+theme_classic()
p2A<-ggplot(data=env.data,aes(Year,FTevents_A))+geom_line()+geom_smooth(method="lm")+theme_classic()
p3A<-ggplot(data=env.data,aes(Year,DurationW_A))+geom_line()+geom_smooth(method="lm")+theme_classic()
p4A<-ggplot(data=env.data,aes(Year,Winter_A))+geom_line()+geom_smooth(method="lm")+theme_classic()
p5A<-ggplot(data=env.data,aes(Year,Spring_A))+geom_line()+geom_smooth(method="lm")+theme_classic()
p6A<-ggplot(data=env.data,aes(Year,Summer_A))+geom_line()+geom_smooth(method="lm")+theme_classic()


plot_grid(p4A,p1A,p2A,p5A,p3A,p6A)

