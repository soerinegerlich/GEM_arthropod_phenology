library(tidyverse)
library(readxl) 
library(lubridate) 

setwd("C:/Arbejde/R/Microclimate/")
#Read file: Air Temperature and Soil Temperature. Provide full path to file
dfa <- read_excel("View_ClimateBasis_Zackenberg_Data_Temperature_Air_temperature_200cm__60min_average_degC29032019163717266.xlsx")
dfs <- read_excel("View_ClimateBasis_Zackenberg_Data_Temperature_Soil_temperature_0cm__60min_average_degC300320192138206285.xlsx")

dfa <- read_excel("View_ClimateBasis_Zackenberg_Data_Air_temperature_Air_temperature__200cm_@_60min_sample__DegreesC07102021100307896.xlsx")
dfs <- read_excel("View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__0cm__60min_average__DegreesC071020211004185438.xlsx")

#Rename column: AT C and ST C
names(dfa)[3] <- "HourTemp"
names(dfs)[3] <- "HourTemp"

#Add sensor column
dfa$Sensor<-"Air"
dfs$Sensor<-"Soil"

#combine datafiles
df<-bind_rows(dfa,dfs)

#Add columns: Hour, DOY, MONTH, Year
df$Hour <- hour(hms(df$Time))
df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <-NA
df$HourTemp <-as.numeric(df$HourTemp)

#Seasonal variation in temperature
df %>% 
  group_by(Sensor,Year,DOY) %>% 
  summarize(DOYTemp=mean(HourTemp,na.rm=T)) %>% 
  ggplot(aes(x=DOY, y=DOYTemp,colour=as.factor(Year))) + 
  geom_line() +
  ylab("Mean daily temperature (degrees C)") +
  facet_wrap(~Sensor)

#Trends in mean monthly temp
df %>% 
  group_by(Sensor,Year,Month) %>% 
  summarize(Temp=mean(HourTemp,na.rm=T)) %>%
  ggplot(aes(x=Year, y=Temp,colour=Sensor)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  ylab("Mean monthly temperature (degrees C)") +
  facet_wrap(Sensor~Month,scales=("free_y"),ncol=6)

#Growing degree days accummulation
df %>% 
  group_by(Sensor,Year,Month,DOY) %>% 
  summarize(GDD=sum(ifelse(HourTemp>0,HourTemp/24,0),na.rm=T))%>%
  group_by(Sensor,Year)%>%
  mutate(cGDD=cumsum(GDD)) %>%
  subset(Year!=1995)%>% # subsetting due to missing data
  ggplot(aes(x=DOY, y=cGDD,colour=as.factor(Year))) + 
  geom_line(size=1) +
  coord_cartesian(xlim = c(140, 290)) +
  ylab("Growing degree days") +
  facet_wrap(~Sensor)

#growing degree days per year
df %>% group_by(Sensor,Year,Month,DOY) %>% 
  summarize(GDD=sum(ifelse(HourTemp>0,HourTemp/24,0),na.rm=T))%>%
  group_by(Sensor,Year)%>%
  mutate(cGDD=sum(GDD)) %>%
  subset(Year!=1995)%>% # subsetting due to missing data
  ggplot(aes(x=Year, y=cGDD)) + 
  geom_line()+geom_point() +
  ylab("Growing degree days") +  
  facet_wrap(~Sensor)

#Air and soil temp at snowmelt
df %>% 
  subset(Year==1999|Year==2005)%>%
  ggplot(aes(x=DOY+Hour/24, y=HourTemp,colour=as.factor(Year))) + 
  geom_line() +
  ylab("Temperature (degrees C)") +
  facet_wrap(~Sensor) +
  coord_cartesian(xlim = c(140, 190), ylim = c(-15, 30))

#Diurnal range at timing of snowmelt
df %>% group_by(Sensor,Year,Month,DOY) %>% 
  subset(Year==1999|Year==2005)%>%
  summarize(
    Range=max(HourTemp,na.rm=T)-min(HourTemp,na.rm=T)) %>%
  ggplot(aes(x=DOY, y=Range,colour=as.factor(Year))) + 
  geom_line() +
  ylab("Diurnal range in temperature (degrees C)") +
  coord_cartesian(xlim = c(80, 190)) +
  facet_wrap(~Sensor)

#Approximation of interannual variation in timing of snowmelt
df %>% group_by(Sensor,Year,Month,DOY) %>% 
  subset(Sensor=="Soil")%>%
  summarize(
    Range=max(HourTemp,na.rm=T)-min(HourTemp,na.rm=T),
    Snowcover=ifelse(Range<10,1,0)) %>%
  subset(Snowcover==0)%>%
  group_by(Sensor,Year)%>%
  summarize(Snowmelt=min(DOY)) %>%
  subset(Year!=1995)%>%
  ggplot(aes(x=Year, y=Snowmelt)) + 
  geom_line() + 
  geom_point() +
  ylab("Approximate timing of snowmelt (DOY))")

#Air and soil temp during freeze up
df %>% 
  subset(Year==1999|Year==2007)%>%
  ggplot(aes(x=DOY+Hour/24, y=HourTemp,colour=as.factor(Year))) + 
  geom_line()+
  coord_cartesian(xlim = c(240, 290), ylim = c(-20, 20)) +
  ylab("Temperature (degrees C)") +
  facet_wrap(~Sensor)

#Freeze-thaw cycles
df %>% group_by(Sensor,Year,Month,DOY) %>% 
  summarize(
    DOYTemp=mean(HourTemp,na.rm=T),
    Min=min(HourTemp,na.rm=T),
    Max=max(HourTemp,na.rm=T),
    FTCycle=ifelse(Min<0&Max>0,1,0)) %>%
  group_by(Sensor,Year,Month)%>%
  summarize(FTCycle=sum(FTCycle)) %>%
  ggplot(aes(x=Month, y=FTCycle,colour=as.factor(Year))) + 
  geom_line() +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab("Freeze-thaw cycles") +
  facet_wrap(~Sensor)

#Change in spring Freeze-thaw cycles
df %>% group_by(Sensor,Year,Month,DOY) %>% 
  summarize(
    DOYTemp=mean(HourTemp,na.rm=T),
    Min=min(HourTemp,na.rm=T),
    Max=max(HourTemp,na.rm=T),
    FTCycle=ifelse(Min<0&Max>0,1,0)) %>%
  group_by(Sensor,Year,Month)%>%
  summarize(FTCycle=sum(FTCycle)) %>%
  subset(Month<7&Month>3) %>%
  group_by(Sensor,Year)%>%
  summarize(FTCycle=sum(FTCycle)) %>%
  ggplot(aes(x=Year, y=FTCycle)) + 
  geom_point() +
  geom_smooth() +
  ylab("Freeze-thaw cycles") +
  facet_wrap(~Sensor)

#Change in fall Freeze-thaw cycles
df %>% group_by(Sensor,Year,Month,DOY) %>% 
  summarize(
    DOYTemp=mean(HourTemp,na.rm=T),
    Min=min(HourTemp,na.rm=T),
    Max=max(HourTemp,na.rm=T),
    FTCycle=ifelse(Min<0&Max>0,1,0)) %>%
  group_by(Sensor,Year,Month)%>%
  summarize(FTCycle=sum(FTCycle)) %>%
  subset(Month>7&Month<11) %>%
  subset(Year!=1995&Year<2015)%>% # subsetting due to missing data
  group_by(Sensor,Year)%>%
  summarize(FTCycle=sum(FTCycle)) %>%
  ggplot(aes(x=Year, y=FTCycle)) + 
  geom_point() +
  geom_smooth() +
  ylab("Freeze-thaw cycles") +
  facet_wrap(~Sensor)

