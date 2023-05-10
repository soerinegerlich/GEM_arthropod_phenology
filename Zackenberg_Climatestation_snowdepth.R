.libPaths() #Tjek hvilken library path der bruges naar pakker installeres.
lib = "C:/Users/au511627/Documents/R/win-library/4.1"
.libPaths(lib)  #kald denne i starten af din session.
#Der kan opstaa problemer med installering af pakker, hvis pathway til installation er forkert (f.eks. er common) 

#rm(list=ls())
#Removes all objects from the workspace.

library(writexl)
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
library(data.table)


dfsnowdepth <- read.csv("Data/Climate_data_Zackenberg/Snowcover_climatestation/data/View_ClimateBasis_Zackenberg_Data_Precipitation_Snow_depth_m130320221458115255.csv", sep="\t",stringsAsFactors = FALSE, header = TRUE)

#Timing of snowmelt in plots
df1 <- read.csv2("Data/Climate_data_Zackenberg\\Snowmelt_Zackenberg.csv",sep=",",stringsAsFactors = FALSE)


#Correcting column names
dfsnowdepth = dfsnowdepth %>% rename("Date"="ï..Date")
dfsnowdepth = dfsnowdepth %>% rename("SnowDepth"="Snow.depth..m.")


#Add columns HOUR, DOY, MONTH, YEAR
dfsnowdepth%>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":")->dfsnowdepth


dfsnowdepth$DOY <- yday(ymd(dfsnowdepth$Date))
dfsnowdepth$Month <- month(ymd(dfsnowdepth$Date))
dfsnowdepth$Year <- year(ymd(dfsnowdepth$Date))

#Turn all -9999 where measurements have not been made to NA's
dfsnowdepth$SnowDepth[dfsnowdepth$SnowDepth == -9999] <-NA
dfsnowdepth$SnowDepth <-as.numeric(dfsnowdepth$SnowDepth)


dfsnowdepth%>%
  group_by(Year,DOY) %>% 
  summarize(DOYsnowdepth=mean(SnowDepth,na.rm=T))%>%
  ggplot(aes(x=DOY, y=DOYsnowdepth)) + 
  geom_line() +
  ylab("Mean daily snow depth (m)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap(~Year)

dfsnowdepth%>%
  group_by(Year,Month,DOY) %>% 
  summarize(DOYsnowdepth=mean(SnowDepth,na.rm=T))->dfsnowdepth_mean

#dfsnowdepth_mean%>%
  #group_by(Year,DOY)%>%
  #summarize(Include=ifelse(DOYsnowdepth<0.1,1,0))->dfsnowmelt1

#Remove months <4 as in some years, there is no snow at the beginning of the year
dfsnowdepth_mean%>%
  subset(!Month<4)->dfsnowdepth_mean


dfsnowdepth_mean$DOYsnowdepth_include<-ifelse(dfsnowdepth_mean$DOYsnowdepth<0.1,1,0)

###Forsøg med at finde første DOY med <10 cm snedække
first_equal_to <- function(x,value){
  (x==value) & (cumsum(x==value)==1)
}

dfsnowdepth_mean%>%
  subset(!is.na(DOYsnowdepth_include))%>%
  group_by(Year)%>%
  mutate(first=first_equal_to(DOYsnowdepth_include,1))->dfsnowmelt2

dfsnowmelt2$first[dfsnowmelt2$first == FALSE] <- 0

dfsnowmelt2<-subset(dfsnowmelt2,first!="0")

dfsnowmelt_climatestation <- read_xlsx("Data/Climate_data_Zackenberg/Snowmelt_climatestation.xlsx")

Snow <- ggplot(data=dfsnowmelt_climatestation, aes(x=Year, y=DOY)) +
  ylab("Day of Year")+
  xlab("")+
  geom_point(size = 3)+
  geom_line(size = 1)+ 
  geom_smooth(method="lm", linetype = "dashed", color = "black", size = 1.5, se = FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1,size=20), plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"), 
        axis.title.y = element_text(size=20,vjust = 2, face = "bold"))+ 
  scale_x_continuous(breaks=c(1995, 2000, 2005, 2010, 2015, 2020))

Snowmelt <- lm(DOY~Year, dfsnowmelt_climatestation)
summary(Snowmelt)
Anova(Snowmelt)


dfsnowmelt2 %>%
  dplyr::select(-c(Month,DOYsnowdepth_include,first)) -> dfsnowmelt3

write.csv(dfsnowmelt3, file = "Data/Climate_data_Zackenberg\\Snowmelt_Climatestation.csv", row.names=FALSE) 
write_xlsx(dfsnowmelt3, "Data/Climate_data_Zackenberg\\Snowmelt_Climatestation.xlsx", col_names = TRUE)


####Compare DOY mean temperature with date of snowmelt to fill empty observations 
#in timing of snowmelt variable

df_temp <- read.csv("Data/Climate_data_Zackenberg\\df_mean_temp_red.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)

#To find snow melting dates for years with little snow we use temperature data
#The requirements for finding the zero curtain each year is as follows (following Rixen et al., 2022)
#1. Day of mean daily temp between -1 and +1 degrees C
#2. Mean daily temp (Max - min) rise above +2 degrees C after diurnal fluctuations

#Calculate Max - Min daily temp
df_temp %>%
  group_by(Year, Month, DOY)%>%
  summarise(K = Max-Min) -> df2

df_temp$K <- df2$K[match(paste0(df_temp$Year, df_temp$Month, df_temp$DOY),
                                                  paste0(df2$Year, df2$Month, df2$DOY))]

#Find days included in zero curtain for each year
df_temp$Include<-ifelse(df_temp$DOYTemp>-1.2&df_temp$DOYTemp<1.20&df_temp$K<2,1,0)

#Subset zero curtain periods
df3 <- subset(df_temp,Include=="1")

#The last day of the zero curtain period is the day of snowmelt
df3 %>%
  group_by(Year)%>%
  summarise(SnowmeltDOY = max(DOY)) -> df_snow


write.csv(df_snow, file = "Data/Climate_data_Zackenberg\\Snowmelt_TemperatureData.csv", row.names=FALSE) 
write_xlsx(df_snow, "Data/Climate_data_Zackenberg\\Snowmelt_TemperatureData.xlsx", col_names = TRUE)

df_temp <- read.csv("Data/Climate_data_Zackenberg\\Snowmelt_TemperatureData.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)
dfsnowmelt_climatestation <- readxl::read_xlsx("Data/Climate_data_Zackenberg/Snowmelt_Climatestation.xlsx")

dfsnowmelt_climatestation <- subset(dfsnowmelt_climatestation,Year!="1996")
dfsnowmelt_climatestation <- subset(dfsnowmelt_climatestation,Year!="2009")
dfsnowmelt_climatestation <- subset(dfsnowmelt_climatestation,Year!="2013")
dfsnowmelt_climatestation <- subset(dfsnowmelt_climatestation,Year!="2019")

ggplot(dfsnowmelt_climatestation, aes(DOY, SnowmeltDOY))+
  geom_point(size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  scale_x_continuous(breaks = c(150, 160, 170, 180, 190, 200))+
  scale_y_continuous(breaks = c(150, 160, 170, 180, 190, 200))+
  ylab("Snowmelt timing from snow depth censor")+
  xlab("Snowmelt timing from soil temperature")+
  theme(panel.background = element_rect(fill = "white"), 
        axis.title.y = element_text(face = "bold", size = 15,color = "black", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = -1), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), 
        panel.border = element_rect(color = "black", fill = NA, size = 2),
        plot.margin = margin(1,1,1,1, "cm"))

res1 <- cor.test(dfsnowmelt_climatestation$DOY, dfsnowmelt_climatestation$DOY, 
                 method = "pearson")
res1

#Comparing timing of snowmelt at climate station and in local plots

df1_Art1 <- subset(df1, Plot == "Art1")
df1_Art2 <- subset(df1, Plot == "Art2")
df1_Art3 <- subset(df1, Plot == "Art3")
df1_Art4 <- subset(df1, Plot == "Art4")
df1_Art5 <- subset(df1, Plot == "Art5")
df1_Art6 <- subset(df1, Plot == "Art6")
df1_Art7 <- subset(df1, Plot == "Art7")

df_Art1 = merge(df_temp, df1_Art1, by = "Year")
class(df_Art1$MeanSnowmelt)
df_Art1$MeanSnowmelt <- as.numeric(df_Art1$MeanSnowmelt)

df_Art2 = merge(df_temp, df1_Art2, by = "Year")
class(df_Art2$MeanSnowmelt)
df_Art2$MeanSnowmelt <- as.numeric(df_Art2$MeanSnowmelt)

df_Art3 = merge(df_temp, df1_Art3, by = "Year")
class(df_Art3$MeanSnowmelt)
df_Art3$MeanSnowmelt <- as.numeric(df_Art3$MeanSnowmelt)

df_Art4 = merge(df_temp, df1_Art4, by = "Year")
class(df_Art4$MeanSnowmelt)
df_Art4$MeanSnowmelt <- as.numeric(df_Art4$MeanSnowmelt)

df_Art5 = merge(df_temp, df1_Art5, by = "Year")
class(df_Art5$MeanSnowmelt)
df_Art5$MeanSnowmelt <- as.numeric(df_Art5$MeanSnowmelt)

df_Art6 = merge(df_temp, df1_Art6, by = "Year")
class(df_Art6$MeanSnowmelt)
df_Art6$MeanSnowmelt <- as.numeric(df_Art6$MeanSnowmelt)

df_Art7 = merge(df_temp, df1_Art7, by = "Year")
class(df_Art7$MeanSnowmelt)
df_Art7$MeanSnowmelt <- as.numeric(df_Art7$MeanSnowmelt)

res1 <- cor.test(df_Art1$SnowmeltDOY, df_Art1$MeanSnowmelt, 
                method = "pearson")
res1

res2 <- cor.test(df_Art2$SnowmeltDOY, df_Art2$MeanSnowmelt, 
                method = "pearson")
res2

res3 <- cor.test(df_Art3$SnowmeltDOY, df_Art3$MeanSnowmelt, 
                method = "pearson")
res3

res4 <- cor.test(df_Art4$SnowmeltDOY, df_Art4$MeanSnowmelt, 
                method = "pearson")
res4

res5 <- cor.test(df_Art5$SnowmeltDOY, df_Art5$MeanSnowmelt, 
                method = "pearson")
res5

res6 <- cor.test(df_Art6$SnowmeltDOY, df_Art6$MeanSnowmelt, 
                method = "pearson")
res6

res7 <- cor.test(df_Art7$SnowmeltDOY, df_Art7$MeanSnowmelt, 
                method = "pearson")
res7


ggplot(df_Art7, aes(SnowmeltDOY, MeanSnowmelt))+
  geom_point()+
  geom_smooth(method = "lm")

####Relating snowmelt dates with temperature####


#No snowmelt data for 1995 and 1996
df_temp <- subset(df_temp,Year!="1995")
df_temp <- subset(df_temp,Year!="1996")
df_temp <- subset(df_temp,Year!="1997")
dfsnowdepth_mean <- subset(dfsnowdepth_mean, Year!="1997")

#Match climate variables 
dfsnowdepth_mean$DOYTemp <- df_temp$DOYTemp[match(paste0(dfsnowdepth_mean$Year, dfsnowdepth_mean$Month, dfsnowdepth_mean$DOY),
                                                    paste0(df_temp$Year, df_temp$Month, df_temp$DOY))]


#Remove months <4 as in some years, there is no snow at the beginning of the year
dfsnowdepth_mean%>%
  subset(!Month<4)->dfsnowdepth_mean


dfsnowdepth_mean$DOYsnowdepth_include<-ifelse(dfsnowdepth_mean$DOYsnowdepth<0.1,1,0)

###Forsøg med at finde første DOY med <10 cm snedække
first_equal_to <- function(x,value){
  (x==value) & (cumsum(x==value)==1)
}

dfsnowdepth_mean%>%
  subset(!is.na(DOYsnowdepth_include))%>%
  group_by(Year)%>%
  mutate(first=first_equal_to(DOYsnowdepth_include,1))->dfsnowmelt2

dfsnowmelt2$first[dfsnowmelt2$first == FALSE] <- 0



#Attempt to plot 

par(mfrow=c(10,4), mar = c(1,1,1,1) + 0.1)

for (i in unique(dfsnowdepth_mean$Year)){
  dfsub <-subset(dfsnowdepth_mean,Year==i)
                # Additional space for second y-axis
plot(dfsub$DOY, dfsub$DOYTemp, pch = 16, col = 2, main = i)              # Create first plot
par(new = TRUE)                             # Add new plot
plot(dfsub$DOY, dfsub$DOYsnowdepth, pch = 17, col = 3, main = i,             # Create second plot without axes
    axis(side = 4, at = pretty(range(dfsub$DOYsnowdepth))))      # Add second axis

}

dev.off()


