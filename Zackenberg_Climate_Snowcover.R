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
library(data.table)

dfsnow <- read.csv("Data/Climate_data_Zackenberg/Snowcover/View_BioBasis_Zackenberg_Data_Abiotics_Snow_and_ice_cover131020211111435653.csv", sep="\t",stringsAsFactors = FALSE, header = TRUE)

dfsnow = dfsnow %>% rename("Date"="ï..Date")

#Add columns: DOY, MONTH, Year
dfsnow$DOY <- yday(ymd(dfsnow$Date))
dfsnow$Month <- month(ymd(dfsnow$Date))
dfsnow$Year <- year(ymd(dfsnow$Date))

dfsnow$SnowCoverFraction[dfsnow$SnowCoverFraction == -9999] <-NA
dfsnow$SnowCoverFraction <-as.numeric(dfsnow$SnowCoverFraction)

#Identify plots in dataset and remove all irrelevant plots
sort(unique(dfsnow$Plot))


dfsnow<-subset(dfsnow,Plot!="Cas1")
dfsnow<-subset(dfsnow,Plot!="Cas2")
dfsnow<-subset(dfsnow,Plot!="Cas3")
dfsnow<-subset(dfsnow,Plot!="Cas4")
dfsnow<-subset(dfsnow,Plot!="Cas5")
dfsnow<-subset(dfsnow,Plot!="Cas6")
dfsnow<-subset(dfsnow,Plot!="Dry1")
dfsnow<-subset(dfsnow,Plot!="Dry2Sal7")
dfsnow<-subset(dfsnow,Plot!="Dry3")
dfsnow<-subset(dfsnow,Plot!="Dry4")
dfsnow<-subset(dfsnow,Plot!="Dry5")
dfsnow<-subset(dfsnow,Plot!="Dry6Pap4")
dfsnow<-subset(dfsnow,Plot!="Dry7")
dfsnow<-subset(dfsnow,Plot!="Dry8")
dfsnow<-subset(dfsnow,Plot!="Eri1")
dfsnow<-subset(dfsnow,Plot!="Eri2")
dfsnow<-subset(dfsnow,Plot!="Eri3")
dfsnow<-subset(dfsnow,Plot!="Eri4")
dfsnow<-subset(dfsnow,Plot!="Pap1")
dfsnow<-subset(dfsnow,Plot!="Pap2Sal5")
dfsnow<-subset(dfsnow,Plot!="Pap3")
dfsnow<-subset(dfsnow,Plot!="Sal1")
dfsnow<-subset(dfsnow,Plot!="Sal2")
dfsnow<-subset(dfsnow,Plot!="Sal3")
dfsnow<-subset(dfsnow,Plot!="Sal4")
dfsnow<-subset(dfsnow,Plot!="Sal6")
dfsnow<-subset(dfsnow,Plot!="Sal7")
dfsnow<-subset(dfsnow,Plot!="Sax1Si1")
dfsnow<-subset(dfsnow,Plot!="Sax1Si2")
dfsnow<-subset(dfsnow,Plot!="Sax1Sil1")
dfsnow<-subset(dfsnow,Plot!="Sax2Si2")
dfsnow<-subset(dfsnow,Plot!="Sax2Sil2")
dfsnow<-subset(dfsnow,Plot!="Sax3Si3")
dfsnow<-subset(dfsnow,Plot!="Sax3Sil3")
dfsnow<-subset(dfsnow,Plot!="Si4")
dfsnow<-subset(dfsnow,Plot!="Sil4")
dfsnow<-subset(dfsnow,Plot!="Veg1")

dfsnow %>%
  select(-c(Field_remarks,General_remarks)) -> dfsnow


#Subsetting one year for simplification
dfsnow%>%
  subset(Year=='2014')->dfsnow2014

#Remove months >8 as we are only interested in date of snow melt early in the season
dfsnow2014%>%
  subset(!Month>8)->dfsnow2014

#Remove traps E to H as measurements for these traps have not been continuous and contains many NA's
dfsnow2014<-subset(dfsnow2014,Section!="E")
dfsnow2014<-subset(dfsnow2014,Section!="F")
dfsnow2014<-subset(dfsnow2014,Section!="G")
dfsnow2014<-subset(dfsnow2014,Section!="H")

#Snow melt DOY for each subplot
dfsnow2014$Snowmelt_DOY<-ifelse(dfsnow2014$SnowCoverFraction<50,1,0)


dfsnow2014$DOY<- as.numeric(dfsnow2014$DOY)
dfsnow2014$SnowCoverFraction<- as.numeric(dfsnow2014$SnowCoverFraction)
dfsnow2014$Snowmelt_DOY<-as.numeric(dfsnow2014$Snowmelt_DOY)

      
###Forsøg med at finde første DOY med <50% snedække.
first_equal_to <- function(x,value){
  (x==value) & (cumsum(x==value)==1)
}
  

dfsnow2014%>%
  group_by(Plot, Section)%>%
  mutate(first=first_equal_to(Snowmelt_DOY,1))->dftest

dftest$first[dftest$first == FALSE] <- 0




#Mean value of snow melt for each plot
dftest%>%
  group_by(Plot, first)%>%
  summarise(MeanSnowmelt=mean(DOY))->dfsnowtest
  #spread(key=Plot,value=MeanSnowCover)->dfsnowtest
dfsnowtest$MeanSnowmelt <- as.numeric(dfsnowtest$MeanSnowmelt)

dfsnowtest<-subset(dfsnowtest,first!="0")


####CALCULATION OF SNOWMELT FOR WHOLE DATASET####


#Remove months >8 as we are only interested in date of snow melt early in the season
dfsnow%>%
  subset(!Month>7)->dfsnow


#Remove subplots E - H but first we need to isolate 2019
dfsnow%>%
  subset(Year!='2019')->dfsnow_sub

#Remove traps E to H as measurements for these traps have not been continuous and contains many NA's
dfsnow_sub<-subset(dfsnow_sub,Section!="E")
dfsnow_sub<-subset(dfsnow_sub,Section!="F")
dfsnow_sub<-subset(dfsnow_sub,Section!="G")
dfsnow_sub<-subset(dfsnow_sub,Section!="H")

#Remove subplots A - D for 2019 (data never contained data og NA value for these subplots)
dfsnow%>%
  subset(Year=='2019')->dfsnow2019

#Create new dataset with all subplots
dfsnow_ref<-bind_rows(dfsnow_sub,dfsnow2019)

dfsnow_ref$SnowCoverFraction<-as.numeric(dfsnow_ref$SnowCoverFraction)
which(is.na(dfsnow_ref$SnowCoverFraction))

#We now have a dataframe with irrelevant subplots removed

#Snow melt DOY for each subplot
dfsnow_ref$Snowmelt_DOY<-ifelse(dfsnow_ref$SnowCoverFraction<=50,1,0)

###Forsøg med at finde første DOY med <50% snedække.
first_equal_to <- function(x,value){
  (x==value) & (cumsum(x==value)==1)
}


dfsnow_ref%>%
  group_by(Year, Plot, Section)%>%
  mutate(first=first_equal_to(Snowmelt_DOY,1))->dfsnowmelt

dfsnowmelt$first[dfsnowmelt$first == FALSE] <- 0

dfsnowmelt%>%
  subset(first=='1')->dfsnowmelt_ref


#Mean value of snow melt for each plot
dfsnowmelt_ref%>%
  group_by(Year,Plot, first)%>%
  summarise(MeanSnowmelt=mean(DOY))->dfsnowmeltall
#spread(key=Plot,value=MeanSnowCover)->dfsnowtest
dfsnowtest$MeanSnowmelt <- as.numeric(dfsnowtest$MeanSnowmelt)

dfsnowtest<-subset(dfsnowtest,first!="0")

ggplot(data=dfsnowmeltall, aes(x=Year, y=MeanSnowmelt, colour=Plot)) +ylab("Day of Year") +geom_line()+geom_point()


ggplot(data=dfsnowmeltall, aes(x=Year, y=MeanSnowmelt, colour=Plot)) +ylab("Day of Year") +geom_line()+geom_point()+geom_smooth(method="lm")+theme_classic()

write.csv(dfsnowmeltall, file = "Data/Climate_data_Zackenberg\\Snowmelt_Zackenberg.csv", row.names=FALSE)

####Check for linear regression####

df_summary<-data.frame(Plot=character(),Slope=numeric(),SD=numeric(),Tvalue=numeric(),Pvalue=numeric(),Rsquare=numeric(),AdjRsquare=numeric(),Count=numeric(),n=numeric())

for (i in unique(dfsnowmeltall$Plot)){
  #print(i)
  dfsnowmeltall1<-subset(dfsnowmeltall,Plot==i)
  mod1 <- lm(MeanSnowmelt ~ Year, data =dfsnowmeltall1)
  df_snowall<-data.frame(Plot=dfsnowmeltall1$Plot[1],
                      Slope=summary(mod1)$coefficients[2],
                      SD=summary(mod1)$coefficients[4],
                      Tvalue=summary(mod1)$coefficients[6],
                      Pvalue=summary(mod1)$coefficients[8],
                      Rsquare=summary(mod1)$r.squared,
                      AdjRsquare=summary(mod1)$adj.r.squared,
                      n=sum(dfsnowmeltall1$MeanSnowmelt))
  df_summary<-bind_rows(df_summary,df_snowall)
}




