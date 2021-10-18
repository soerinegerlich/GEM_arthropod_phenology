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

#Date of first snow melt (<50% snow cover)
df_snowmelt_all<-data.frame(Plot=character(),Section=character(),SnowCoverFraction=numeric(),DOY=numeric())
                       

for (k in unique(dfsnow2014$Plot)){
  #print(dfsnow2014$Plot)
  dfsub<-subset(dfsnow2014,Plot==k)
  for (i in unique(dfsub$Section)){
    print(i)
    #for(j in unique(dfsub$Year)){
      dfsuba<-subset(dfsub,Section==i)
      dfsuba<-subset(dfsuba,Year==j)
      if(dfsuba%>%group_by(Plot, Section)%>%summarise(DOY_snowmelt=SnowCoverFraction<=50)){
        df_SnowMeltSection<-data.frame(Plot=dfsuba$Plot[1], #sum(!is.na()) er antallet af ikke-Na værdier
                            Section=dfsuba$Section[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                            SnowCoverFraction=dfsuba$SnowCoverFraction[1],
                            DOY=dfsuba$DOY[1])
                    
      }
      else{
        df_SnowMeltSection1<-data.frame(Plot=dfsuba$Plot[1], #sum(!is.na()) er antallet af ikke-Na værdier
                                       Section=dfsuba$Section[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                                       SnowCoverFraction=dfsuba$SnowCoverFraction[1],
                                       DOY=dfsuba$DOY[1])
        df_snowmelt_all<-bind_rows(df_snowmelt_all,df_SnowMeltSection)
      }
    }
  }
#}
      
###Forsøg med eksempel fra StackOverflow
first_equal_to <- function(x,value){
  (x==value) & (cumsum(x==value)==1)
}
  

dfsnow2014%>%
  group_by(Plot, Section)%>%
  mutate(first=first_equal_to(Snowmelt_DOY,1))->dftest

dftest$first[dftest$first == FALSE] <- 0



#Mean value of snow melt for each plot
dfsnow2014%>%
  group_by(DOY,Plot)%>%
  summarise(MeanSnowCover=mean(unique(Section)))%>%
  spread(key=Plot,value=MeanSnowCover)->dfsnowtest



               

