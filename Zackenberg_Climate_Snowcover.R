.libPaths() #Tjek hvilken library path der bruges naar pakker installeres.
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
#install.packages("plotrix")
library(plotrix)

dfsnow <- read.csv("Data/Climate_data_Zackenberg/Snowcover/View_BioBasis_Zackenberg_Data_Abiotics_Snow_and_ice_cover131020211111435653.csv", sep="\t",stringsAsFactors = FALSE, header = TRUE)

dfsnow2019 <- read.csv("Data/Climate_data_Zackenberg/Snowcover/2019/SnowAndIceInPermanentPlots2019.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)

head(dfsnow)

dfsnow = dfsnow %>% rename("Date"="?..Date")

#Add columns: DOY, MONTH, Year
dfsnow$DOY <- yday(ymd(dfsnow$Date))
dfsnow$Month <- month(ymd(dfsnow$Date))
dfsnow$Year <- year(ymd(dfsnow$Date))

dfsnow$SnowCoverFraction[dfsnow$SnowCoverFraction == -9999] <-NA
dfsnow$SnowCoverFraction <-as.numeric(dfsnow$SnowCoverFraction)

dfsnow2019$DOY <- yday(ymd(dfsnow2019$Obsdate))
dfsnow2019$Month <- month(ymd(dfsnow2019$Obsdate))
dfsnow2019$Year <- year(ymd(dfsnow2019$Obsdate))

dfsnow2019$SnowCoverFraction[dfsnow2019$SnowCoverFraction == -9999] <-NA
dfsnow2019$SnowCoverFraction <-as.numeric(dfsnow2019$SnowCoverFraction)

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

dfsnow2019<-subset(dfsnow2019,Plot!="Cas1")
dfsnow2019<-subset(dfsnow2019,Plot!="Cas2")
dfsnow2019<-subset(dfsnow2019,Plot!="Cas3")
dfsnow2019<-subset(dfsnow2019,Plot!="Cas4")
dfsnow2019<-subset(dfsnow2019,Plot!="Cas5")
dfsnow2019<-subset(dfsnow2019,Plot!="Cas6")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry1")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry2Sal7")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry3")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry4")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry5")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry6Pap4")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry7")
dfsnow2019<-subset(dfsnow2019,Plot!="Dry8")
dfsnow2019<-subset(dfsnow2019,Plot!="Eri1")
dfsnow2019<-subset(dfsnow2019,Plot!="Eri2")
dfsnow2019<-subset(dfsnow2019,Plot!="Eri3")
dfsnow2019<-subset(dfsnow2019,Plot!="Eri4")
dfsnow2019<-subset(dfsnow2019,Plot!="Pap1")
dfsnow2019<-subset(dfsnow2019,Plot!="Pap2Sal5")
dfsnow2019<-subset(dfsnow2019,Plot!="Pap3")
dfsnow2019<-subset(dfsnow2019,Plot!="Sal1")
dfsnow2019<-subset(dfsnow2019,Plot!="Sal2")
dfsnow2019<-subset(dfsnow2019,Plot!="Sal3")
dfsnow2019<-subset(dfsnow2019,Plot!="Sal4")
dfsnow2019<-subset(dfsnow2019,Plot!="Sal6")
dfsnow2019<-subset(dfsnow2019,Plot!="Sal7")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax1Si1")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax1Si2")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax1Sil1")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax2Si2")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax2Sil2")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax3Si3")
dfsnow2019<-subset(dfsnow2019,Plot!="Sax3Sil3")
dfsnow2019<-subset(dfsnow2019,Plot!="Si4")
dfsnow2019<-subset(dfsnow2019,Plot!="Sil4")
dfsnow2019<-subset(dfsnow2019,Plot!="Veg1")

dfsnow %>%
  select(-c(Field_remarks,General_remarks)) -> dfsnow

dfsnow2019 %>%
  select(-c(Observer, Field_remarks,General_remarks)) -> dfsnow2019
dfsnow2019 = dfsnow2019 %>% rename("Date"="Obsdate")


dfsnow2019$Date <- format(as.Date(dfsnow2019$Date, format = "%Y/%m/%d"), "%Y-%m-%d")

dfsnow_all <- bind_rows(dfsnow,dfsnow2019)

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
dfsnow_all%>%
  subset(!Month>7)->dfsnow_all


#Remove subplots E - H 
#Remove traps E to H as measurements for these traps have not been continuous and contains many NA's
dfsnow_all<-subset(dfsnow_all,Section!="E")
dfsnow_all<-subset(dfsnow_all,Section!="F")
dfsnow_all<-subset(dfsnow_all,Section!="G")
dfsnow_all<-subset(dfsnow_all,Section!="H")

dfsnow_all$SnowCoverFraction<-as.numeric(dfsnow_all$SnowCoverFraction)
which(is.na(dfsnow_all$SnowCoverFraction))
print(dfsnow_all$SnowCoverFraction)
scf <- dfsnow_all$SnowCoverFraction


df_snowmelt_transitions<-data.frame(Year=numeric(), Plot=character(), Section=character(), SnowCoverFraction=numeric(), DOY=numeric())
df_snowmelt_interpolations<-data.frame(Year=numeric(), Plot=character(), Section=character(), SnowCoverFraction=numeric(), DOY=numeric())
df_snowmelt_transitions_only<-data.frame(Year=numeric(), Plot=character(), Section=character(), SnowCoverFraction=numeric(), DOY=numeric())
df_snowmelt_exact_50_only<-data.frame(Year=numeric(), Plot=character(), Section=character(), SnowCoverFraction=numeric(), DOY=numeric())

for (year in unique(dfsnow_all$Year)) {
  print(year)
  dfsub_year<-subset(dfsnow_all,Year==year)
  
  for (plot in unique(dfsub_year$Plot)) {
    print(plot)
    dfsub_plot<-subset(dfsub_year,Plot==plot)
    
    for (section in unique(dfsub_plot$Section)) {
      print(section)
      dfsub_section<-subset(dfsub_plot,Section==section)
      
      excact50ScfDOY <- NA
      exact50Scf <- NA
      
      previousScf <- NA
      previousScfDOY <- NA
      
      transitionStartDOY <- NA
      transitionStartSCF <- NA
      transitionEndDOY <- NA
      transitionEndSCF <- NA
      
      for (doy in unique(dfsub_section$DOY)) {
        if(is.na(excact50ScfDOY) & is.na(transitionStartDOY) & is.na(transitionEndDOY)){
          print(doy)
          dfsub_doy<-subset(dfsub_section,DOY==doy)
          print(dfsub_doy)
          
          scf <- dfsub_doy$SnowCoverFraction[1]
          print(scf)
          result <- is.na(scf)
          print(result)
          
          if(!is.na(scf)){
            if (scf == 50){
              excact50ScfDOY = doy
              exact50Scf = scf
            }
            else if (is.na(previousScf)){
              previousScf <- scf
              previousScfDOY <- doy
            }
            else {
              if (scf < 50){
                if (previousScf > 50){
                  transitionStartDOY <- previousScfDOY
                  transitionStartSCF <- previousScf
                  transitionEndDOY <- doy
                  transitionEndSCF <- scf
                }
              }
              
              previousScf <- scf
              previousScfDOY <- doy
            }
          }
        }
      }
      
      allBelow50ScfDOY <- NA
      allBelow50Scf <- NA
      
      if(is.na(excact50ScfDOY) & is.na(transitionStartDOY) & is.na(transitionEndDOY)){
        for (doy in unique(dfsub_section$DOY)) {
          if(is.na(allBelow50ScfDOY)){
            dfsub_doy<-subset(dfsub_section,DOY==doy)
            scf <- dfsub_doy$SnowCoverFraction[1]
            
            if(!is.na(scf)){
              if(scf < 50){
                allBelow50ScfDOY <- doy
                allBelow50Scf <- scf
              }
            }
            
          }
        }
      }
      
      if(!is.na(excact50ScfDOY)){
       # print(dfsub_section)
       # print(paste("excact50ScfDOY",excact50ScfDOY))
        
        df_snowmelt_exact_50<-data.frame(Year=year, Plot=plot, Section=section, SnowCoverFraction=exact50Scf, DOY=excact50ScfDOY)
        df_snowmelt_transitions<-bind_rows(df_snowmelt_transitions, df_snowmelt_exact_50)
        df_snowmelt_interpolations<-bind_rows(df_snowmelt_interpolations, df_snowmelt_exact_50)
        df_snowmelt_exact_50_only<-bind_rows(df_snowmelt_exact_50_only, df_snowmelt_exact_50)
      }
      else if(!is.na(transitionStartDOY) | !is.na(transitionEndDOY)){
        #print(dfsub_section)
        #print(paste("transitionStartDOY",transitionStartDOY))
        #print(paste("transitionEndDOY",transitionEndDOY))
        
        df_snowmelt_transition_previous<-data.frame(Year=year, Plot=plot, Section=section, SnowCoverFraction=transitionStartSCF, DOY=transitionStartDOY)
        df_snowmelt_transitions<-bind_rows(df_snowmelt_transitions, df_snowmelt_transition_previous)
        df_snowmelt_transitions_only<-bind_rows(df_snowmelt_transitions_only, df_snowmelt_transition_previous)
        
        df_snowmelt_transition_current<-data.frame(Year=year, Plot=plot, Section=section, SnowCoverFraction=transitionEndSCF, DOY=transitionEndDOY)
        df_snowmelt_transitions<-bind_rows(df_snowmelt_transitions, df_snowmelt_transition_current)
        df_snowmelt_transitions_only<-bind_rows(df_snowmelt_transitions_only, df_snowmelt_transition_current)
        
        df_snowmelt_interpolation<-data.frame(Year=year, Plot=plot, Section=section, 
                                              SnowCoverFraction=mean(c(transitionStartSCF, transitionEndSCF)), DOY=mean(c(transitionStartDOY, transitionEndDOY)))
        df_snowmelt_interpolations<-bind_rows(df_snowmelt_interpolations, df_snowmelt_interpolation)
      }
      else if (!is.na(allBelow50ScfDOY)){
        #print(dfsub_section)
        #print(paste("allBelow50ScfDOY",allBelow50ScfDOY))
        
        df_snowmelt_all_below_50<-data.frame(Year=year, Plot=plot, Section=section, SnowCoverFraction=allBelow50Scf, DOY=allBelow50ScfDOY)
        df_snowmelt_transitions<-bind_rows(df_snowmelt_transitions, df_snowmelt_all_below_50)
        df_snowmelt_interpolations<-bind_rows(df_snowmelt_interpolations, df_snowmelt_all_below_50)
      }
      else {
        #print(paste("All SCF are above 50 or NA. Year:", year, "Plot:", plot, "Section:", section))
        #(dfsub_section)
      }
    }
  }
}

df_snowmelt_pred_DOY<-data.frame(Year=numeric(), Plot=character(), Section=character(), SnowCoverFraction=numeric(), DOY=numeric())

for (year in unique(df_snowmelt_transitions_only$Year)) {
  #print(year)
  dfsub_year<-subset(df_snowmelt_transitions_only,Year==year)
  
  for (plot in unique(dfsub_year$Plot)) {
    #print(plot)
    dfsub_plot<-subset(dfsub_year,Plot==plot)
    
    for (section in unique(dfsub_plot$Section)) {
      #print(section)
      dfsub_section<-subset(dfsub_plot,Section==section)
      
      #print(dfsub_section$DOY)
      
      interpolation <- data.frame(approx(dfsub_section$SnowCoverFraction, dfsub_section$DOY, xout = seq(from = 0, to = 100, by = 10)))
      
      interpolation_row_with_50_scf <- subset(interpolation, x == 50)
      
      df_snowmelt_pred_DOY_single_row<-data.frame(Year=year, Plot=plot, Section=section, SnowCoverFraction=interpolation_row_with_50_scf$x, DOY=round(interpolation_row_with_50_scf$y, digits=0))
      df_snowmelt_pred_DOY<-bind_rows(df_snowmelt_pred_DOY, df_snowmelt_pred_DOY_single_row)
    }
  }
}


df_snowmelt_exact_50_and_interpolated <- bind_rows(df_snowmelt_pred_DOY,df_snowmelt_exact_50_only)

ggplot(data=df_snowmelt_exact_50_and_interpolated, aes(x=Year, y=DOY, colour=Plot)) +ylab("Day of Year")+geom_point()+geom_line()

df_snowmelt_exact_50_and_interpolated %>%
  mutate(Habitat = case_when(
    Plot == "Art1" ~ "Pond",
    Plot == "Art2" ~ "Wet fen",
    Plot == "Art3" ~ "Mesic heath",
    Plot == "Art4" ~ "Mesic heath",
    Plot == "Art5" ~ "Arid heath",
    Plot == "Art6" ~ "Snow bed",
    Plot == "Art7" ~ "Arid heath")) -> df1

#Mean value of snow melt for each plot
df1%>%
  group_by(Year,Habitat)%>%
  summarise(MeanSnowmelt = mean(MeanSnowmelt),
            SD = sd(MeanSnowmelt))->dfsnowmeltall
#spread(key=Plot,value=MeanSnowCover)->dfsnowtest
dfsnowmeltall$MeanSnowmelt <- as.numeric(dfsnowmeltall$MeanSnowmelt)

#df_snowmelt_exact_50_and_interpolated%>%
  #group_by(Year,Plot)%>%
  #summarise(SE_Snowmelt=std.error(DOY,na.rm = TRUE))->dfsnowmeltSE

dfsnowmeltall$SE <- dfsnowmeltSE$SE_Snowmelt[match(paste0(dfsnowmeltall$Year,dfsnowmeltall$Plot),
                                                    paste0(dfsnowmeltSE$Year,dfsnowmeltSE$Plot))]

#dfsnowmeltall$na = factor(is.na(dfsnowmeltall$MeanSnowmelt))


write.csv(dfsnowmeltall, file = "Data/Climate_data_Zackenberg\\Snowmelt_Zackenberg.csv", row.names=FALSE)

df1 <- read.csv2("Data/Climate_data_Zackenberg\\Snowmelt_Zackenberg.csv",sep=",",stringsAsFactors = FALSE)

class(df1$MeanSnowmelt)
df1$MeanSnowmelt <- as.numeric(df1$MeanSnowmelt)

ggplot(df1, aes(x=Year, y=MeanSnowmelt, color = Plot))+
  geom_line(aes(x=Year, y=MeanSnowmelt))

df1 %>%
  mutate(Habitat = case_when(
    Plot == "Art1" ~ "Pond",
    Plot == "Art2" ~ "Wet fen",
    Plot == "Art3" ~ "Mesic heath",
    Plot == "Art4" ~ "Mesic heath",
    Plot == "Art5" ~ "Arid heath",
    Plot == "Art6" ~ "Snow bed",
    Plot == "Art7" ~ "Arid heath")) -> df1

df1 %>%
  group_by(Year, Habitat)%>%
  summarise(AverageSnowmelt = mean(MeanSnowmelt)) -> df2


df3 = expand.grid(Year=unique(df1$Year),Habitat=unique(df1$Habitat)) %>% left_join(df2)

ggplot(data=df3, aes(x=Year, y=AverageSnowmelt, colour=Habitat)) +
  ylab("Day of Year")+
  geom_hline(yintercept = c(140,160,180,200), linetype="dashed", color = "gray")+
  geom_line(size = 1)+geom_point(size = 2)+
  # geom_errorbar(aes(ymin=MeanSnowmelt-SD, ymax=MeanSnowmelt+SD), width=.2, size = 1)+
  theme(panel.background = element_rect(fill = "white"), 
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"),
        axis.text.y = element_text(face = "bold", size = 15, color = "black"),
        axis.title.x = element_text(face = "bold", size = 15, color = "black"),
        axis.title.y = element_text(face = "bold", size = 15, vjust = 2, color = "black"))

df1 %>%
  group_by(Year, Habitat) %>%
  summarise(Snowmelt_habitat = mean(MeanSnowmelt)) -> df1a

dfsnowmeltall %>%
  group_by(Year) %>%
  summarise(Snowmelt_habitat = mean(MeanSnowmelt)) -> df1b

#Create a sequence of year observations from 1996 - 2019

df2 = expand.grid(Year=unique(df1a$Year),Habitat=unique(df1a$Habitat)) %>% left_join(df1a)


ggplot()+
  geom_point(data = df2, aes(x=Year, y=Snowmelt_habitat, colour = Habitat))+
  geom_line(data = df2, aes(x=Year, y=Snowmelt_habitat, colour = df2$Habitat))


df1a$Habitat <- as.factor(df1a$Habitat)
contrasts(df1a$Habitat) <- contr.treatment(5, base = 2)

multiple_lm <- lm(Snowmelt_habitat ~ Year + Habitat + Year:Habitat, df1a)
summary(multiple_lm)
AIC(multiple_lm)
anova(multiple_lm)

multiple_lm1 <- lm(Snowmelt_habitat ~ Year + Habitat, df1a)
summary(multiple_lm1)
AIC(multiple_lm1)
anova(multiple_lm1)

#write.csv(dfsnowmeltall, file = "Data/Climate_data_Zackenberg\\Snowmelt_Zackenberg.csv", row.names=FALSE)

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


