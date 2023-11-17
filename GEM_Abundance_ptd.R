#install.packages(c("tidyverse", "tidyr", "RDocumentation", "readxl", "lubridate", "mgcv", "MESS", "corrplot", "writexl"))
library(tidyverse)
library(tidyr)
library(RDocumentation)#Denne gør din help() funktion bedre
library(readxl) 
library(lubridate) #Beregner datoer og tidspunkter. Det er en toolbox, eller en række tools der hjælper med at fremstille tid/datoer bedre. 
library(mgcv) #funktioner der kan analysere med GAM og generalised additive mixed modeling.
library(MESS) #teste antagelser i GAM, statistiske detaljer.
library(corrplot)
library(writexl)
library(dplyr)


##### Zackenberg data - downloaded 14 September 2022 #####


df1 <- read.csv2("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence140920221552287525.csv",sep="\t",stringsAsFactors = FALSE)


#Rename columns.
#No space between Days and letter
df1 = df1 %>% rename("DaysA"="Days.A", "DaysB"="Days.B", "DaysC"="Days.C", "DaysD"="Days.D", "DaysE"="Days.E", "DaysF"="Days.F", "DaysG"="Days.G", "DaysH"="Days.H")
#df1 = df1 %>% rename("Plot"="Plot.ID", "Date"="ï..Date") #Fejl vises ved Date. 
df1 = df1 %>% rename("Plot"="Plot.ID")

#Replace all Days rows which includes -9999 with NA. Just means that there is no snow
df1$DaysA[df1$DaysA == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysB[df1$DaysB == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysC[df1$DaysC == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysD[df1$DaysD == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysE[df1$DaysE == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysF[df1$DaysF == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysG[df1$DaysG == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysH[df1$DaysH == -9999] <- NA #Replace '-9999' with 'NA'

#Same with abundance columns but we replace -9999 and -999 with 0 instead in order to use it for calculations later
df1$A[df1$A == -9999] <- 0 #Replace '-9999' with zero
df1$B[df1$B == -9999] <- 0 #Replace '-9999' with zero
df1$C[df1$C == -9999] <- 0 #Replace '-9999' with zero
df1$D[df1$D == -9999] <- 0 #Replace '-9999' with zero
df1$E[df1$E == -9999] <- 0 #Replace '-9999' with zero
df1$F[df1$F == -9999] <- 0 #Replace '-9999' with zero
df1$G[df1$G == -9999] <- 0 #Replace '-9999' with zero
df1$H[df1$H == -9999] <- 0 #Replace '-9999' with zero

df1$A[df1$A == -999] <- 0 #Replace '-9999' with zero
df1$B[df1$B == -999] <- 0 #Replace '-9999' with zero
df1$C[df1$C == -999] <- 0 #Replace '-9999' with zero
df1$D[df1$D == -999] <- 0 #Replace '-9999' with zero
df1$E[df1$E == -999] <- 0 #Replace '-9999' with zero
df1$F[df1$F == -999] <- 0 #Replace '-9999' with zero
df1$G[df1$G == -999] <- 0 #Replace '-9999' with zero
df1$H[df1$H == -999] <- 0 #Replace '-9999' with zero

#Some places is written NA but this is added by observers and needs to be converted to 0 for later calculations
df1[df1=="NA"] <- NA
df1$A[is.na(df1$A)] <- 0 #Replace 'na' with zero
df1$B[is.na(df1$B)] <- 0 #Replace 'na' with zero
df1$C[is.na(df1$C)] <- 0 #Replace 'na' with zero
df1$D[is.na(df1$D)] <- 0 #Replace 'na' with zero
df1$E[is.na(df1$E)] <- 0 #Replace 'na' with zero
df1$F[is.na(df1$F)] <- 0 #Replace 'na' with zero
df1$G[is.na(df1$G)] <- 0 #Replace 'na' with zero
df1$H[is.na(df1$H)] <- 0 #Replace 'na' with zero

df1$DaysA[is.na(df1$DaysA)] <- 0 #Replace 'na' with zero
df1$DaysB[is.na(df1$DaysB)] <- 0 #Replace 'na' with zero
df1$DaysC[is.na(df1$DaysC)] <- 0 #Replace 'na' with zero
df1$DaysD[is.na(df1$DaysD)] <- 0 #Replace 'na' with zero
df1$DaysE[is.na(df1$DaysE)] <- 0 #Replace 'na' with zero
df1$DaysF[is.na(df1$DaysF)] <- 0 #Replace 'na' with zero
df1$DaysG[is.na(df1$DaysG)] <- 0 #Replace 'na' with zero
df1$DaysH[is.na(df1$DaysH)] <- 0 #Replace 'na' with zero

#New columns with DOY, week number and year
df1$DOY<-yday(ymd(df1$Date)) #Add Day of year variable
df1$DOY7<-floor(df1$DOY/7)*7 #Add week column. Dividing DOY med 7, removing decimals(floor) and uses number as week. But not correct and we will not be using it...
df1$Month<-month(df1$Date) # Add month variable as factor.
df1$Year<-year(df1$Date) # Add year variable as a factor.

#Change Days E,F,G,H to zero after 2006 trapdays are recorded, but samples not processed
#From 2006 traps are only sorted from A - D 
df1 <- within(df1, DaysE[Year>2006] <- 0)
df1 <- within(df1, DaysF[Year>2006] <- 0)
df1 <- within(df1, DaysG[Year>2006] <- 0)
df1 <- within(df1, DaysH[Year>2006] <- 0)


#Change to numeric
df1$A <- as.numeric(df1$A)
df1$B <- as.numeric(df1$B)
df1$C <- as.numeric(df1$C)
df1$D <- as.numeric(df1$D)
df1$E <- as.numeric(df1$E)
df1$F <- as.numeric(df1$F)
df1$G <- as.numeric(df1$G)
df1$H <- as.numeric(df1$H)

#Combine taxonomic information into one column
df1$SpeciesID<-as.character(df1$Family)
df1$SpeciesID[is.na(df1$SpeciesID)]<-as.character(df1$Order[is.na(df1$SpeciesID)])
df1$SpeciesID[is.na(df1$SpeciesID)]<-as.character(df1$Phylum[is.na(df1$SpeciesID)])

#Adjust taxonomic names.
sort(unique(df1$SpeciesID))
df1$SpeciesID[df1$SpeciesID == "PieridaeÂ "] <- "Pieridae" 
df1$SpeciesID[df1$SpeciesID == "TriopsidaeÂ "] <- "Triopsidae"
#For some reason the names will not change!!!

#gsub("TriopsidaeÂ","Triopsidae",df1$SpeciesID)
#gsub("PieridaeÂ ","Pieridae",df1$SpeciesID)

#str_replace_all(df1$SpeciesID,"Â","")

#Remove data which cannot be identified to family level
df1<- subset(df1,SpeciesID!="unidentified")
df1<- subset(df1,SpeciesID!="Brachycera larvae")
df1<- subset(df1,SpeciesID!="Cyclorrhapha larvae")
df1<- subset(df1,SpeciesID!="Lepidoptera larvae")
df1<- subset(df1,SpeciesID!="Diptera larvae")
df1<- subset(df1,SpeciesID!="Nematocera larvae")
df1<- subset(df1,SpeciesID!="Symphyta larvae")
df1<- subset(df1,SpeciesID!="Tipulidae larvae")
df1<- subset(df1,SpeciesID!="Hymenoptera larvae")


#print(unique(df1$SpeciesID))

#Summarize to one value per SpeciesID,Year,Plot,DOY
df1%>%
  group_by(SpeciesID,Year,Plot,Month,DOY)%>%
  summarise(DaysA=mean(DaysA),DaysB=mean(DaysB),DaysC=mean(DaysC),DaysD=mean(DaysD),
            DaysE=mean(DaysE),DaysF=mean(DaysF),DaysG=mean(DaysG),DaysH=mean(DaysH),
            A=sum(A),B=sum(B),C=sum(C),D=sum(D),E=sum(E),F=sum(F),G=sum(G),H=sum(H))->df2


#Sum of abundance in all traps. Abundance not calculated till later on though
#df2$Abundance<-df2$A+df2$B+df2$C+df2$D+df2$E+df2$F+df2$G+df2$H
df2$Trapdays<-df2$DaysA+df2$DaysB+df2$DaysC+df2$DaysD+df2$DaysE+df2$DaysF+df2$DaysG+df2$DaysH
#df2$Abundance<-df2$A+df2$B+df2$C+df2$D+df2$E+df2$F+df2$G+df2$H

#Abundance test plots - Family level data. 

#Just to make sure all NAs are 0.
df2$A[is.na(df2$A)] <- 0
df2$B[is.na(df2$B)] <- 0
df2$C[is.na(df2$C)] <- 0
df2$D[is.na(df2$D)] <- 0
df2$E[is.na(df2$E)] <- 0
df2$F[is.na(df2$F)] <- 0
df2$G[is.na(df2$G)] <- 0
df2$H[is.na(df2$H)] <- 0

#ABUNDANCE CALCULATION

#Some of the families are combined as they were mixed together in the early years
df2%>%
  group_by(SpeciesID,Year,Plot,Month,DOY,Trapdays)%>%
  summarise(Abundance=A+B+C+D+E+F+G+H)%>%
  within(Abundance[is.na(Abundance)] <- 0)%>%
  spread(key=SpeciesID,value=Abundance)%>%
  #pivot_wider(names_from = c(SpeciesID), values_from = c(Abundance))%>%
  within(Muscidae[is.na(Muscidae)]<- 0)%>%
  within(Anthomyiidae[is.na(Anthomyiidae)]<- 0)%>%
  within(Anthomyzidae[is.na(Anthomyzidae)]<- 0)%>%
  within(Chironomidae[is.na(Chironomidae)]<- 0)%>%
  within(Ceratopogonidae[is.na(Ceratopogonidae)]<- 0)%>%
  within(Mycetophilidae[is.na(Mycetophilidae)]<- 0)%>%
  within(Sciaridae[is.na(Sciaridae)]<- 0)%>%
  mutate(ANMU=Muscidae+Anthomyiidae+Anthomyzidae,
         CHCE=Chironomidae+Ceratopogonidae,
         MYSC=Mycetophilidae+Sciaridae)%>%
  select(-c(Muscidae,Anthomyiidae,Anthomyzidae,Chironomidae,Ceratopogonidae,Mycetophilidae,Sciaridae))%>%
  gather(key=SpeciesID,value=Abundance,6:71) -> df3
#pivot_longer(names_to = c(SpeciesID), values_from = c(Abundance), 6:71) -> df3a


#Data from 2010 is available and we need to extract this data in order to correct the data according to mites and spiders
df_2010<-subset(df3,Year=="2010")
#write_xlsx(df_2010, "Data/Pre_data\\df_2010.xlsx", col_names = TRUE)

df3 <- read_excel("Data/Pre_data/df3.xlsx")

#Match corrected df3 with uncorrected df_2010 before manually correcting for mites and spider abundances
df3 <- rbind(df3,df_2010)

#Need to reorder rows
df3 %>%
  arrange(Year) -> df3_new

#Now create a new df3
#write_xlsx(df3_new, "Data/Pre_data\\df3_new.xlsx", col_names = TRUE)

#The manual corrections will be performed using the "Acari_lyco_correction" script

####GET df3 FROM DRIVE AS CORRECTIONS IN MITE AND LYCOSIDAE ARE INCLUDED####
df3 <- read_excel("Data/Pre_data/df3_new.xlsx")

df3$Abundance<-as.numeric(df3$Abundance)

#df3$Abundance_corrected <- ifelse(df3$Year<2006,df3$Abundance/2,df3$Abundance)

#Tests to check if calculation is correct

dfCollem<-subset(df2,SpeciesID=="Collembola")
dfCollem<-subset(dfCollem,Plot=="Art5")
dfCollem<-subset(dfCollem,Year=="2002")

dfAcari2<-subset(df3,SpeciesID=="Acari")
dfAcari2<-subset(dfAcari2,Plot=="Art6")
dfAcari2<-subset(dfAcari2,Year=="1999")

dfCollembola2<-subset(df3,SpeciesID=="Collembola")
dfCollembola2<-subset(dfCollembola2,Plot=="Art5")
dfCollembola2<-subset(dfCollembola2,Year=="2002")

dfAgromyzidae2<-subset(df3,SpeciesID=="Agromyzidae")
dfAgromyzidae2<-subset(dfAgromyzidae2,Plot=="Art1")
dfAgromyzidae2<-subset(dfAgromyzidae2,Year=="2005")

dfCHCE2<-subset(df3,SpeciesID=="CHCE")
dfCHCE2<-subset(dfCHCE2,Plot=="Art1")
dfCHCE2<-subset(dfCHCE2,Year=="1997")

#After calculating abundance some NA values may appear if the observations in the calculations only contained 0
#Therefore we make sure to convert all NA values to 0 again
#df3$Abundance_corrected[is.na(df3$Abundance_corrected)] <- 0

df3$Abundance[is.na(df3$Abundance)] <- 0

####CRITERIA IMPLEMENTED####

#Column with Event created. If abundance is more than 0 it will be indicated with 1, otherwise 0.
#df3$Event<-ifelse(df3$Abundance_corrected>0,1,0)
df3$Event<-ifelse(df3$Abundance>0,1,0)

df3$Month<- as.numeric(df3$Month)
class(df3$Abundance)

df3%>%
  subset(Month>5&Month<9)%>% 
  group_by(SpeciesID,Plot,Year)%>%
  summarise(TotalAbundance=sum(Abundance),TotalEvents=sum(Event))->df2a

####Extra - Check total abundance for each family in every plot###

df3%>%
  subset(Month>5&Month<9)%>% 
  group_by(SpeciesID,Plot)%>%
  summarise(TotalAbundance=sum(Abundance))%>%
  spread(key=Plot,value=TotalAbundance)->df2b



df2b%>%
  group_by(SpeciesID)%>%
  summarise(Sum = Art1+Art2+Art3+Art4+Art5+Art6+Art7) -> df2d

df2b$Sum <- (df2d$Sum[match(paste0(df2b$SpeciesID),paste0(df2d$SpeciesID))])#paste betyder at det er kombinationen af variable der skal matche.
sum(df2b$Sum)


#write_xlsx(df2b, "Data\\Summary_arthropods_Zackenberg\\Total_sum_Arthropods.xlsx", col_names = TRUE)

#Sorting data where data is only available for <5 years
#Vi vil gerne sortere taxa fra, hvor der kun er data for <5 år
#Abundans kriterie skal være på 50 individer
#df2a$TotalAbunAndEventCriteria<-ifelse(df2a$TotalAbundance>25&df2a$TotalEvents>2,1,0)
df2a$TotalAbunAndEventCriteria<-ifelse(df2a$TotalAbundance>50&df2a$TotalEvents>2,1,0)

#df2a%>%
#group_by(SpeciesID,Year)%>%
#summarise(TotalYear=sum(TotalAbunAndEventCriteria))->df3a


#Calculation number of years where a family and plot has fulfilled criteria in abundance and events
#Vi beregner antal år, hvor et plot har opfyldt kriterier for abundans og events
df2a%>%
  group_by(SpeciesID,Plot)%>%
  summarise(TotalYear=sum(TotalAbunAndEventCriteria))->df2c

#Overføre TotalYear kolonne til df2a
#df2a$TotalYear <- (df3a$TotalYear[match(paste0(df2a$SpeciesID,df2a$Year),paste0(df3a$SpeciesID,df3a$Year))])
#df3$TotalAbunAndEventCriteria <- (df2a$TotalAbunAndEventCriteria[match(paste0(df3$SpeciesID,df3$Year),paste0(df2a$SpeciesID,df2a$Year))])
df3$TotalYear <- (df2c$TotalYear[match(paste0(df3$SpeciesID,df3$Plot),paste0(df2c$SpeciesID,df2c$Plot))])



#df3$YearThres<-ifelse(df3$TotalYear>0,1,0)
#df3$Include2<-0


#Calculation of Include column where all 3 criteria are fulfilled or not####
df2a$TotalEvents<- as.numeric(df2a$TotalEvents)
df2a$TotalAbundance<- as.numeric(df2a$TotalAbundance)

df2a$Include<-ifelse(df2a$TotalAbundance>50&df2a$TotalEvents>2,1,0)#Need at least 50 individuals in a season and 3 capture events

#Filter original data for sampling criterias
df3$Include <- (df2a$Include[match(paste0(df3$SpeciesID,df3$Year,df3$Plot),paste0(df2a$SpeciesID,df2a$Year,df2a$Plot))])#paste betyder at det er kombinationen af variable der skal matche.

#Include abundancePTD
df3$AbundancePTD <- (df3$Abundance/df3$Trapdays)*10

#Abundance
df3%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6")%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(Abundance_corrected = sum(Abundance_corrected),
            Trapdays=sum(Trapdays),
            AbundancePTD=Abundance/Trapdays)%>%
  ggplot(aes(Year,Abundance_corrected)) + ylab("Abundance") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df3%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6")%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(Abundance = sum(Abundance),
            Trapdays=sum(Trapdays),
            AbundancePTD=Abundance/Trapdays)%>%
  ggplot(aes(Year,Abundance)) + ylab("Abundance") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))


#AbundancePTD
df3%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6")%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(Abundance = sum(Abundance),
            Trapdays=sum(Trapdays),
            AbundancePTD=Abundance/Trapdays)%>%
  ggplot(aes(Year,AbundancePTD)) + ylab("Abundance per trap day") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#AbundancePTD plots as colours
df3%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6")%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(Abundance = sum(Abundance),
            Trapdays=sum(Trapdays),
            AbundancePTD=Abundance/Trapdays)%>%
  ggplot(aes(Year,AbundancePTD,colour=Plot)) + ylab("Abundance per trap day") + 
  #geom_smooth(method="lm")+
  geom_line()+facet_wrap(~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Abundance plots as colours
df3%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6")%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(Abundance = sum(Abundance),
            Trapdays=sum(Trapdays),
            AbundancePTD=Abundance/Trapdays)%>%
  ggplot(aes(Year,Abundance,colour=Plot)) + ylab("Abundance") + 
  #geom_smooth(method="lm")+
  geom_line()+facet_wrap(~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#df3$Include<-ifelse(df3$TotalAbundanceYear>24&df3$TotalEventsYear>2,1,0)

#SpeciesID before plot
df4 <-df3 %>% 
  select(SpeciesID, everything())



####NEW DATA USED FOR GAM####
df6 <- data.frame(df4)
df5 <- select(df6, SpeciesID,Plot,Year,DOY,Abundance,AbundancePTD,Include,Event,TotalYear)
sum(is.na(df5$Include))#Tjek om der er nogle NA værdier
#df5$AbundancePTD <- (df5$Abundance/df5$Trapdays)
#df5$Include <- (df2a$Include[match(paste0(df4$SpeciesID,df4$Plot),paste0(df2a$SpeciesID,df2a$Plot))])
#summarise(Abundance = sum(Abundance),Trapdays=sum(Trapdays),AbundancePTD=Abundance/Trapdays)->df4

class(df5$Include)

df5$AbundancePTD[is.na(df5$AbundancePTD)] <- 0

write.csv(df5, file = "Data/Dataset_for_GAM\\EMdata_final_AbundancePTD.csv", row.names=FALSE)
#row.names=FALSE fjerne første kolonne, da vi ellers får et problem med en kolonne X.


############################################################
#New data frame saved including the following columns:
#df4 = SpeciesID, Plot, Year, DOY, Abundance, Event, Include
############################################################

#df7<- read.csv("ZAC_all.csv")
df7<-read.csv2("Data/Dataset_for_GAM/EMdata_final_AbundancePTD.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)

#df8<-subset(df7,select=-c(X))


df7$Year<- as.factor(df7$Year)#Easier to work with year as factor
#df7$SpeciesID<- as.factor(df7$SpeciesID)
#df7$Plot<- as.factor(df7$Plot)
df7$Abundance<- as.numeric(df7$Abundance)
class(df7$AbundancePTD)
#typeof(df7$Abundance)
df7$AbundancePTD<-as.numeric(df7$AbundancePTD)


df7$TotalYear<-as.numeric(df7$TotalYear)

#class(df7$Trapdays)
#df7$Trapdays<-as.integer(df7$Trapdays)
#class(df7$Trapdays)

#class(df7$AbundancePTD)
#df7$AbundancePTD<-as.numeric(df7$AbundancePTD)
#class(df7$AbundancePTD)

#Now the data is tested

df7%>%
  group_by(SpeciesID, Plot, Year)%>%
  summarise(Include= max(Include))->df7temp
df7temp%>%
  group_by(SpeciesID, Plot)%>%
  summarise(Total_GAM= sum(Include))->df7GAM
df7include <- subset(df7GAM, Total_GAM >= 5)
#>= 10 eller 5 eller? Der er ingen Total GAM på 10..

#########unique(df7include$SpeciesID)

#THIS IS MAINLY FOR VISUAL PURPOSES#
#Creates PDF files with phen curves
##Laver en fil i en folder med figures for alle 69 species.
##Der benyttes loop 'for' funktion, hvor der også indgår nested loops.
df7$Abundance<- as.numeric(df7$Abundance)
class(df7$AbundancePTD)
class(df7$Abundance)
df7$DOY<- as.numeric(df7$DOY)
df7$AbundancePTD<- as.numeric(df7$AbundancePTD)


for (k in unique(df7$SpeciesID)){
  dfsub<-subset(df7,SpeciesID==k)
  pdf(paste("Figures_PTD",k,".pdf"),width=20,height=12)
  par(mfrow=c(7,25),oma = c(5,5,4,0) ,mar = c(2,1,2,2) + 0.1) #it goes c(bottom, left, top, right) 
  for (i in unique(dfsub$Plot)){
    for(j in unique(dfsub$Year)){
      dfsuba<-subset(dfsub,Plot==i)
      dfsuba<-subset(dfsuba,Year==j)
      threshold <- length(dfsuba[dfsuba$AbundancePTD >= 1, 1]) # beregne antal events
      sumabund<-sum(dfsuba$Abundance)
      if(threshold <= 2||sumabund<50){ # skip gam if too few data
        plot(dfsuba$DOY,dfsuba$AbundancePTD,type="p",main=j,
             ylim=c(0,1.05*max(1,max(dfsuba$AbundancePTD,na.rm=TRUE))),
             xlim=c(154,238))#Det er de åbne symboler der ikke når threshold.
      }
      else{
        g <- gam(round(AbundancePTD,2)~s(DOY, k=4),family=poisson(link = "log"),data=dfsuba)
        pred <- data.frame(DOY = seq(154,238, by=0.1)) # frame of model predictions: day 154 to 238.
        pred$AbundancePTD <- predict(g, newdata = pred, type ="response") # Get predicted values under new values of avgsr and trapdays.
        plot(pred$DOY,pred$AbundancePTD,type="l",col="black",lwd=1,main=j,
             ylim=c(0,1.05*max(c(max(pred$AbundancePTD,na.rm=TRUE),max(dfsuba$AbundancePTD,na.rm=TRUE)))),
                    xlim=c(154,238))
        points(dfsuba$DOY,dfsuba$AbundancePTD,pch=16,cex=1.5)#lukkede symboler der opfylder threshold værdier
      }
    }
  }
  dev.off()
}

df_test <- subset(df7, df7$SpeciesID == "ANMU")
df_test <- subset(df_test, df_test$Plot == "Art2")
df_test <- subset(df_test, df_test$Year == 1997)

length(df_test[df_test$AbundancePTD >= 1, 1])

####################
# Four functions (phenodate,phenogam,modpred,EM) to calculate
# "Onset", "Peak" og "End". Calculated as 10%, 50% og 90% of arthropod taxa 
# in each plot caught in a season
####################

class(df7$Abundance)

#### phenodate --- FUNKTION ####
phenodate<-function(mod,em.level)
{if(min(mod$prop)>em.level){result<-NA}
  else{ lower<-(max(mod$prop[mod$prop <= em.level]))
  upper<-(min(mod$prop[mod$prop > em.level]))
  lower_doy<-mod$DOY[which(mod$prop==lower)]
  upper_doy<-mod$DOY[which(mod$prop==upper)]
  result<-((em.level - lower) * ((upper_doy - lower_doy) / (upper - lower)) + lower_doy)}
  return(result)}
#This function interpolates to the date equal to a given fraction (em.level) of the total seasonal capture rate
#em.level: onset=0.10, peak=0.50, end=0.90


#### phenogam --- FUNKTION ####
phenogam <- function(SpeciesID, Plot, Year, data = df7)
{y <- data[ data$SpeciesID == SpeciesID & 
              data$Plot == Plot &
              data$Year == Year, ] # Extracting data from species, plot, year combination from df7
event <- length(y[y$AbundancePTD >= 1, 1])
cum.abundance <- sum(y$Abundance)
if(event <= 2 || cum.abundance <= 49) # Mindst 3 events for at lave g værdi og/eller 50+ abundans.
{return(list(NA))} # Return NA, if less than two events have more than 25 specimens.
g <- gam(round(AbundancePTD,0)~s(DOY, k=4),family=poisson(link = "log"),data=y)#man skal runde abundans op til 0 decimaler fordi der er blevet to familier lagt sammen i starten og derefter skilt ad fordi man fandt ud af at chironomidae var ca. 2% af den samlede og ceratopogonidae var de resterende. Se tokes artikel interannual, spacial, seasonal..
return(g)}
#Estimates a non-linear GAM function to describe seasondynamics for a species in a plot in a year.
#k describes number of dimensions used in the function


#### modpred --- FUNKTION ####
modpred <- function(SpeciesID, Plot, Year, data = df7)
{y <- data[ data$SpeciesID == SpeciesID & 
              data$Plot == Plot & 
              data$Year == Year, ] # Extracting data from species, plot, year combination
phenogam.result <- phenogam(SpeciesID, Plot, Year) # Get the gam-object
pred <- data.frame(DOY = seq(154,238, by=0.1)) # frame of model predictions: day 154 to 238.
if(length(phenogam.result) <= 1)
{result <- rep(NA,3)} # Assign NA if no gam model, three times because in table there is three(Onset, peak, end) for each combination of SpeciesID, Plot, Year.
else  # Give gam model prediction if there is a gam model
{pred$AbundancePTD <- predict(phenogam.result, newdata = pred, type ="response") # Get predicted values under new values of avgsr and trapdays.
pred$csum <- ave(pred$AbundancePTD, FUN=cumsum)
pred$prop <- pred$csum/max(pred$csum)
result<- c(Onset=phenodate(mod=pred,0.1),Peak=phenodate(mod=pred,0.5),End=phenodate(mod=pred,0.9))}
return(result)} #return predict table with predicted values.
#Calculates seasondynamics per 0.1 day from the GAM function estimated in phenogam and calculates onset, peak and end


print(df7$SpeciesID)
print(levels(df7$SpeciesID))
#### EM --- FUNKTION ####
EM <- sapply(levels(as.factor(df7$SpeciesID)), function(SpeciesID) {
  print(SpeciesID)
  #print(levels(as.factor(df7$Plot)))
  sapply(levels(as.factor(df7$Plot)), function(Plot) {
    #print("Plot")
    sapply(levels(as.factor(df7$Year)), function(Year) {
      #print("Year")
      ope.liste <- c(modpred(SpeciesID = SpeciesID, Plot = Plot, Year = Year))
      return(ope.liste)
    })
  })
})
# ope.liste = onset, peak, end, listen.
#Returns a list with results, but takes a while to run!


#Creates list to dataframe
dfEM <- as.data.frame(EM)
dfEM$Pheno.Event <- rep(c("Onset","Peak","End"),175) #25*7=175
dfEM$Year<- rep(c(seq(1996,2010,by=1),seq(2011,2020,by=1)),each=3,times=7)
dfEM$Plot<-rep(c("Art1","Art2","Art3","Art4","Art5","Art6","Art7"),each=75)#25*3=75

#All data for families are piled
dfOPE <- gather(dfEM,key=SpeciesID,value=DOY,1:71,-Year,-Plot,-Pheno.Event)

df7%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(TotalAbundance=sum(Abundance),
            MeanAbundance=mean(Abundance,na.rm=T))->df7a

dfOPE$TotalAbundance <- df7a$TotalAbundance[match(paste0(dfOPE$SpeciesID,dfOPE$Year,dfOPE$Plot),
                                                  paste0(df7a$SpeciesID,df7a$Year,df7a$Plot))]

dfOPE$TotalAbundance[is.na(dfOPE$TotalAbundance)] <- 0


length(dfEM)
#OPE = Onset, Peak, End
#write.csv(df5, file = "Data/Dataset_for_GAM\\EMdata_final.csv", row.names=FALSE)
write.csv(dfOPE, file = "Data/Dataset_for_GAM_NEW\\dfOPE_dataframe_ptd.csv", row.names=FALSE)
#write.table(dfOPE, file = "OPE_liste.txt", sep = "\t")
dfOPE<-read.csv2("Data/Dataset_for_GAM_NEW/dfOPE_dataframe_ptd.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)

dfOPEsub <- subset(dfOPE, dfOPE$DOY!= "NA", select=-Year)

class(dfOPEsub$DOY)
dfOPEsub$DOY<-as.numeric(dfOPEsub$DOY)
class(dfOPEsub$DOY)
#dfOPEsub[,"DOY", drop = FALSE] #To change dimensions
dfOPEsub%>%
  group_by(SpeciesID,Plot,Pheno.Event)%>%
  summarise(MinDOY= min(DOY), MeanDOY= mean(DOY),MaxDOY = max(DOY))->dfOPEsub
write.table(dfOPEsub, file="Data/Dataset_for_GAM_NEW\\OPE_mmm_ptd.txt",sep = "\t")

sort(unique(dfOPE$SpeciesID))

#Columns with Onset, Peak, and End.
#Calculation of duration of arthropod activity
df8 <- dfOPE %>% 
  spread(Pheno.Event, DOY)

df8$End<-as.numeric(df8$End)
df8$Onset<-as.numeric(df8$Onset)
df8$Peak<-as.numeric(df8$Peak)
df8$TotalAbundance<-as.numeric(df8$TotalAbundance)

df8$Duration <- (df8$End - df8$Onset)
df8 <- df8 %>% 
  select(Year, Plot, SpeciesID, TotalAbundance, Onset, Peak, End, Duration)
df8sub <- subset(df8, df8$Duration != "NA") 
df8$Year<- as.factor(df8$Year)
df8sub$Year<- as.factor(df8sub$Year)

write.csv(df8, file="Data/Dataset_for_GAM_NEW\\duration_ptd.csv", row.names = FALSE)
write.csv(df8sub, file="Data/Dataset_for_GAM_NEW\\duration_subset_ptd.csv",row.names = FALSE)
write.table(df8sub, file="Data/Dataset_for_GAM_NEW\\duration_subset_ptd.txt",sep ="\t",row.names = FALSE)
#Her laver jeg lige filer af df8 og df8sub, så behøves jeg bare læse dem.


############################################################
#Datasets saved which include:
#dfOPE = Pheno.Event, Year, Plot, SpeciesID, DOY.
#df8 =  Year, Plot, SpeciesID, Onset, Peak, End, Duration.
#df8sub = Year, Plot, SpeciesID, Onset, Peak, End, Duration. (No NA's)
############################################################



#Uploading datasets
dfOPE <- read.csv("Data/Dataset_for_GAM_NEW/dfOPE_dataframe.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)
dfOPE$Year<- as.factor(dfOPE$Year)
df8_old <- read.csv("Data/Dataset_for_GAM_NEW\\duration.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)
df8$Year<- as.factor(df8$Year)
df8sub <- read.csv2("Data/Dataset_for_GAM_NEW\\duration_subset.txt",  sep="\t",stringsAsFactors = FALSE, header = TRUE)
df8sub$Year<- as.factor(df8sub$Year)

head(dfOPE)

#Checking correlation

#Peak and Onset
df_test <- subset(df8,!is.na(Onset))

df_test%>%
  group_by(SpeciesID,Plot)%>%
  summarise(cor=cor(Onset,Peak, method = "pearson")) -> df_Cor

df_test%>%
  group_by(SpeciesID,Plot)%>%
  summarise(cor=cor(End,Peak, method = "pearson")) -> df_CorEnd

cor(df_test$Onset, df_test$Peak, method = "pearson")
cor(df_test$Peak, df_test$End, method = "pearson")

test_Onset <- cor.test(df_test$Onset, df_test$Peak)
test_Onset

test_End <- cor.test(df_test$End, df_test$Peak)
test_End

test <- cor.test(df_test$End, df_test$Onset)
test


