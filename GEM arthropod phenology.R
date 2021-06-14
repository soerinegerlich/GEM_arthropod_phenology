#Alle nedstående er nogle toolboxes der skal installeres og køres inden man begynder at kigge på data.
#install.packages("RDocumentation")#her er RDocumentation lige instaleret manuelt. Kan også gøre det i Tools.
library(tidyverse)
library(tidyr)
library(RDocumentation)#Denne gør din help() funktion bedre
library(readxl) #Indlæser excel filer, import into R.
library(lubridate) #Beregner datoer og tidspunkter. Det er en toolbox, eller en række tools der hjælper med at fremstille tid/datoer bedre. 
library(mgcv) #funktioner der kan analysere med GAM og generalised additive mixed modeling.
library(MESS) #teste antagelser i GAM, statistiske detaljer.
library(corrplot)

##### Zackenberg data - downloaded 16 April 2021 #####

#/Users/Soerine/Documents/PhD projekt/Data/Data_Arthropods Zackenberg
#C:\Users\au511627\OneDrive - Aarhus Universitet\PhD projekt\Data\Data_Arthropods Zackenberg\View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence1604202114574319


df1 <- read.csv2("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence1604202114574319.csv",sep="\t",stringsAsFactors = FALSE)
#df1 <- read.csv2("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence030120201503537843.csv",sep="\t",stringsAsFactors = FALSE)
#df1 <- read.csv2("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_phenology191220172147231575.csv",sep="\t")
#df1 <- read_excel("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence030120201503537843.xlsx")
#df2 <- read_excel("Data/Metadata for columns03012020150355003.xlsx")

#Rename columns.
#Det er bestemt at der ikke skal være mellemrum mellem snow og bogstav, also 
df1 = df1 %>% rename("DaysA"="Days.A", "DaysB"="Days.B", "DaysC"="Days.C", "DaysD"="Days.D", "DaysE"="Days.E", "DaysF"="Days.F", "DaysG"="Days.G", "DaysH"="Days.H")
df1 = df1 %>% rename("Plot"="Plot.ID", "Date"="ï..Date") #Fejl vises ved Date. 
#df1 = df1 %>% rename("Plot"="Plot.ID")


#Her skal vi i alle Days kolonnerne erstatte -9999 med NA, dette fordi i vores excel art står der -9999, men de betyder bare at der ikke er noget snow.
df1$DaysA[df1$DaysA == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysB[df1$DaysB == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysC[df1$DaysC == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysD[df1$DaysD == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysE[df1$DaysE == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysF[df1$DaysF == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysG[df1$DaysG == -9999] <- NA #Replace '-9999' with 'NA'
df1$DaysH[df1$DaysH == -9999] <- NA #Replace '-9999' with 'NA'

#Her gør vi det samme for abundansen observeret, men vi skriver 0 fordi ellers kan vi ikke bruge abundansen til at regne med senere.
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

#Nogle steder i data står der NA men det anses for at være skrift og ikke NA fra R. Det skal laves om til 0 for at vi igen kan regne med abundans senere.
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

#Tilføjer nye kolonner med DOY, uge nummer, year.
df1$DOY<-yday(ymd(df1$Date)) #Add Day of year variable
df1$DOY7<-floor(df1$DOY/7)*7 #Add week column. deler DOY med 7, fjerner decimaler(floor) og tager tallet som "uge" nummer. Men denne formel er ikke helt rigtig, desuden er det ikke noget vi tager med videre.
df1$Month<-month(df1$Date) # Add month variable, men som en factor, altså noget som R kender/kan arbejde med.
df1$Year<-year(df1$Date) # Add year variable, men som en factor, altså noget som R kender/kan arbejde med.

#Change Days E,F,G,H to zero after 2006 trapdays are recorded, but samples not processed
#Fra 2006 blev antallet af traps reduceret til 4 fra 8. 
df1 <- within(df1, DaysE[Year>2006] <- 0)
df1 <- within(df1, DaysF[Year>2006] <- 0)
df1 <- within(df1, DaysG[Year>2006] <- 0)
df1 <- within(df1, DaysH[Year>2006] <- 0)


#Disse tre kolonner er indlæst som faktorer
df1$A <- as.numeric(df1$A)
df1$B <- as.numeric(df1$B)
df1$C <- as.numeric(df1$C)
df1$D <- as.numeric(df1$D)
df1$E <- as.numeric(df1$E)
df1$F <- as.numeric(df1$F)
df1$G <- as.numeric(df1$G)
df1$H <- as.numeric(df1$H)

#Combine taxonomic information into one column
df1$SpeciesID<-as.character(df1$Family)#vi vil helst have familien.
df1$SpeciesID[is.na(df1$SpeciesID)]<-as.character(df1$Order[is.na(df1$SpeciesID)])#Hvis der ikke er en familie for data, så prøver vi med orden.
df1$SpeciesID[is.na(df1$SpeciesID)]<-as.character(df1$Phylum[is.na(df1$SpeciesID)])#Hvis der ikke er en orden, så må det blive phylum.

#Adjust taxonomic names.
sort(unique(df1$SpeciesID))
df1$SpeciesID[df1$SpeciesID == "PieridaeÂ "] <- "Pieridae" #Der er et mærkeligt Â ud for disse taxa. Her fjernes det.
df1$SpeciesID[df1$SpeciesID == "TriopsidaeÂ "] <- "Triopsidae"

#Fjerner dele af data som ikke kan nøgles til familie niveau
df1<- subset(df1,SpeciesID!="unidentified")
df1<- subset(df1,SpeciesID!="Brachycera larvae")
df1<- subset(df1,SpeciesID!="Cyclorrhapha larvae")
df1<- subset(df1,SpeciesID!="Lepidoptera larvae")
df1<- subset(df1,SpeciesID!="Diptera larvae")
df1<- subset(df1,SpeciesID!="Nematocera larvae")
df1<- subset(df1,SpeciesID!="Symphyta larvae")
df1<- subset(df1,SpeciesID!="Tipulidae larvae")
df1<- subset(df1,SpeciesID!="Hymenoptera larvae")
df1<- subset(df1,Year!="2010")
#Fjerner 2010, fordi alle kasser med dyr er mistet på vej til DK, på nær lidt prøver fra sidst på sæson. Det er ikke en fuld sæson. hele året bliver fjernet.

length(unique(df1$SpeciesID))#ender op med 69 levels.#Ender med 70. Er der tilføjet en art mere siden sidst?
#sort(unique(df1$Year))

#Summarize to one value per SpeciesID,Year,Plot,DOY
df1%>%
  group_by(SpeciesID,Year,Plot,Month,DOY)%>%
  summarise(DaysA=mean(DaysA),DaysB=mean(DaysB),DaysC=mean(DaysC),DaysD=mean(DaysD),
            DaysE=mean(DaysE),DaysF=mean(DaysF),DaysG=mean(DaysG),DaysH=mean(DaysH),
  A=sum(A),B=sum(B),C=sum(C),D=sum(D),E=sum(E),F=sum(F),G=sum(G),H=sum(H))->df2
#Her laves et nyt datasæt kaldt df2


#Sum of abundance in all traps, ny kolonne. Abundance regnes dog først i koden nedenfor.
#Lægger tal fra alle traps i et plot sammen.
#df2$Abundance<-df2$A+df2$B+df2$C+df2$D+df2$E+df2$F+df2$G+df2$H
df2$Trapdays<-df2$DaysA+df2$DaysB+df2$DaysC+df2$DaysD+df2$DaysE+df2$DaysF+df2$DaysG+df2$DaysH

#sum(subset(df2,SpeciesID=="Acari")$Abundance)

df2$Abundance <- as.numeric(df2$Abundance)

#Abundans test plots - Family level data. 

##Funktionen spread() kan ikke aggregere data og derfor skal group_by og summarise funktioner inkorporeres.
##Andet problem er, at der dannes NA ved alle DOY's hvor der ikke er blevet samlet dyr ind og dette varierer mellem taxa. 
df2%>%
  group_by(SpeciesID,Year,Plot,Month,DOY,Trapdays)%>%
  summarise(Abundance=A+B+C+D+E+F+G+H)%>%
  within(Abundance[is.na(Abundance)] <- 0)%>%
  spread(key=SpeciesID,value=Abundance, convert = TRUE)%>%
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


#Stikproever for at tjekke om værdierne i df3 er korrekte
dfAcari<-subset(df2,SpeciesID=="Acari")
dfAcari<-subset(dfAcari,Plot=="Art1")
dfAcari<-subset(dfAcari,Year=="1996")

dfAcari2<-subset(df3,SpeciesID=="Acari")
dfAcari2<-subset(dfAcari2,Plot=="Art1")
dfAcari2<-subset(dfAcari2,Year=="1996")

dfAnthomyiidae2<-subset(df2,SpeciesID=="Anthomyiidae")
dfAnthomyiidae2<-subset(dfAnthomyiidae2,Plot=="Art5")
dfAnthomyiidae2<-subset(dfAnthomyiidae2,Year=="2002")

dfAgromyzidae2<-subset(df2,SpeciesID=="Agromyzidae")
dfAgromyzidae2<-subset(dfAgromyzidae2,Plot=="Art5")
dfAgromyzidae2<-subset(dfAgromyzidae2,Year=="2002")

dfCHCE2<-subset(df3,SpeciesID=="CHCE")
dfCHCE2<-subset(dfCHCE2,Plot=="Art1")
dfCHCE2<-subset(dfCHCE2,Year=="1997")


#Sørger lige for at fjerne alle NA og erstatter med 0.
df3$Abundance[is.na(df3$Abundance)] <- 0
#Kolonne med Event oprettes. Hvis abundansen er mere end 0 skrives 1, ellers 0.
#Dette for at sige at der er et event.
df3$Event<-ifelse(df3$Abundance>0,1,0)

df3$Month<- as.numeric(df3$Month)

df3%>%
  subset(Month>5&Month<9)%>% #månderne maj til september
  group_by(SpeciesID,Plot,Year)%>%
  summarise(TotalAbundance=sum(Abundance),TotalEvents=sum(Event))->df2a

df2a$Include<-ifelse(df2a$TotalAbundance>25&df2a$TotalEvents>2,1,0)#Need at least ? individuals in a season and 3 capture events
#mindst 25 individer, indsamlet mindst 3 gange. På et enket år for en enkel art og plot.
#df2a$Include<-ifelse(df2a$TotalAbundance<500,0,1)#Need at least 100 individuals across years per plot 
#I den nye kolonne som hedder Include. Ifelse(test_expression,x,y). Resultater vises som enten 0 (mindre end 500) eller 1 (højere end 500).
#Der sorteres efter total abundans i df2a som derefter skal overføres til df3.
#Filter original data for sampling criterias
df3$Include <- (df2a$Include[match(paste0(df3$SpeciesID,df3$Year,df3$Plot),paste0(df2a$SpeciesID,df2a$Year,df2a$Plot))])#paste betyder at det er kombinationen af variable der skal matche.
#Her er lavet en include kolonne i df3 der har resultaterne af include fra df2a.
#Her er det vigtigt at notere at denne include kommer ved alle de DOY, arter, year, plot hvor der er en include fordi der har været 3 events ÅRLIGT for denne art, plot, og der har været en abundans over 25.
#df3$Include<-ifelse(df3$TotalAbundanceYear>24&df3$TotalEventsYear>2,1,0)

#Nu vil vi gerne have speciesID til at stå før plot.
df4 <-df3 %>% 
  select(SpeciesID, everything())


####NYT DATASÆT SOM SKAL BRUGES TIL GAM####
df6 <- data.frame(df3)
df5 <- select(df6, SpeciesID,Plot,Year,DOY,Abundance,Include,Event)
#df5$AbundancePTD <- (df5$Abundance/df5$Trapdays)
#df5$Include <- (df2a$Include[match(paste0(df4$SpeciesID,df4$Plot),paste0(df2a$SpeciesID,df2a$Plot))])
#Summarise funktionen giver ikke de rigtige resultater.
#summarise(Abundance = sum(Abundance),Trapdays=sum(Trapdays),AbundancePTD=Abundance/Trapdays)->df4

#Nogle resultater bliver NaN (kan ikke vise tallet) under AbundansPTD

class(df5$Include)

write.csv(df5, file = "Data/Dataset_for_GAM\\EMdata_final.csv", row.names=FALSE)
#row.names=FALSE fjerne første kolonne, da vi ellers får et problem med en kolonne X.


############################################################
#NU ER DER LAVET ET GEMT DATA DER INDEHOLDER:
#df4 = SpeciesID, Plot, Year, DOY, Abundance, Event, Include
############################################################

#df7<- read.csv("ZAC_all.csv")
df7<-read.csv2("Data/Dataset_for_GAM/EMdata_final.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)

#df8<-subset(df7,select=-c(X))

#Her læses filen ind som et tabel, da det er sådan den er gemt ovenfor.
#Man kunne også gemme den som en csv (ligesom den til Toke) og så læse den ind som csv.
#df5 <- read.csv("EMdata.csv", header = TRUE)

df7$Year<- as.factor(df7$Year)#den var en integer, men det er nemmere at arbejde med den som en faktor.
#df7$SpeciesID<- as.factor(df7$SpeciesID)
#df7$Plot<- as.factor(df7$Plot)

class(df7$Abundance)
typeof(df7$Abundance)
df7$Abundance<-as.numeric(df7$Abundance)
class(df7$Abundance)

#class(df7$Trapdays)
#df7$Trapdays<-as.integer(df7$Trapdays)
#class(df7$Trapdays)

#class(df7$AbundancePTD)
#df7$AbundancePTD<-as.numeric(df7$AbundancePTD)
#class(df7$AbundancePTD)

#Her indlæses filer med data der skal testes.
df7<-read.csv2("Data/Dataset_for_GAM/EMdata_final.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)
#df5<- read.csv("ZAC_all.csv")
df7$Year<- as.factor(df7$Year)

#Nu skal filerne TESTES - Der er blevet lavet en loop der indsætter alle SpeciesID i en fil. (df7)
df7%>%
  group_by(SpeciesID, Plot, Year)%>%
  summarise(Include= max(Include))->df7temp
df7temp%>%
  group_by(SpeciesID, Plot)%>%
  summarise(Total_GAM= sum(Include))->df7GAM
df7include <- subset(df7GAM, Total_GAM >= 5)
#>= 10 eller 5 eller? Der er ingen Total GAM på 10..

#########unique(df7include$SpeciesID)

##Laver en fil i en folder med figures for alle 69 species.
##Der benyttes loop 'for' funktion, hvor der også indgår nested loops.
df7$Abundance<- as.numeric(df7$Abundance)
class(df7$Abundance)
df7$DOY<- as.numeric(df7$DOY)

for (k in unique(df7$SpeciesID)){
  dfsub<-subset(df7,SpeciesID==k)
  pdf(paste("Figures",k,".pdf"),width=20,height=12)
  par(mfrow=c(7,23),oma = c(5,5,4,0) ,mar = c(2,1,2,2) + 0.1) #it goes c(bottom, left, top, right) 
  for (i in unique(dfsub$Plot)){
    for(j in unique(dfsub$Year)){
      dfsuba<-subset(dfsub,Plot==i)
      dfsuba<-subset(dfsuba,Year==j)
      threshold <- length(dfsuba[dfsuba$Abundance >= 1, 1]) # beregne antal events
      sumabund<-sum(dfsuba$Abundance)
      if(threshold <= 2||sumabund<25){ # skip gam if too few data
        plot(dfsuba$DOY,dfsuba$Abundance,type="p",main=j,
             ylim=c(0,1.05*max(1,max(dfsuba$Abundance,na.rm=TRUE))),
             xlim=c(154,238))#Det er de åbne symboler der ikke når threshold.
      }
      else{
        g <- gam(round(Abundance,0)~s(DOY, k=4),family=poisson(link = "log"),data=dfsuba)
        pred <- data.frame(DOY = seq(154,238, by=0.1)) # frame of model predictions: day 154 to 238.
        pred$Abundance <- predict(g, newdata = pred, type ="response") # Get predicted values under new values of avgsr and trapdays.
        plot(pred$DOY,pred$Abundance,type="l",col="black",lwd=1,main=j,
             ylim=c(0,1.05*max(c(max(pred$Abundance,na.rm=TRUE),max(dfsuba$Abundance,na.rm=TRUE)))))
        points(dfsuba$DOY,dfsuba$Abundance,pch=16,cex=1.5)#lukkede symboler der opfylder threshold værdier
      }
    }
  }
  dev.off()
}


####################
# Herunder er fire funktioner (phenodate,phenogam,modpred,EM), der udfører trin i beregningen af
# de fænologiske begivenheder "Onset", "Peak" og "End". De udgør henholdsvis 10%, 50% og 90% af sæsonens fangst
# af dyr i et bestemt plot, år og af en bestemt artsgruppe. Metoden er beskrivet i Sarah Loboda's nye artikel
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
#Denne funktion, interpolerer til den dato, der svarer til en given andel (em.level) af den samlede sæsonfangst
#em.level: onset=0.10, peak=0.50, end=0.90


#### phenogam --- FUNKTION ####
phenogam <- function(SpeciesID, Plot, Year, data = df7)
  {y <- data[ data$SpeciesID == SpeciesID & 
              data$Plot == Plot &
              data$Year == Year, ] # Extracting data from species, plot, year combination from df7
    event <- length(y[y$Abundance >= 1, 1])
    cum.abundance <- sum(y$Abundance)
    if(event <= 2 || cum.abundance <= 24) # Mindst 3 events for at lave g værdi og/eller 25+ abundans.
      {return(list(NA))} # Return NA, if less than two events have more than 25 specimens.
  g <- gam(round(Abundance,0)~s(DOY, k=4),family=poisson(link = "log"),data=y)#man skal runde abundans op til 0 decimaler fordi der er blevet to familier lagt sammen i starten og derefter skilt ad fordi man fandt ud af at chironomidae var ca. 2% af den samlede og ceratopogonidae var de resterende. Se tokes artikel interannual, spacial, seasonal..
  return(g)}
# Denne funktion estimerer en ikke-lineær gam funktion (generalized additive model) til at beskrive sæsondynamikken for
# en given art i et plot i et år.


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
    {pred$Abundance <- predict(phenogam.result, newdata = pred, type ="response") # Get predicted values under new values of avgsr and trapdays.
    pred$csum <- ave(pred$Abundance, FUN=cumsum)
    pred$prop <- pred$csum/max(pred$csum)
    result<- c(Onset=phenodate(mod=pred,0.1),Peak=phenodate(mod=pred,0.5),End=phenodate(mod=pred,0.9))}
  return(result)} #return predict table with predicted values.
# Denne funktion beregner sæsondynamik per 0.1 dag ud fra gam funktionen estimeret i phenogam og kalder
# phenodate for at beregne Onset, Peak og End for de predikterede værdier


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
# Denne funktion looper gennem alle arter, plots og år og returnerer en liste med alle beregninger. 
# Den tager nogle minutter om at køre
# ope.liste = onset, peak, end, listen.


#Her laves listen (EM) om til en dataframe
dfEM <- as.data.frame(EM)
#Her tilføjes beskrivende variable
dfEM$Pheno.Event <- rep(c("Onset","Peak","End"),161) #23*7=161
dfEM$Year<- rep(c(seq(1996,2009,by=1),seq(2011,2019,by=1)),each=3,times=7)
dfEM$Plot<-rep(c("Art1","Art2","Art3","Art4","Art5","Art6","Art7"),each=69)#23*3=69


#Her stables alle artsdata ovenpå hinanden.
dfOPE <- gather(dfEM,key=SpeciesID,value=DOY,1:69,-Year,-Plot,-Pheno.Event)
#ncol(dfEM) kan erstatte 69.
#length(dfEM). For at finde ud af længden af datasættet. Det er 69.
#OPE = Onset, Peak, End
#write.csv(df5, file = "Data/Dataset_for_GAM\\EMdata_final.csv", row.names=FALSE)
write.csv(dfOPE, file = "Data/Dataset_for_GAM_NEW\\dfOPE_dataframe.csv", row.names=FALSE)
#write.table(dfOPE, file = "OPE_liste.txt", sep = "\t")
dfOPE<-read.csv2("Data/Dataset_for_GAM_NEW/dfOPE_dataframe.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)
#Denne fil er nu noget der skal bruges til nogen ggplots

#Der skal lige laves en ekstra tabel med minimum, gennemsnitlig og maximum pheno.events per art per plot.
dfOPEsub <- subset(dfOPE, dfOPE$DOY!= "NA", select=-Year)
#Husk at arbejde med DOY som numerisk værdi, ellers melder den fejl.
class(dfOPEsub$DOY)
dfOPEsub$DOY<-as.numeric(dfOPEsub$DOY)
class(dfOPEsub$DOY)
#dfOPEsub[,"DOY", drop = FALSE] #For at ændre dimensioner. Det er dog nok at ændre typen.
dfOPEsub%>%
  group_by(SpeciesID,Plot,Pheno.Event)%>%
  summarise(MinDOY= min(DOY), MeanDOY= mean(DOY),MaxDOY = max(DOY))->dfOPEsub
write.table(dfOPEsub, file="Data/Dataset_for_GAM_NEW\\OPE_mmm.txt",sep = "\t")

sort(unique(dfOPE$SpeciesID))

#Her har JEG lavet kolonner fra Onset, Peak, og Event.
#Har også lavet en kolonne der udregner duration, derefter omarrangeret kolonnerne.
df8 <- dfOPE %>% 
  spread(Pheno.Event, DOY)
#Tjek data:
dfAcari3<-subset(dfOPE,SpeciesID=="Acari")
dfAcari3<-subset(dfAcari3,Plot=="Art2")
dfAcari3<-subset(dfAcari3,Year=="1997")
class(dfOPE$End)
df8$End<-as.numeric(df8$End)
df8$Onset<-as.numeric(df8$Onset)
df8$Peak<-as.numeric(df8$Peak)

df8$Duration <- (df8$End - df8$Onset)
df8 <- df8 %>% 
  select(Year, Plot, SpeciesID, Onset, Peak, End, Duration)
df8sub <- subset(df8, df8$Duration != "NA") #Dette subset er kun værdier der ikke har NA
df8$Year<- as.factor(df8$Year)
df8sub$Year<- as.factor(df8sub$Year)
#Her husker man at lave year om til en faktor, hvis nu df8 el. df8sub skal bruges til udregninger.

write.csv(df8, file="Data/Dataset_for_GAM_NEW\\duration.csv", row.names = FALSE)
write.csv(df8sub, file="Data/Dataset_for_GAM_NEW\\duration_subset.csv",row.names = FALSE)
write.table(df8sub, file="Data/Dataset_for_GAM_NEW\\duration_subset.txt",sep ="\t",row.names = FALSE)
#Her laver jeg lige filer af df8 og df8sub, så behøves jeg bare læse dem.


############################################################
#NU ER DER LAVET ET GEMT DATA DER INDEHOLDER:
#dfOPE = Pheno.Event, Year, Plot, SpeciesID, DOY.
#df8 =  Year, Plot, SpeciesID, Onset, Peak, End, Duration.
#df8sub = Year, Plot, SpeciesID, Onset, Peak, End, Duration. (minus dem med NA)
############################################################


#################################################################
# Nu skal vi lave fine plots (WORK IN PROGRESS)
#################################################################

#Her indlæses filer med data der er blevet testet og nu skal der arbejdes videre
dfOPE <- read.csv("Data/Dataset_for_GAM_NEW/dfOPE_dataframe.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)
dfOPE$Year<- as.factor(dfOPE$Year)
df8 <- read.csv("Data/Dataset_for_GAM_NEW\\duration.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)
df8$Year<- as.factor(df8$Year)
df8sub <- read.csv2("Data/Dataset_for_GAM_NEW\\duration_subset.txt",  sep="\t",stringsAsFactors = FALSE, header = TRUE)
df8sub$Year<- as.factor(df8sub$Year)

head(dfOPE)

####SE I SCRIPT 'DATA FOR PLOTS'####

