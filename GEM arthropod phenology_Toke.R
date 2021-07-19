#Alle nedstående er nogle toolboxes der skal installeres og køres inden man begynder at kigge på data.
#install.packages("RDocumentation")#her er RDocumentation lige instaleret manuelt. Kan også gøre det i Tools.
library(tidyverse)
library(RDocumentation)#Denne gør din help() funktion bedre
library(readxl) #Indlæser excel filer, import into R.
library(lubridate) #Beregner datoer og tidspunkter. Det er en toolbox, eller en række tools der hjælper med at fremstille tid/datoer bedre. 
library(mgcv) #funktioner der kan analysere med GAM og generalised additive mixed modeling.
library(MESS) #teste antagelser i GAM, statistiske detaljer.
library(corrplot)

##### Zackenberg data - downloaded 3 Jan 2020 #####

df1 <- read.csv2("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence030120201503537843.csv",sep="\t",stringsAsFactors = FALSE)
#df1 <- read.csv2("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_phenology191220172147231575.csv",sep="\t")
#df1 <- read_excel("Data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence030120201503537843.xlsx")
#df2 <- read_excel("Data/Metadata for columns03012020150355003.xlsx")

#Rename columns.
#Det er bestemt at der ikke skal være mellemrum mellem snow og bogstav, also 
df1 = df1 %>% rename("DaysA"="Days.A", "DaysB"="Days.B", "DaysC"="Days.C", "DaysD"="Days.D", "DaysE"="Days.E", "DaysF"="Days.F", "DaysG"="Days.G", "DaysH"="Days.H")
df1 = df1 %>% rename("Plot"="Plot.ID", "Date"="ï..Date")
#% betyder at man ændre i tabellen ikke laver en ny baseret på den gamle.

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
df1$Month<-month(df1$Date) # Add year variable, men som en factor, altså noget som R kender/kan arbejde med.
df1$Year<-year(df1$Date) # Add year variable, men som en factor, altså noget som R kender/kan arbejde med.

#Change Days E,F,G,H to zero after 2006 trapdays are recorded, but samples not processed
df1 <- within(df1, DaysE[Year>2006] <- 0)
df1 <- within(df1, DaysF[Year>2006] <- 0)
df1 <- within(df1, DaysG[Year>2006] <- 0)
df1 <- within(df1, DaysH[Year>2006] <- 0)

#Disse tre kolonner er indlæst som faktorer
df1$A <- as.numeric(df1$A)
df1$C <- as.numeric(df1$C)
df1$E <- as.numeric(df1$E)

#Combine taxonomic information into one column
df1$SpeciesID<-as.character(df1$Family)#vi vil helst have familien.
df1$SpeciesID[is.na(df1$SpeciesID)]<-as.character(df1$Order[is.na(df1$SpeciesID)])#Hvis der ikke er en familie for data, så prøver vi med orden.
df1$SpeciesID[is.na(df1$SpeciesID)]<-as.character(df1$Phylum[is.na(df1$SpeciesID)])#Hvis der ikke er en orden, så må det blive phylum.

#Adjust taxonomic names.
sort(unique(df1$SpeciesID))
df1$SpeciesID[df1$SpeciesID == "Pieridae?"] <- "Pieridae"
df1$SpeciesID[df1$SpeciesID == "Triopsidae?"] <- "Triopsidae"

#Dennne sum kan godt laves for alle, for at se hvor mange det er man fjerner. (Det har jeg gjort)
sum(subset(df1,SpeciesID=="unidentified")$Abundance) #der er 91
sum(subset(df1,SpeciesID=="Brachycera larvae")$Abundance) #der er 11
sum(subset(df1,SpeciesID=="Cyclorrhapha larvae")$Abundance) #der er 495
sum(subset(df1,SpeciesID=="Lepidoptera larvae")$Abundance) #der er 1310
sum(subset(df1,SpeciesID=="Diptera larvae")$Abundance) #der er 8
sum(subset(df1,SpeciesID=="Nematocera larvae")$Abundance) #der er 750
sum(subset(df1,SpeciesID=="Symphyta larvae")$Abundance) #der er 2
sum(subset(df1,SpeciesID=="Tipulidae larvae")$Abundance) #der er 1
sum(subset(df1,SpeciesID=="Hymenoptera larvae")$Abundance) #der er 33
sum(subset(df1,Year=="2010")$Abundance) 

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

length(unique(df1$SpeciesID))#ender op med 69 levels.
#sort(unique(df1$Year))

#Summarize to one value per SpeciesID,Year,Plot,DOY
df1%>%group_by(SpeciesID,Year,Plot,Month,DOY)%>%
  summarise(DaysA=mean(DaysA),DaysB=mean(DaysB),DaysC=mean(DaysC),DaysD=mean(DaysD),
            DaysE=mean(DaysE),DaysF=mean(DaysF),DaysG=mean(DaysG),DaysH=mean(DaysH),
  A=sum(A),B=sum(B),C=sum(C),D=sum(D),E=sum(E),F=sum(F),G=sum(G),H=sum(H))->df2

# #Sum of abundance in all traps, ny kolonne. $ betyder at man laver en ny kolonne
df2$Abundance<-df2$A+df2$B+df2$C+df2$D+df2$E+df2$F+df2$G+df2$H
df2$Trapdays<-df2$DaysA+df2$DaysB+df2$DaysC+df2$DaysD+df2$DaysE+df2$DaysF+df2$DaysG+df2$DaysH

#Abundans test plots - Family level data
df2%>%
  spread(key=SpeciesID,value=Abundance)%>%
  gather(key=SpeciesID,value=Abundance,22:90)%>%
  within(Abundance[is.na(Abundance)] <- 0)%>%
  spread(key=SpeciesID,value=Abundance)%>%
  mutate(ANMU=Muscidae+Anthomyiidae+Anthomyzidae,
         CHCE=Chironomidae+Ceratopogonidae,
         MYSC=Mycetophilidae+Sciaridae)%>%
  select(-c(Muscidae,Anthomyiidae,Anthomyzidae,Chironomidae,Ceratopogonidae,Mycetophilidae,Sciaridae))%>%
  gather(key=SpeciesID,value=Abundance,22:86)->df3

df3%>%
  subset(Month>5&Month<9)%>%
  group_by(SpeciesID,Plot)%>%
  summarise(TotalAbundance= sum(Abundance))->df2a

df2a$Include<-ifelse(df2a$TotalAbundance<500,0,1)#Need at least 100 individuals across years per plot

#Filter original data for sampling criterias
df3$Include <- (df2a$Include[match(paste0(df3$SpeciesID,df3$Plot),paste0(df2a$SpeciesID,df2a$Plot))])#paste betyder at det er kombinationen af variable der skal matche.

#Trapdays
df3%>%
  #subset(Month>5&Month<9)%>%
  group_by(SpeciesID, Plot, Year, DOY)%>%
  summarise(TrapdaysA=mean(DaysA),
            TrapdaysB=mean(DaysB),
            TrapdaysC=mean(DaysC),
            TrapdaysD=mean(DaysD),
            TrapdaysE=mean(DaysE),
            TrapdaysF=mean(DaysF),
            TrapdaysG=mean(DaysG),
            TrapdaysH=mean(DaysH),
            Trapdays=mean(Trapdays))%>%
  group_by(Plot,Year,DOY)%>%
  summarise(Trapdays=mean(Trapdays,na.rm=T))%>%
  ggplot(aes(DOY,Trapdays,colour=Plot)) + ylab("Trap days") + 
  geom_point() + facet_grid(Plot~Year,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Trapdays
df3%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6")%>%
  subset(Plot!="Art6"&Month>5&Month<9)%>%
  group_by(Plot,Year,DOY)%>%
  summarise(Trapdays=mean(Trapdays))%>%
  group_by(Plot,Year)%>%
  summarise(Trapdays=sum(Trapdays))%>%
  ggplot(aes(Year,Trapdays)) + ylab("Trap days") + 
  geom_point()+facet_grid(~Plot,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Abundance
df3%>%
  #subset(Month>5&Month<9&Plot!="Art6"&SpeciesID=="Anthomyiidae")%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="Anthomyzidae")%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="Agromyzidae")%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="ANMU")%>%
  group_by(Plot, Year, DOY)%>%
  summarise(Abundance = sum(Abundance),
            AbundanceA=sum(A),
            AbundanceB=sum(B),
            AbundanceC=sum(C),
            AbundanceD=sum(D),
            AbundanceE=sum(E),
            AbundanceF=sum(F),
            AbundanceG=sum(G),
            AbundanceH=sum(H),
            Trapdays=mean(Trapdays))%>%
  group_by(Plot,Year,DOY)%>%
  summarise(Abundance=sum(Abundance,na.rm=T))%>%
  ggplot(aes(DOY,Abundance)) + ylab("Abundance") + 
  geom_line() + facet_grid(Plot~Year,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Abundance
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

#AbundancePTD - single groups
df3%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="Noctuidae")%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="Syrphidae")%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="Linyphiidae")%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="Thomisidae")%>%
  subset(Month>5&Month<9&Include==1&Plot!="Art6"&(SpeciesID=="MYSC"|SpeciesID=="Acari"))%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="CHCE")%>%
  #subset(Month>5&Month<9&Include==1&Plot!="Art6"&SpeciesID=="MYSC")%>%
  group_by(SpeciesID,Plot,Year)%>%
  summarise(Abundance = sum(Abundance),
            Trapdays=sum(Trapdays),
            AbundancePTD=Abundance/Trapdays)%>%
  ggplot(aes(Year,AbundancePTD)) + ylab("Abundance per trap day") + 
  geom_smooth(method="lm")+geom_point()+facet_wrap(SpeciesID~Plot,scales = "free_y",ncol=6) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Crosscorrelations
df3%>%
  subset(Month>5&Month<9&Include==1)%>%
  subset(Plot=="Art5"|Plot=="Art7")%>%
  group_by(SpeciesID,Year)%>%
  summarise(AbundancePTD=log(sum(Abundance)+1)/sum(Trapdays))%>%
  spread(key=SpeciesID,value=AbundancePTD)%>%
  within(rm(Year))->corrdata1

df3%>%
  subset(Month>5&Month<9&Include==1)%>%
  subset(Plot=="Art3"|Plot=="Art4")%>%
  group_by(SpeciesID,Year)%>%
  summarise(AbundancePTD=log(sum(Abundance)+1)/sum(Trapdays))%>%
  spread(key=SpeciesID,value=AbundancePTD)%>%
  within(rm(Year))->corrdata2

df3%>%
  subset(Month>5&Month<9&Include==1)%>%
  subset(Plot=="Art2")%>%
  group_by(SpeciesID,Year)%>%
  summarise(AbundancePTD=log(sum(Abundance)+1)/sum(Trapdays))%>%
  spread(key=SpeciesID,value=AbundancePTD)%>%
  within(rm(Year))->corrdata3

df3%>%
  subset(Month>5&Month<9&Include==1)%>%
  subset(Plot=="Art1")%>%
  group_by(SpeciesID,Year)%>%
  summarise(AbundancePTD=log(sum(Abundance)+1)/sum(Trapdays))%>%
  spread(key=SpeciesID,value=AbundancePTD)%>%
  within(rm(Year))->corrdata4

M1<-cor(corrdata1,use="pairwise.complete.obs")
res1 <- cor.mtest(corrdata1, conf.level = .95,method="spearman")

M2<-cor(corrdata2,use="pairwise.complete.obs")
res2 <- cor.mtest(corrdata2, conf.level = .95,method="spearman")

M3<-cor(corrdata3,use="pairwise.complete.obs")
res3 <- cor.mtest(corrdata3, conf.level = .95,method="spearman")

M4<-cor(corrdata4,use="pairwise.complete.obs")
res4 <- cor.mtest(corrdata4, conf.level = .95,method="spearman")

par(mfrow=c(2,2),oma = c(5,2,2,5) + 0.1,mar = c(0,0,0,0) ) 
corrplot(M1, method="ellipse",type="lower",diag=FALSE,outline="black",p.mat = res1$p, 
         sig.level = 0.05,tl.col="black",tl.cex=1,cl.cex=1,addgrid.col=NA,mar=c(0.5,0.5,0.5,0.5))

corrplot(M2, method="ellipse",type="lower",diag=FALSE,outline="black",p.mat = res2$p, 
         sig.level = 0.05,tl.col="black",tl.cex=1,cl.cex=1,addgrid.col=NA,mar=c(0.5,0.5,0.5,0.5))

corrplot(M3, method="ellipse",type="lower",diag=FALSE,outline="black",p.mat = res3$p, 
         sig.level = 0.05,tl.col="black",tl.cex=1,cl.cex=1,addgrid.col=NA,mar=c(0.5,0.5,0.5,0.5))

corrplot(M4, method="ellipse",type="lower",diag=FALSE,outline="black",p.mat = res4$p, 
         sig.level = 0.05,tl.col="black",tl.cex=1,cl.cex=1,addgrid.col=NA,mar=c(0.5,0.5,0.5,0.5))

  corrdata%>%
  ggplot(aes(CHCE,Linyphiidae)) + geom_smooth(method="lm")+geom_point()

plot.new(); dev.off()
dftest<-subset(df1,SpeciesID=="Muscidae"&Year==1996)

ggplot(data=dftest, aes(DOY,Abundance, colour=Plot)) + ylab("Abundance") + 
  geom_point() + facet_wrap(~Plot,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(data=subset(df3,SpeciesID=="Chironomidae"&Plot!="Art6"), aes(as.numeric(as.character(Year)),AbundancePTD, colour=Plot)) + ylab("Abundance") + 
  geom_point() + geom_smooth(method="lm")+facet_wrap(~Plot,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(data=subset(df3,SpeciesID=="Muscidae"&Plot!="Art6"), aes(as.numeric(as.character(Year)),AbundancePTD, colour=Plot)) + ylab("Abundance") + 
  geom_point() + geom_smooth(method="lm")+facet_wrap(~Plot,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df1%>%subset(Year==2017&Plot=="Art2"&SpeciesID=="Muscidae")->dftest
head(dftest)


############################################################
#NU ER DER LAVET ET GEMT DATA DER INDEHOLDER:
#df4 = SpeciesID, Plot, Year, DOY, Abundance, Event, Include
############################################################


df5<- read.csv("ZAC_all.csv")
#Her læses filen ind som et tabel, da det er sådan den er gemt ovenfor.
#Man kunne også gemme den som en csv (ligesom den til Toke) og så læse den ind som csv.
#df5 <- read.csv("EMdata.csv", header = TRUE)

df5$Year<- as.factor(df5$Year)#den var en integer, men det er nemmere at arbejde med den som en faktor.


####################
# Herunder er fire funktioner (phenodate,phenogam,modpred,EM), der udfører trin i beregningen af
# de fænologiske begivenheder "Onset", "Peak" og "End". De udgør henholdsvis 10%, 50% og 90% af sæsonens fangst
# af dyr i et bestemt plot, år og af en bestemt artsgruppe. Metoden er beskrivet i Sarah Loboda's nye artikel
####################


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
phenogam <- function(SpeciesID, Plot, Year, data = df5)
  {y <- data[ data$SpeciesID == SpeciesID & 
              data$Plot == Plot &
              data$Year == Year, ] # Extracting data from species, plot, year combination from df5
    event <- length(y[y$Abundance >= 1, 1])
    cum.abundance <- sum(y$Abundance)
    if(event <= 2 || cum.abundance <= 24) # Mindst 3 events for at lave g værdi og/eller 25+ abundans.
      {return(list(NA))} # Return NA, if less than two events have more than 25 specimens.
  g <- gam(round(Abundance,0)~s(DOY, k=4),family=poisson(link = "log"),data=y)#man skal runde abundans op til 0 decimaler fordi der er blevet to familier lagt sammen i starten og derefter skilt ad fordi man fandt ud af at chironomidae var ca. 2% af den samlede og ceratopogonidae var de resterende. Se tokes artikel interannual, spacial, seasonal..
  return(g)}
# Denne funktion estimerer en ikke-lineær gam funktion (generalized additive model) til at beskrive sæsondynamikken for
# en given art i et plot i et år.


#### modpred --- FUNKTION ####
modpred <- function(SpeciesID, Plot, Year, data = df5)
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


#### EM --- FUNKTION ####
EM <- sapply(levels(df5$SpeciesID), function(SpeciesID)
        {sapply(levels(df5$Plot), function(Plot)
          {sapply(levels(df5$Year), function(Year)
            {ope.liste <- c(modpred(SpeciesID = SpeciesID, Plot = Plot, Year = Year))
      return(ope.liste)})})})
# Denne funktion looper gennem alle arter, plots og år og returnerer en liste med alle beregninger. 
# Den tager nogle minutter om at køre
# ope.liste = onset, peak, end, listen.


#Her laves listen (EM) om til en dataframe
dfEM <- as.data.frame(EM)
#Her tilføjes beskrivende variable
dfEM$Pheno.Event <- rep(c("Onset","Peak","End"),154)
dfEM$Year<- rep(c(seq(1996,2009,by=1),seq(2011,2018,by=1)),each=3,times=7)
dfEM$Plot<-rep(c("Art1","Art2","Art3","Art4","Art5","Art6","Art7"),each=66)


#Her stables alle artsdata ovenpå hinanden.
dfOPE <- gather(dfEM,key=SpeciesID,value=DOY,1:69)
#OPE = Onset, Peak, End
write.csv(dfOPE, file="OPE_liste.csv")
write.table(dfOPE, file = "OPE_liste.txt", sep = "\t")
#Denne fil er nu noget der skal bruges til nogen ggplots

#Der skal lige laves en ekstra tabel med minimum, gennemsnitlig og maximum pheno.events per art per plot.
dfOPEsub <- subset(dfOPE, dfOPE$DOY != "NA", select=-Year)
dfOPEsub%>%group_by(SpeciesID,Plot,Pheno.Event)%>%summarise(MinDOY= min(DOY), MeanDOY = mean(DOY), MaxDOY = max(DOY))->dfOPEsub
write.table(dfOPEsub, file="OPE_mmm.txt",sep = "\t")

#Her har JEG lavet kolonner fra Onset, Peak, og Event.
#Har også lavet en kolonne der udregner duration, derefter omarangeret kolonnerne.
df6 <- dfOPE %>% spread(Pheno.Event, DOY)
df6$Duration <- (df6$End - df6$Onset)
df6 <- df6 %>% select(Year, Plot, SpeciesID, Onset, Peak, End, Duration)
df6sub <- subset(df6, df6$Duration != "NA") #Dette subset er kun værdier der ikke har NA
df6$Year<- as.factor(df6$Year)
df6sub$Year<- as.factor(df6sub$Year)
#Her husker man at lave year om til en faktor, hvis nu df6 el. df6sub skal bruges til udregninger.

write.csv(df6, file="duration.csv")
write.csv(df6sub, file="duration_subset.csv")
write.table(df6sub, file="duration_subset.txt",sep ="\t")
#Her laver jeg lige filer af df6 og df6sub, så behøves jeg bare læse dem.


############################################################
#NU ER DER LAVET ET GEMT DATA DER INDEHOLDER:
#dfOPE = Pheno.Event, Year, Plot, SpeciesID, DOY.
#df6 =  Year, Plot, SpeciesID, Onset, Peak, End, Duration.
#df6sub = Year, Plot, SpeciesID, Onset, Peak, End, Duration. (minus dem med NA)
############################################################


#Her indlæses filer med data der skal testes.
df5<- read.csv("ZAC_all.csv")
df5$Year<- as.factor(df5$Year)

#Nu skal filerne TESTES - Der er blevet lavet en loop der indsætter alle SpeciesID i en fil. (df5)
df5%>%group_by(SpeciesID, Plot, Year)%>%summarise(Include= max(Include))->df5temp
df5temp%>%group_by(SpeciesID, Plot)%>%summarise(Total_GAM= sum(Include))->df5GAM
df5include <- subset(df5GAM, Total_GAM >= 10)

#########unique(df5include$SpeciesID)

##Laver en fil i en folder med figures for alle 69 species.
for (k in unique(df5$SpeciesID)){
  dfsub<-subset(df5,SpeciesID==k)
  pdf(paste("Figures/Fig. ",k,".pdf"),width=20,height=12)
  par(mfrow=c(7,22),oma = c(5,5,4,0) ,mar = c(2,1,2,2) + 0.1) #it goes c(bottom, left, top, right) 
  for (i in unique(dfsub$Plot)){
    for(j in unique(dfsub$Year)){
      dfsuba<-subset(dfsub,Plot==i)
      dfsuba<-subset(dfsuba,Year==j)
      threshold <- length(dfsuba[dfsuba$Abundance >= 1, 1]) # beregne antal events
      sumabund<-sum(dfsuba$Abundance)
      if(threshold <= 2||sumabund<25){ # skip gam if too few data
        plot(dfsuba$DOY,dfsuba$Abundance,type="p",main=j,
             ylim=c(0,1.05*max(1,max(dfsuba$Abundance))),
             xlim=c(154,238))#Det er de åbne symboler der ikke når threshold.
      }
      else{
        g <- gam(round(Abundance,0)~s(DOY, k=4),family=poisson(link = "log"),data=dfsuba)
        pred <- data.frame(DOY = seq(154,238, by=0.1)) # frame of model predictions: day 154 to 238.
        pred$Abundance <- predict(g, newdata = pred, type ="response") # Get predicted values under new values of avgsr and trapdays.
        plot(pred$DOY,pred$Abundance,type="l",col="black",lwd=1,main=j,
             ylim=c(0,1.05*max(c(max(pred$Abundance),max(dfsuba$Abundance)))))
        points(dfsuba$DOY,dfsuba$Abundance,pch=16,cex=1.5)#lukkede symboler der opfylder threshold værdier
      }
    }
  }
  dev.off()
}

#################################################################
# Nu skal vi lave fine plots (WORK IN PROGRESS)
#################################################################

#Her indlæses filer med data der er blevet testet og nu skal der arbejdes videre
dfOPE <- read.csv("OPE_liste.csv")
dfOPE$Year<- as.factor(dfOPE$Year)
df6 <- read.csv("duration.csv", header= TRUE)
df6$Year<- as.factor(df6$Year)
df6sub <- read.csv("duration_subset.csv", header=TRUE)
df6sub$Year<- as.factor(df6sub$Year)

head(dfOPE)

#Boxplot af fænologivariable
ggplot(data=dfOPE, aes(Year,DOY, colour=Pheno.Event)) + ylab("Day of Year") + 
  geom_boxplot() + facet_wrap(~Pheno.Event,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
  

ggplot(data=subset(dfOPE,Pheno.Event=="Peak"),aes(Year,DOY)) + ylab("Day of Year") + geom_boxplot() + 
  facet_wrap(~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(data=df6sub, aes(Year,Duration)) + ylab("Day of Year") + geom_boxplot() + 
  facet_wrap(~SpeciesID,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Her laves et plot for en artsgruppe, så resultaterne kan ses i en figur
ggplot(data=subset(dfOPE,SpeciesID=="Chironomidae"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  geom_line()+geom_point()+ylab("Day of year")+facet_grid(SpeciesID~Plot,scales = "free_y")+theme(axis.text.x = element_text(angle = 90, hjust = 0))

#Her er samme type plot, blot for en anden artsgruppe
ggplot(data=subset(dfOPE,SpeciesID=="Muscidae"&Pheno.Event=="Peak"), aes(as.integer(as.character(Year)),DOY, colour=Plot))+
  geom_line()+geom_point()+ylab("Day of year")+facet_grid(~SpeciesID,scales = "free_y")+theme(axis.text.x = element_text(angle = 90, hjust = 0))
