library(tidyverse)
library(RDocumentation)#Denne gør din help() funktion bedre
library(readxl) #Indlæser excel filer, import into R.
library(lubridate) #Beregner datoer og tidspunkter. Det er en toolbox, eller en række tools der hjælper med at fremstille tid/datoer bedre. 
library(mgcv) #funktioner der kan analysere med GAM og generalised additive mixed modeling.
library(MESS) #teste antagelser i GAM, statistiske detaljer.
library(corrplot)


#Her indlæses filer med data der er blevet testet og nu skal der arbejdes videre
dfOPE <- read.csv("R_projects/GEM_arthropod_phenology/Data/Dataset_for_GAM/dfOPE_dataframe.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)
dfOPE$Year<- as.factor(dfOPE$Year)
df8 <- read.csv("R_projects/GEM_arthropod_phenology/Data/Dataset_for_GAM/duration.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)
df8$Year<- as.factor(df8$Year)
df8sub <- read.csv2("R_projects/GEM_arthropod_phenology/Data/Dataset_for_GAM/duration_subset.csv",  sep=",",stringsAsFactors = FALSE, header = TRUE)
df8sub$Year<- as.factor(df8sub$Year)

head(dfOPE)

#Boxplot af fænologivariable
ggplot(data=dfOPE, aes(Year,DOY, colour=Pheno.Event)) + ylab("Day of Year") + 
  geom_boxplot() + facet_wrap(~Pheno.Event,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

class(dfOPE$DOY)


####FIGUR FOR ACARI####
df8sub %>% group_by(SpeciesID, Year) -> df8subGroupBy

dfTestAcari1 <- subset(df8subGroupBy,SpeciesID=="Acari")

df8$Peak<-as.numeric(df8$Peak)
df8$End<-as.numeric(df8$End)
df8$Onset<-as.numeric(df8$Onset)
df8sub$Peak<-as.numeric(df8sub$Peak)
df8sub$End<-as.numeric(df8sub$End)
df8sub$Onset<-as.numeric(df8sub$Onset)

colnames(df8sub)[colnames(df8sub) == "Plot"] <- "Plot_no."

###TEST###
ggplot(data=dfTestAcari1, aes(Year,Onset)) + 
  ylab("Day of Year")+ 
  geom_point(shape = 1,size=2)+
  geom_point(data=dfTestAcari1, aes(Year,Peak), shape = 2,size=2)+
  geom_point(data=dfTestAcari1, aes(Year,End),shape = 0,size=2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

###TEST###
ggplot(data=df8sub, aes(Year,Onset, color = Plot_no.)) + 
  ylab("Day of Year")+ 
  geom_point(shape = 1,size=2)+
  geom_point(data=df8sub, aes(Year,Peak, color = Plot_no.), shape = 2,size=2)+
  geom_point(data=df8sub, aes(Year,End, color = Plot_no.),shape = 0,size=2)+
  facet_wrap(~SpeciesID,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#####ARTHROPOD TAXA SEASON DYNAMICS PLOT#####
dfOPEsub <- subset(dfOPE, dfOPE$DOY!= "NA")
colnames(dfOPEsub)[colnames(dfOPEsub) == "Plot"] <- "Plot_no."

q <- ggplot(data=dfOPEsub, aes(x=Year,y=DOY, color=Plot_no., shape=Pheno.Event)) + 
  ylab("Day of Year")+ 
  geom_point()+
  facet_wrap(~SpeciesID,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  ggtitle("Arthropod taxa caught through the season in trapping plots for 1996 - 2019")
q + theme(plot.title = element_text(angle = 360, hjust = 0.5))


####FIGUR FOR ACARI MED MEAN PLOT####
ggplot(data=dfTestAcari, aes(Year,Onset)) + 
  ylab("Day of Year") + 
  geom_point() + 
  #geom_errorbar(
  #data=dfOPE2,
  #aes(
  # mean(DOY),
  #ymin=mean(DOY)-sd(DOY),
  #ymax=mean(DOY)-sd(DOY)))+
  geom_point(data=dfTestAcari, aes(Year,Peak),colour='red',size=1.5)+
  geom_point(data=dfTestAcari, aes(Year,End),colour='blue',size=1.5)+
  geom_errorbar(data=dfTestAcari, aes(Year,Onset,ymin=Onset-OnsetSD,ymax=Onset+OnsetSD), colour = 'black', width = 0.4)+
  geom_errorbar(data=dfTestAcari, aes(Year,Peak,ymin=Peak-PeakSD,ymax=Peak+PeakSD), colour = 'red', width = 0.4)+
  geom_errorbar(data=dfTestAcari, aes(Year,End,ymin=End-EndSD,ymax=End+EndSD), colour = 'blue', width = 0.4)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#####ARTHROPOD TAXA SEASON DYNAMICS WITH MEAN PLOT#####
p <- ggplot(data=dfTest, aes(Year,Onset)) + 
  ylab("Day of Year") + 
  geom_point() + 
  #geom_errorbar(
  #data=dfOPE2,
  #aes(
  # mean(DOY),
  #ymin=mean(DOY)-sd(DOY),
  #ymax=mean(DOY)-sd(DOY)))+
  geom_point(data=dfTest, aes(Year,Peak),colour='red',size=1.5)+
  geom_point(data=dfTest, aes(Year,End),colour='blue',size=1.5)+
  geom_errorbar(data=dfTest, aes(Year,Onset,ymin=Onset-OnsetSD,ymax=Onset+OnsetSD), colour = 'black', width = 0.4)+
  geom_errorbar(data=dfTest, aes(Year,Peak,ymin=Peak-PeakSD,ymax=Peak+PeakSD), colour = 'red', width = 0.4)+
  geom_errorbar(data=dfTest, aes(Year,End,ymin=End-EndSD,ymax=End+EndSD), colour = 'blue', width = 0.4)+
  facet_wrap(~SpeciesID,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  ggtitle("Arthropod phenological events from 1996 to 2019")
p + theme(plot.title = element_text(angle = 360, hjust = 0.5))


#3/6/2021
###PREPARATION OF DATA###
#df8subGroupBy <- group_by(df8sub, SpeciesID, Year)
df8sub %>% group_by(SpeciesID, Year) -> df8subGroupBy
df8subGroupBy$Peak<-as.numeric(df8subGroupBy$Peak)
df8subGroupBy$End<-as.numeric(df8subGroupBy$End)
df8subGroupBy$Onset<-as.numeric(df8subGroupBy$Onset)
dfTest <- summarise(df8subGroupBy, OnsetSD = sd(Onset), Onset = mean(Onset), PeakSD = sd(Peak), Peak = mean(Peak), EndSD = sd(End), End = mean(End))


dfTestAcari <- subset(dfTest,SpeciesID=="Acari")

# TEST SOERINE
createPdfForSpecies <- function(data, species){
  dataBySpecies <- subset(data,SpeciesID==species)
  
  speciesPlot <- ggplot(data=dataBySpecies, aes(Year,DOY, colour=Pheno.Event)) + ylab("Day of Year") + 
    geom_boxplot() + facet_wrap(~Pheno.Event,scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) + ggtitle(species)
  
  #pdf(paste("Data/OPE_figures_by_species\\",species,".pdf", sep=""),width=10,height=6)
  #print(speciesPlot)
  #dev.off()
}

for (species in unique(df8sub$SpeciesID)){
  print(species)
  createPdfForSpecies(dfOPE, species)
}


colnames(dfOPE)[colnames(dfOPE) == "Plot"] <- "Plot_no"

#Her laves et plot for en artsgruppe, saa resultaterne kan ses i en figur
#Plot for Acari
Acari <- ggplot(data=subset(dfOPEsub,SpeciesID=="Acari"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  #facet_wrap(~Plot_no.,scales = "free_y")+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Acari", sep=""),width=10,height=6)
print(Acari)
dev.off()

#Plot for Agromyzidae
Agromyzidae <- ggplot(data=subset(dfOPEsub,SpeciesID=="Agromyzidae"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  #facet_wrap(~Plot_no.,scales = "free_y")+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Agromyzidae.pdf", sep=""),width=10,height=6)
print(Agromyzidae)
dev.off()

#Plot for ANMU
ANMU <- ggplot(data=subset(dfOPEsub,SpeciesID=="ANMU"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","ANMU.pdf", sep=""),width=10,height=6)
print(ANMU)
dev.off()

#Plot for Aphidoidea
Aphidoidea <- ggplot(data=subset(dfOPEsub,SpeciesID=="Aphidoidea"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Aphidoidea.pdf", sep=""),width=10,height=6)
print(Aphidoidea)
dev.off()

#Plot for Braconidae
Braconidae <- ggplot(data=subset(dfOPEsub,SpeciesID=="Braconidae"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Braconidae.pdf", sep=""),width=10,height=6)
print(Braconidae)
dev.off()

#Plot for Calliphoridae
Calliphoridae <- ggplot(data=subset(dfOPEsub,SpeciesID=="Calliphoridae"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Calliphoridae.pdf", sep=""),width=10,height=6)
print(Calliphoridae)
dev.off()

#Plot for Cecidomyiidae
Cecidomyiidae <- ggplot(data=subset(dfOPEsub,SpeciesID=="Cecidomyiidae"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Cecidomyiidae.pdf", sep=""),width=10,height=6)
print(Cecidomyiidae)
dev.off()

#Plot for Chalcidoidea
Chalcidoidea <- ggplot(data=subset(dfOPEsub,SpeciesID=="Chalcidoidea"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","Chalcidoidea.pdf", sep=""),width=10,height=6)
print(Chalcidoidea)
dev.off()

#Plot for CHCE
CHCE <- ggplot(data=subset(dfOPEsub,SpeciesID=="CHCE"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("Data/Figures_for_presentation\\","CHCE.pdf", sep=""),width=10,height=6)
print(CHCE)
dev.off()

#Plot for Coccoidea
Coccoidea <- ggplot(data=subset(dfOPEsub,SpeciesID=="Coccoidea"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("R_projects/GEM_arthropod_phenology/Data/Figures_for_presentation\\","Coccoidea.pdf", sep=""),width=10,height=6)
print(Coccoidea)
dev.off()

#Plot for Collembola
Collembola <- ggplot(data=subset(dfOPEsub,SpeciesID=="Collembola"), aes(as.integer(as.character(Year)),DOY, colour=Pheno.Event))+
  ylab("Day of year")+
  xlab("Year")+
  geom_line()+
  geom_point()+
  facet_grid(SpeciesID~Plot_no.,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
pdf(paste("R_projects/GEM_arthropod_phenology/Data/Figures_for_presentation\\","Collembola.pdf", sep=""),width=10,height=6)
print(Collembola)
dev.off()


#Denne figur viser for én taxa, et pheno.event med alle plots.
ggplot(data=subset(dfOPEsub,SpeciesID=="Acari"&Pheno.Event=="Peak"), aes(as.integer(as.character(Year)),DOY, colour=Plot_no.))+
  geom_line()+
  geom_point()+
  ylab("Day of year")+
  facet_grid(~SpeciesID,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))


