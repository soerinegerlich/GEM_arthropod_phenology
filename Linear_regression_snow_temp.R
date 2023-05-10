#Meta analysis using the metafor package in R

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gvlma) #For assessing linear model assumptions. 
library(car)
library(writexl)
library(readxl)
library(lme4)
library(lmerTest)
#install.packages("rsq")
library(metafor)
library(rsq)
#install.packages("forestplot")
library(forestplot)
library(ggpubr)
library(scales)

df_phen_event <- read.csv("Data/Piecewise_regression_temperature/piecewise_temp.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)
df_phen_event_air <- read.csv("Data/Piecewise_regression_temperature/piecewise_temp_air.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)

df8 <- read.csv("Data/Dataset_for_GAM_NEW\\duration.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)

dfsnowmelt_climatestation <- readxl::read_xlsx("Data/Climate_data_Zackenberg/Snowmelt_climatestation.xlsx")


#Match climate variables with phen. event data to compile them in the same dataframe
df_phen_event$Snowmelt <- dfsnowmelt_climatestation$DOY[match(paste0(df_phen_event$Year),
                                                              paste0(dfsnowmelt_climatestation$Year))]

df_phen_event_air$Snowmelt <- dfsnowmelt_climatestation$DOY[match(paste0(df_phen_event_air$Year),
                                                              paste0(dfsnowmelt_climatestation$Year))]

#write.csv(df_phen_event_air, file="Data/Dataset_for_GAM_NEW\\df_phen_event_air_snow.csv", row.names = FALSE)

df_summary_all_air<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope1=numeric(),Slope2=numeric(),
                            SE1=numeric(),SE2=numeric(),Tvalue1=numeric(),Tvalue2=numeric(),Pvalue1=numeric(),Pvalue2=numeric(),
                            Count=numeric(),n=numeric(),AIC=numeric(),Rsquared=numeric(), 
                            Residual=numeric(),CI_lwr1=numeric(), CI_upr1=numeric(),CI_lwr2=numeric(),CI_upr2=numeric(),
                            vif_snow=numeric(), vif_temp=numeric())

for (i in unique(df_phen_event_air$SpeciesID)){
  print(i)
  df8b<-subset(df_phen_event_air,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    print(j)
    df8a<-subset(df8b,Plot==j)
    df8sub<-subset(df8a,!is.na(Onset)&!is.na(Onset_Temp))
    
    if(length(df8sub$Year)<5){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
      #print(sum(is.na(df8$Onset)))
      #print(sum(!is.na(df8$Onset)))
      #length(df8$Onset[is.na(df8$Onset)]) #Denne kode viser alle værdier af ikke-NA værdier!!
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                          Plot=df8sub$Plot[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                          Pheno_event="Onset",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    else{ 
      mod1 <- lm(Onset ~ Snowmelt+Onset_Temp, data=df8sub)       
      AIC <- AIC(mod1)
      R.mod1 <- rsq(mod1, adj = TRUE)
      Residual1 <- sqrt(deviance(mod1)/df.residual(mod1))
      CI <- confint(mod1, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], 
                          Plot=df8sub$Plot[1],
                          Pheno_event="Onset",
                          Slope1=summary(mod1)$coefficients[2],
                          Slope2=summary(mod1)$coefficients[3],
                          SE1=summary(mod1)$coefficients[5],
                          SE2=summary(mod1)$coefficients[6],
                          Tvalue1=summary(mod1)$coefficients[8],
                          Tvalue2=summary(mod1)$coefficients[9],
                          Pvalue1=summary(mod1)$coefficients[11],
                          Pvalue2=summary(mod1)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC,
                          Rsquared=R.mod1,
                          Residual=Residual1,
                          CI_lwr1=CI[2],
                          CI_upr1=CI[5],
                          CI_lwr2=CI[3],
                          CI_upr2=CI[6],
                          vif_snow=car::vif(mod1)[1],
                          vif_temp=car::vif(mod1)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    #plot(mod1)
    #}
    #} 
    
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Snowmelt+Peak_Temp, data=df8sub)
      AIC2 <- AIC(mod1)
      R.mod2 <- rsq(mod2, adj = TRUE)
      Residual2 <- sqrt(deviance(mod2)/df.residual(mod2))
      CI2 <- confint(mod2, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope1=summary(mod2)$coefficients[2],
                          Slope2=summary(mod2)$coefficients[3],
                          SE1=summary(mod2)$coefficients[5],
                          SE2=summary(mod2)$coefficients[6],
                          Tvalue1=summary(mod2)$coefficients[8],
                          Tvalue2=summary(mod2)$coefficients[9],
                          Pvalue1=summary(mod2)$coefficients[11],
                          Pvalue2=summary(mod2)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Peak)),
                          AIC=AIC2,
                          Rsquared=R.mod2,
                          Residual=Residual2,
                          CI_lwr1=CI2[2],
                          CI_upr1=CI2[5],
                          CI_lwr2=CI2[3],
                          CI_upr2=CI2[6],
                          vif_snow=car::vif(mod2)[1],
                          vif_temp=car::vif(mod2)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    
    #plot(mod2)
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ Snowmelt+End_Temp, data=df8sub)
      AIC3 <- AIC(mod3)
      R.mod3 <- rsq(mod3, adj = TRUE)
      Residual3 <- sqrt(deviance(mod3)/df.residual(mod3))
      CI3 <- confint(mod3, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope1=summary(mod3)$coefficients[2],
                          Slope2=summary(mod3)$coefficients[3],
                          SE1=summary(mod3)$coefficients[5],
                          SE2=summary(mod3)$coefficients[6],
                          Tvalue1=summary(mod3)$coefficients[8],
                          Tvalue2=summary(mod3)$coefficients[9],
                          Pvalue1=summary(mod3)$coefficients[11],
                          Pvalue2=summary(mod3)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$End)),
                          AIC=AIC3,
                          Rsquared=R.mod3,
                          Residual=Residual3,
                          CI_lwr1=CI3[2],
                          CI_upr1=CI3[5],
                          CI_lwr2=CI3[3],
                          CI_upr2=CI3[6],
                          vif_snow=car::vif(mod3)[1],
                          vif_temp=car::vif(mod3)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
    #plot(mod3)
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Duration",
                          Slope1=NA,
                          Slope2=NA,
                          SE1=NA,
                          SE2=NA,
                          Tvalue1=NA,
                          Tvalue2=NA,
                          Pvalue1=NA,
                          Pvalue2=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr1=NA,
                          CI_upr1=NA,
                          CI_lwr2=NA,
                          CI_upr2=NA,
                          vif_snow=NA,
                          vif_temp=NA)
    }
    
    else{ 
      
      mod4 <- lm(Duration ~ Snowmelt+Onset_Temp, data=df8sub)
      AIC4 <- AIC(mod4)
      R.mod4 <- rsq(mod4, adj = TRUE)
      Residual4 <- sqrt(deviance(mod4)/df.residual(mod4))
      CI4 <- confint(mod4, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Duration",
                          Slope1=summary(mod4)$coefficients[2],
                          Slope2=summary(mod4)$coefficients[3],
                          SE1=summary(mod4)$coefficients[5],
                          SE2=summary(mod4)$coefficients[6],
                          Tvalue1=summary(mod4)$coefficients[8],
                          Tvalue2=summary(mod4)$coefficients[9],
                          Pvalue1=summary(mod4)$coefficients[11],
                          Pvalue2=summary(mod4)$coefficients[12],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Duration)),
                          AIC=AIC4,
                          Rsquared=R.mod4,
                          Residual=Residual4,
                          CI_lwr1=CI4[2],
                          CI_upr1=CI4[5],
                          CI_lwr2=CI4[3],
                          CI_upr2=CI4[6],
                          vif_snow=car::vif(mod1)[1],
                          vif_temp=car::vif(mod1)[2])
      df_summary_all_air<-bind_rows(df_summary_all_air,df_temp)
    }
  }
}

write_xlsx(df_summary_all_air, "Data/Climate_pheno\\df_summary_all_air.xlsx", col_names = TRUE)
write_xlsx(df_summary_all, "Data/Climate_pheno\\df_summary_all.xlsx", col_names = TRUE)

df_summary_all_air %>%
  mutate(Order = case_when(
    SpeciesID == "Acari" ~ "Decomposer",
    SpeciesID == "ANMU" ~ "Diptera",
    SpeciesID == "Aphidoidea" ~ "Hemiptera",
    SpeciesID == "Chalcidoidea" ~ "Diptera",
    SpeciesID == "CHCE" ~ "Diptera",
    SpeciesID == "Coccoidea" ~ "Hemiptera",
    SpeciesID == "Collembola" ~ "Decomposer",
    SpeciesID == "Culicidae" ~ "Diptera",
    SpeciesID == "Ichneumonidae" ~ "Hymenoptera",
    SpeciesID == "Linyphiidae" ~ "Aranea",
    SpeciesID == "Lycosidae" ~ "Aranea",
    SpeciesID == "MYSC" ~ "Diptera",
    SpeciesID == "Nymphalidae" ~ "Lepidoptera",
    SpeciesID == "Phoridae" ~ "Diptera",
    SpeciesID == "Scathophagidae" ~ "Diptera",
    SpeciesID == "Thomisidae" ~ "Aranea")) -> df_summary_all_air


df_summary_all_air %>%
  mutate(Habitat = case_when(
    Plot == "Art1" ~ "Pond",
    Plot == "Art2" ~ "Wet fen",
    Plot == "Art3" ~ "Mesic heath",
    Plot == "Art4" ~ "Mesic heath",
    Plot == "Art5" ~ "Arid heath",
    Plot == "Art6" ~ "Snow-bed",
    Plot == "Art7" ~ "Arid heath")) -> df_summary_all_air

df_summary_all_air$Plot[df_summary_all_air$Plot == "Art1"] <- "Plot 1" 
df_summary_all_air$Plot[df_summary_all_air$Plot == "Art2"] <- "Plot 2" 
df_summary_all_air$Plot[df_summary_all_air$Plot == "Art3"] <- "Plot 3" 
df_summary_all_air$Plot[df_summary_all_air$Plot == "Art4"] <- "Plot 4" 
df_summary_all_air$Plot[df_summary_all_air$Plot == "Art5"] <- "Plot 5" 
df_summary_all_air$Plot[df_summary_all_air$Plot == "Art6"] <- "Plot 6"
df_summary_all_air$Plot[df_summary_all_air$Plot == "Art7"] <- "Plot 7" 


df_summary_all_air$SpeciesID[df_summary_all_air$SpeciesID == "CHCE"] <- "Chironomidae"
df_summary_all_air$SpeciesID[df_summary_all_air$SpeciesID == "ANMU"] <- "Muscidae"
df_summary_all_air$SpeciesID[df_summary_all_air$SpeciesID == "MYSC"] <- "Sciaridae"

#df_summary_all <- subset(df_summary_all, df_summary_all$Plot!= "Plot 1"&df_summary_all$Order!="Decomposer")
df_summary_all_air <- df_summary_all_air[!(df_summary_all_air$Plot == "Plot 1" & (df_summary_all_air$Order == "Decomposer")),]
df_summary_all_air <- df_summary_all_air[!(df_summary_all_air$Plot == "Plot 6" & (df_summary_all_air$SpeciesID == "Linyphiidae")),]

df_Onset <- subset(df_summary_all_air, Pheno_event == "Onset")
df_Peak <- subset(df_summary_all_air, Pheno_event == "Peak")
df_End <- subset(df_summary_all_air, Pheno_event == "End")
df_Duration <- subset(df_summary_all_air, Pheno_event == "Duration")


df_Onset$SpeciesID <- factor(df_Onset$SpeciesID,                 # Relevel group factor
                             levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                        "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_Peak$SpeciesID <- factor(df_Peak$SpeciesID,                 # Relevel group factor
                            levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                       "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_End$SpeciesID <- factor(df_Peak$SpeciesID,                 # Relevel group factor
                           levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                      "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_Duration$SpeciesID <- factor(df_Duration$SpeciesID,                 # Relevel group factor
                                levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                           "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_Onset$Habitat <- factor(df_Onset$Habitat,                 # Relevel group factor
                          levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond", "Snow-bed"))

df_Peak$Habitat <- factor(df_Peak$Habitat,                 # Relevel group factor
                            levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond", "Snow-bed"))

df_Duration$Habitat <- factor(df_Duration$Habitat,                 # Relevel group factor
                          levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond", "Snow-bed"))

df_End$Habitat <- factor(df_End$Habitat,                 # Relevel group factor
                          levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond", "Snow-bed"))

 

#Try with forest plot and CI

Onset_temp <- ggplot(df_Onset, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("")+
  ylab("")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), position = position_dodge(width = 0.5), alpha = 0.5)+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        panel.border = element_rect(color = "black", fill = NA, size = 2))


Peak_temp <- ggplot(df_Peak, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("")+
  ylab("")+
  #xlim(c(-15,10))+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), position = position_dodge(width = 0.8), alpha = 0.5)+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        panel.border = element_rect(color = "black", fill = NA, size = 2))


End_temp <- ggplot(df_End, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Phenological sensitivity (days per °C)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


Duration_temp <- ggplot(df_Duration, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Phenological sensitivity (days per °C)")+
  ylab("")+
 #xlim(c(-15,10))+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.8))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))

ggarrange(Onset_temp, Peak_temp, End_temp, Duration_temp, labels = c("A. Onset", "B. Peak", "C. End", "D. Duration"), hjust = 0, vjust = 0.1, ncol = 1, nrow = 4, legend = "none")+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm")) 
ggarrange(Peak_temp, Duration_temp, labels = c("A) Peak", "B) Duration"), hjust = 0, vjust = 0.3, ncol = 1, nrow = 2, legend = "none", font.label = list(size = 18))+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))

ggarrange(Peak_temp,                             # First row 
          ggarrange(Duration_temp, labels = c("(b) Duration"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Peak", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                                      # Labels of the scatter plot
)+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))

ggarrange(Onset_temp,                             # First row 
          ggarrange(End_temp, labels = c("(b) End"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Onset", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                                      # Labels of the scatter plot
)+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))


Onset <- ggplot(df_Onset, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("")+
  ylab("")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15, color = "black"), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))

Peak <- ggplot(df_Peak, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15, color = "black"), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))

End <- ggplot(df_End, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Phenological sensitivity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0, color = "black"), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = -0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


Duration <- ggplot(df_Duration, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Phenological sensitivity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0, color = "black"), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = -0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


ggarrange(Peak,                             # First row 
          ggarrange(Duration, labels = c("(b) Duration"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Peak", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                                      # Labels of the scatter plot
)+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))

ggarrange(Onset,                             # First row 
          ggarrange(End, labels = c("(b) End"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Onset", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                         # Labels of the scatter plot
)+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))


