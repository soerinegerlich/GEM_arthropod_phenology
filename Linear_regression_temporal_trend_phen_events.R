library(tidyverse)
library(readxl) #Indlæser excel filer, import into R.
library(corrplot)
library(stats)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
library(splitstackshape)
#install.packages("splitstackshape")
#install.packages("agricolae")
library(agricolae)
library(lme4)
library(lmerTest)
library(car)

df8 <- read.csv("Data/Dataset_for_GAM_NEW\\duration.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)

df8$End<-as.numeric(df8$End)
df8$Onset<-as.numeric(df8$Onset)
df8$Peak<-as.numeric(df8$Peak)
df8$Duration<-as.numeric(df8$Duration)

df8 %>%
  mutate(Order = case_when(
    SpeciesID == "Acari" ~ "Decomposer",
    SpeciesID == "ANMU" ~ "Pollinator",
    SpeciesID == "Aphidoidea" ~ "Herbivor",
    SpeciesID == "Chalcidoidea" ~ "Parasitoid",
    SpeciesID == "CHCE" ~ "Pollinator",
    SpeciesID == "Coccoidea" ~ "Herbivor",
    SpeciesID == "Collembola" ~ "Decomposer",
    SpeciesID == "Culicidae" ~ "Pollinator",
    SpeciesID == "Ichneumonidae" ~ "Parasitoid",
    SpeciesID == "Linyphiidae" ~ "Predator",
    SpeciesID == "Lycosidae" ~ "Predator",
    SpeciesID == "MYSC" ~ "Pollinator",
    SpeciesID == "Nymphalidae" ~ "Pollinator",
    SpeciesID == "Phoridae" ~ "Pollinator",
    SpeciesID == "Scathophagidae" ~ "Pollinator",
    SpeciesID == "Thomisidae" ~ "Predator")) -> df8

df8$Order[is.na(df8$Order)] <- "NA"
df8<- subset(df8,Order!="NA")

print(unique(df8$SpeciesID))

df8 <- subset(df8, df8$Plot!= "Art1" | df8$SpeciesID!= "Collembola")
df8 <- subset(df8, df8$Plot!= "Art1" | df8$SpeciesID!= "Acari")

#Full LMM investigating changes in community phenology across years
model <- lmer(Onset ~ Year + (1 | SpeciesID:Plot), data=df8)
summary(model)
anova(model)
print(LSD.test(model,"Order"))
confint(model, level = 0.95)

model <- lmer(Peak ~ Year + (1 | SpeciesID:Plot), data=df8)
summary(model)
anova(model)
print(LSD.test(model,"Order"))
confint(model, level = 0.95)

ggplot(df8, aes(Year, Peak))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

model <- lmer(End ~ Year + (1 | SpeciesID:Plot), data=df8)
summary(model)
anova(model)

#Analysis of variance to see if changes in phenology across years is different
# among functional groups and plots

model <- lmer(Onset ~ Year + Plot + Order + Year:Plot + Year:Order + (1 | Order:SpeciesID), data=df8)
summary(model)
anova(model)

model <- lmer(Peak ~ Year + Plot + Order + Year:Plot + Year:Order + (1 | Order:SpeciesID), data=df8)
summary(model)
anova(model)

model <- lmer(End ~ Year + Plot + Order + Year:Plot + Year:Order + (1 | Order:SpeciesID), data=df8)
summary(model)
anova(model)

model <- lmer(Duration ~ Year + Plot + Order + Year:Plot + Year:Order + (1 | Order:SpeciesID), data=df8)
summary(model)
anova(model)

#Analysis of variance to see if changes in phenology across years is different
# among arthropod taxa and plots

model <- lm(Onset ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df8)
summary(model)
anova(model)

model <- lm(Peak ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df8)
summary(model)
anova(model)

model2 <- lm(End ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df8)
summary(model2)
anova(model2)

model3 <- lm(Duration ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df8)
summary(model3)
anova(model3)

df_sub <- subset(df8, Order == "Pollinator" )
df_sub1 <- subset(df8, Order == "Predator" )
df_sub2 <- subset(df8, Order == "Herbivor" )
df_sub3 <- subset(df8, Order == "Parasitoid" )
df_sub4 <- subset(df8, Order == "Decomposer" )

df_sub %>%
  mutate(Emerge = case_when(
    SpeciesID == "ANMU" ~ "Late",
    SpeciesID == "CHCE" ~ "Early",
    SpeciesID == "Culicidae" ~ "Late",
    SpeciesID == "MYSC" ~ "Early",
    SpeciesID == "Nymphalidae" ~ "Late",
    SpeciesID == "Phoridae" ~ "Late",
    SpeciesID == "Scathophagidae" ~ "Late")) -> df_sub

model <- lmer(Peak ~ Year + (1 | SpeciesID:Plot), data=df_sub)
summary(model)
anova(model)
#print(LSD.test(model,c("Year","SpeciesID"))) #For interactions, but not possible with continues variable
df_Art2 <- subset(df_sub, Plot == "Art2" )
model <- lmer(Peak ~ Year + (1 | SpeciesID), data=df_Art2)
summary(model)
anova(model)
confint(model, level = 0.95)

model <- lm(Peak ~ Year + Plot + Emerge + Year:Plot + Year:Emerge, data=df_sub)
summary(model)
anova(model)


model <- lmer(Peak ~ Year + (1 | Plot:SpeciesID), data=df_sub)
summary(model)
anova(model)
confint(model, level = 0.95)

model <- lm(Duration ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df_sub1)
summary(model)
anova(model)
confint(model, lvel = 0.95)

model <- lm(Duration ~ Year, data=df_sub1)
summary(model)
anova(model)


model <- lm(Duration ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df_sub2)
summary(model)
anova(model)

model <- lm(Duration ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df_sub3)
summary(model)
anova(model)

model <- lm(Peak ~ Year + Plot + SpeciesID + Year:Plot + Year:SpeciesID, data=df_sub4)
summary(model)
anova(model)


df_summary_lm<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope=numeric(),
                          SD=numeric(),Tvalue=numeric(),Pvalue=numeric(),Rsquare=numeric(),AdjRsquare=numeric(),
                          Count=numeric(),n=numeric(),Residual=numeric())

for (i in unique(df8$SpeciesID)){
  print(i)
  df8b<-subset(df8,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    df8a<-subset(df8b,Plot==j)
    
    if(sum(!is.na(df8a$Onset))<6){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
      #print(sum(is.na(df8$Onset)))
      #print(sum(!is.na(df8$Onset)))
      #length(df8$Onset[is.na(df8$Onset)]) #Denne kode viser alle værdier af ikke-NA værdier!!
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                          Plot=df8a$Plot[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                          Pheno_event="Onset",
                          Slope=NA,
                          SD=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Rsquare=NA,
                          AdjRsquare=NA,
                          Count=NA,
                          n=NA,
                          Residual=NA)
    }
    else{
      mod1 <- lm(Onset ~ Year, data =df8a)       
      Residual1 <- sqrt(deviance(mod1)/df.residual(mod1))
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1], 
                          Plot=df8a$Plot[1],
                          Pheno_event="Onset",
                          Slope=summary(mod1)$coefficients[2],
                          SD=summary(mod1)$coefficients[4],
                          Tvalue=summary(mod1)$coefficients[6],
                          Pvalue=summary(mod1)$coefficients[8],
                          Rsquare=summary(mod1)$r.squared,
                          AdjRsquare=summary(mod1)$adj.r.squared,
                          Count=sum(df8a$TotalAbundance),
                          n=sum(!is.na(df8a$Onset)),
                          Residual=Residual1)
      df_summary_lm<-bind_rows(df_summary_lm,df_temp)
    }
    #plot(mod1)
    
    
    if(sum(!is.na(df8a$Peak))<6){
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="Peak",
                          Slope=NA,
                          SD=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Rsquare=NA,
                          AdjRsquare=NA,
                          Count=NA,
                          n=NA,
                          Residual=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Year, data =df8a)
      Residual2 <- sqrt(deviance(mod2)/df.residual(mod2))
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="Peak",
                          Slope=summary(mod2)$coefficients[2],
                          SD=summary(mod2)$coefficients[4],
                          Tvalue=summary(mod2)$coefficients[6],
                          Pvalue=summary(mod2)$coefficients[8],
                          Rsquare=summary(mod2)$r.squared,
                          AdjRsquare=summary(mod2)$adj.r.squared,
                          Count=sum(df8a$TotalAbundance),
                          n=sum(!is.na(df8a$Peak)),
                          Residual=Residual2)
      df_summary_lm<-bind_rows(df_summary_lm,df_temp)
    }
    
    #plot(mod2)
    
    if(sum(!is.na(df8a$End))<6){
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="End",
                          Slope=NA,
                          SD=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Rsquare=NA,
                          AdjRsquare=NA,
                          Count=NA,
                          n=NA,
                          Residual=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ Year, data =df8a)
      Residual3 <- sqrt(deviance(mod3)/df.residual(mod3))
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="End",
                          Slope=summary(mod3)$coefficients[2],
                          SD=summary(mod3)$coefficients[4],
                          Tvalue=summary(mod3)$coefficients[6],
                          Pvalue=summary(mod3)$coefficients[8],
                          Rsquare=summary(mod3)$r.squared,
                          AdjRsquare=summary(mod3)$adj.r.squared,
                          Count=sum(df8a$TotalAbundance),
                          n=sum(!is.na(df8a$End)),
                          Residual=Residual3)
      df_summary_lm<-bind_rows(df_summary_lm,df_temp)
    }
    
    if(sum(!is.na(df8a$Duration))<6){
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="Duration",
                          Slope=NA,
                          SD=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Rsquare=NA,
                          AdjRsquare=NA,
                          Count=NA,
                          n=NA,
                          Residual=NA)
    }
    
    else{ 
      
      mod4 <- lm(Duration ~ Year, data =df8a)
      Residual4 <- sqrt(deviance(mod4)/df.residual(mod4))
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="Duration",
                          Slope=summary(mod4)$coefficients[2],
                          SD=summary(mod4)$coefficients[4],
                          Tvalue=summary(mod4)$coefficients[6],
                          Pvalue=summary(mod4)$coefficients[8],
                          Rsquare=summary(mod4)$r.squared,
                          AdjRsquare=summary(mod4)$adj.r.squared,
                          Count=sum(df8a$TotalAbundance),
                          n=sum(!is.na(df8a$Duration)),
                          Residual=Residual4)
      df_summary_lm<-bind_rows(df_summary_lm,df_temp)
    }
  }
}

df_Peak <- filter(df_summary_lm, Pheno_event == 'Peak')
df_Duration <- filter(df_summary_lm, Pheno_event == 'Duration')

df_summary_lm %>%
  #subset(Pvalue < 0.05)%>%
  ggplot(aes(Plot, Residual, color = Pheno_event))+
  geom_point()+
  facet_wrap(~SpeciesID)

####Attempt to create new figure with interval slopes####

df_model_significance <-
  data.frame(
    SpeciesID = character(),
    Plot = character(),
    Pheno_event = character(),
    Slope = numeric(),
    Pvalue = numeric(),
    AdjRsquare = numeric(),
    Slope_interval = character()
  )

for (species in unique(df_summary_lm$SpeciesID)) {
  print(species)
  df_summary_lm_sub <- subset(df_summary_lm, SpeciesID == species)
  for (plot in unique(df_summary_lm_sub$Plot)) {
    print(plot)
    df_summary_lm_sub2 <- subset(df_summary_lm_sub, Plot == plot)
    for (phenoEvent in unique(df_summary_lm_sub2$Pheno_event)) {
      df_summary_lm_sub3 <-
        subset(df_summary_lm_sub2, Pheno_event == phenoEvent)
      
      Significance_result <- NA
      
      if (df_summary_lm_sub3$Slope < -1.5) {
        Significance_result <-
          "-2.0"
      } else if (df_summary_lm_sub3$Slope > -1.5 && df_summary_lm_sub3$Slope < -1) {
        Significance_result <-
          "-1.5"
      } else if (df_summary_lm_sub3$Slope > -1 && df_summary_lm_sub3$Slope < -0.5) {
        Significance_result <-
          "-1.0"
      } else if (df_summary_lm_sub3$Slope > -0.5 && df_summary_lm_sub3$Slope < -0.1) {
        Significance_result <-
          "-0.5"
      } else if (df_summary_lm_sub3$Slope > -0.1 && df_summary_lm_sub3$Slope < 0.1) {
        Significance_result <-
          "0.0"
      } else if (df_summary_lm_sub3$Slope > 0.1 && df_summary_lm_sub3$Slope < 0.5) {
        Significance_result <-
          "0.5"
      } else if (df_summary_lm_sub3$Slope > 0.5 && df_summary_lm_sub3$Slope < 1) {
        Significance_result <-
          "1.0"
      } else if (df_summary_lm_sub3$Slope > 1 && df_summary_lm_sub3$Slope < 1.5) {
        Significance_result <-
          "1.5"
      } else if (df_summary_lm_sub3$Slope > 1.5) {
        Significance_result <-
          "2.0"
      } else {
        print("No pvalues")
        Significance_result <- "Not determined"
      }
      
      
      df_temp <-
        data.frame(
          SpeciesID = species,
          Plot = plot,
          Pheno_event = phenoEvent,
          Slope = df_summary_lm_sub3$Slope,
          Pvalue = df_summary_lm_sub3$Pvalue,
          AdjRsquare = df_summary_lm_sub3$AdjRsquare,
          Slope_interval = Significance_result
        )
      
      df_model_significance <- bind_rows(df_model_significance, df_temp)
    }
  }
}

df_model_significance$Number <- 1

df_model_significance %>%
  group_by(Pheno_event, Slope_interval)%>%
  summarise(Sum = sum(Number)) -> df_test

unique(df_model_significance$Slope_interval)

df_test$Slope_interval[is.na(df_test$Slope_interval)] <- "1" 

df_test$Slope_interval <- factor(df_test$Slope_interval,                 # Relevel group factor
                             levels = c("-2.0", "-1.5","-1.0","-0.5","0.0",
                                        "0.5","1.0","1.5","2.0"))

df_test$Pheno_event <- factor(df_test$Pheno_event,                 # Relevel group factor
                                 levels = c("Onset", "Peak", "End", "Duration"))

df_Duration <- filter(df_test, Pheno_event == 'Duration')
df_test <- subset(df_test, !Pheno_event == 'Duration')

myLoc <- 
  (which(levels(df_test$Slope_interval) == "(-0.05,0.05]"))


p1 +
  geom_vline(aes(xintercept = myLoc))

df_test %>%
  #subset(Pvalue < 0.05)%>%
  ggplot(aes(Slope_interval, Sum, fill = Pheno_event))+
  geom_bar(stat = "identity")+
  facet_wrap(~Pheno_event)+
  xlab("Phenological shift (days/yr)")+
  ylab("Number of taxa x plot")+
  scale_fill_brewer(palette="Set2")+
  geom_vline(xintercept = 0.0)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        legend.position = "none", axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(vjust = -5), axis.title.y = element_text(vjust = 2),
        plot.margin = unit(c(0.5,0.1,0.5,0.1), "cm"))

df_Duration %>%
  #subset(Pvalue < 0.05)%>%
  ggplot(aes(Slope_interval, Sum, fill = Pheno_event))+
  geom_bar(stat = "identity")+
  facet_wrap(~Pheno_event)+
  xlab("Phenological shift (days/yr)")+
  ylab("Number of taxa x plot")+
  scale_fill_brewer(palette="BrBG")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        legend.position = "none", axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(vjust = -5), axis.title.y = element_text(vjust = 2),
        plot.margin = unit(c(0.5,3,0.5,3), "cm"))



####Analysis of results####

df_model_significance <-
  data.frame(
    SpeciesID = character(),
    Plot = character(),
    Pheno_event = character(),
    Slope = numeric(),
    Pvalue = numeric(),
    AdjRsquare = numeric(),
    Significance = character()
  )

for (species in unique(df_summary_lm$SpeciesID)) {
  print(species)
  df_summary_lm_sub <- subset(df_summary_lm, SpeciesID == species)
  for (plot in unique(df_summary_lm_sub$Plot)) {
    print(plot)
    df_summary_lm_sub2 <- subset(df_summary_lm_sub, Plot == plot)
    for (phenoEvent in unique(df_summary_lm_sub2$Pheno_event)) {
      df_summary_lm_sub3 <-
        subset(df_summary_lm_sub2, Pheno_event == phenoEvent)
      
      Significance_result <- NA
      
      if (df_summary_lm_sub3$Pvalue < 0.05 || df_summary_lm_sub3$Pvalue == 0.05) {
        Significance_result <-
          "Significant"
      } else if (df_summary_lm_sub3$Pvalue > 0.05 && df_summary_lm_sub3$Pvalue < 0.06) {
        Significance_result <-
          "Near_significant"
      } else if (df_summary_lm_sub3$Pvalue > 0.05) {
        Significance_result <-
          "Non-significant"
      } else {
        print("No pvalues")
        Significance_result <- "Not determined"
      }
      
      
      df_temp <-
        data.frame(
          SpeciesID = species,
          Plot = plot,
          Pheno_event = phenoEvent,
          Slope = df_summary_lm_sub3$Slope,
          Pvalue = df_summary_lm_sub3$Pvalue,
          AdjRsquare = df_summary_lm_sub3$AdjRsquare,
          Significance = Significance_result
        )
      
      df_model_significance <- bind_rows(df_model_significance, df_temp)
    }
  }
}


print(unique(df_model_significance$SpeciesID))

df1 <- filter(df_model_significance, Significance == 'Significant')
df1a <- filter(df_model_significance, Significance == 'Non-significant')
df2 <- filter(df1, Slope > 0)
df3 <- filter(df1, Slope < 0)
df4 <- filter(df1, Pheno_event == "Onset")
df5 <- filter(df1, Pheno_event == "Peak")
df6 <- filter(df1, Pheno_event == "End")
df7 <- filter(df_model_significance, Pheno_event == "Duration")
df9 <- filter (df1, Pheno_event == "Duration")
df10 <- filter(df1a, Pheno_event == "Duration")
df11 <- filter(df_model_significance, Pheno_event == "Onset")

df8 %>%
  mutate(Habitat = case_when(
    Plot == "Art1" ~ "Wet fen",
    Plot == "Art2" ~ "Wet fen",
    Plot == "Art3" ~ "Mesic heath",
    Plot == "Art4" ~ "Mesic heath",
    Plot == "Art5" ~ "Arid heath",
    Plot == "Art6" ~ "Mesic heath",
    Plot == "Art7" ~ "Arid heath")) -> df8

df7 <- filter(df1, Habitat == "Wet fen")
df8 <- filter(df1, Habitat == "Mesic heath")
df9 <- filter(df1, Habitat == "Arid heath")



#install.packages("writexl")
library(writexl)

write_xlsx(df1, "Data/Linear_regression\\df_summary_significance.xlsx", col_names = TRUE)
write_xlsx(df1a, "Data/Linear_regression\\df_summary_nonsignificance.xlsx", col_names = TRUE)
write_xlsx(df2, "Data/Linear_regression\\df_summary_significance_positive_slope.xlsx", col_names = TRUE)
write_xlsx(df3, "Data/Linear_regression\\df_summary_significance_negative_slope.xlsx", col_names = TRUE)


####CREATING FIGURE####

#df_Acari <- subset(df_summary_lm,SpeciesID=="Acari")

#Need to add new new column with order
unique(df_summary_lm$SpeciesID)

df_summary_lm %>%
  mutate(Order = case_when(
           SpeciesID == "Acari" ~ "Decomposer",
           SpeciesID == "ANMU" ~ "Diptera",
           SpeciesID == "Aphidoidea" ~ "Hemiptera",
           SpeciesID == "Chalcidoidea" ~ "Hymenoptera",
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
           SpeciesID == "Thomisidae" ~ "Aranea")) -> df_summary_lm

df_summary_lm$Plot[df_summary_lm$Plot == "Art1"] <- "Plot 1" 
df_summary_lm$Plot[df_summary_lm$Plot == "Art2"] <- "Plot 2" 
df_summary_lm$Plot[df_summary_lm$Plot == "Art3"] <- "Plot 3" 
df_summary_lm$Plot[df_summary_lm$Plot == "Art4"] <- "Plot 4" 
df_summary_lm$Plot[df_summary_lm$Plot == "Art5"] <- "Plot 5" 
df_summary_lm$Plot[df_summary_lm$Plot == "Art6"] <- "Plot 6"
df_summary_lm$Plot[df_summary_lm$Plot == "Art7"] <- "Plot 7" 

df_summary_lm$SpeciesID[df_summary_lm$SpeciesID == "CHCE"] <- "Chironomidae"
df_summary_lm$SpeciesID[df_summary_lm$SpeciesID == "ANMU"] <- "Muscidae"
df_summary_lm$SpeciesID[df_summary_lm$SpeciesID == "MYSC"] <- "Sciaridae"

df_summary_lm$Pheno_event <- factor(df_summary_lm$Pheno_event,                 # Relevel group factor
                         levels = c("Onset", "Peak", "End", "Duration"))

df_summary_lm$SpeciesID <- factor(df_summary_lm$SpeciesID,                 # Relevel group factor
                            levels = c("Chironomidae", "Linyphiidae", "Sciaridae", "Lycosidae", "Collembola", "Muscidae", 
                                       "Acari", "Thomisidae", "Culicidae", "Ichneumonidae", "Coccoidea", "Nymphalidae", "Phoridae", "Chalcidoidea", "Aphidoidea"))

df_summary_lm$Plot <- factor(df_summary_lm$Plot,                 # Relevel group factor
                       levels = c("Plot 7", "Plot 6", "Plot 5", "Plot 4", "Plot 3", "Plot 2", "Plot 1"))



#colnames(df_summary_lm) <- c("SpeciesID", "Plot", "Phenological event", "Slope", "SD", "Tvalue", "Pvalue", "Rsquare", "AdjRsquare", "Count", "n", "Order")

ggplot(df_summary_lm, aes(x = Slope, y = Plot, color = Pvalue < 0.051 | Pvalue == 0.5, shape = Pheno_event))+
  xlab("Phenological shift (days per year)")+
  scale_color_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  geom_point(size = 3)+facet_grid(Order + SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_text(angle = 0), panel.background = element_rect(fill = "white"), panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), axis.title.y = element_blank())+
  geom_vline(xintercept = 0)


df8_Onset<- subset(df_summary_lm,Pheno_event=="Onset")
df8_Peak<- subset(df_summary_lm,Pheno_event=="Peak")
df8_End<- subset(df_summary_lm,Pheno_event=="End")
df8_Duration<- subset(df_summary_lm,Pheno_event=="Duration")



Onset <- ggplot(df8_Onset, aes(x = Slope, y = Plot, color = Pvalue < 0.051 | Pvalue == 0.5))+
  xlab("Phenological shift (days per year)")+
  scale_color_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  geom_point(size = 3)+
  geom_errorbar(aes(xmin=Slope-SD, xmax=Slope+SD, y = Plot))+
  facet_grid(SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_blank(), panel.background = element_rect(fill = "white"), panel.spacing = unit(.08, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), axis.title.y = element_blank(),legend.position="none",
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.1,1,0.1), "cm"))+
  geom_vline(xintercept = 0)

Onset

Peak <- ggplot(df8_Peak, aes(x = Slope, y = Plot, color = Pvalue < 0.051 | Pvalue == 0.5))+
  xlab("Phenological shift (days per year)")+
  scale_color_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  geom_point(size = 3)+
  geom_errorbar(aes(xmin=Slope-SD, xmax=Slope+SD, y = Plot))+
  facet_grid(SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_blank(), panel.background = element_rect(fill = "white"), panel.spacing = unit(.08, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none",
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.1,1,0.1), "cm"))+
  geom_vline(xintercept = 0)

Peak

End <- ggplot(df8_End, aes(x = Slope, y = Plot, color = Pvalue < 0.051 | Pvalue == 0.5))+
  xlab("Phenological shift (days per year)")+
  scale_color_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  geom_point(size = 3)+
  geom_errorbar(aes(xmin=Slope-SD, xmax=Slope+SD, y = Plot))+
  facet_grid(SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_blank(), panel.background = element_rect(fill = "white"), panel.spacing = unit(0.08, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none",
        axis.title.x = element_text(hjust = -1), plot.margin = unit(c(0.5,0.1,0.5,0.1), "cm"))+
  geom_vline(xintercept = 0)

End

Duration <- ggplot(df8_Duration, aes(x = Slope, y = Plot, color = Pvalue < 0.051 | Pvalue == 0.5))+
  xlab("Phenological shift (days per year)")+
  scale_color_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  geom_point(size = 3)+
  geom_errorbar(aes(xmin=Slope-SD, xmax=Slope+SD, y = Plot))+
  facet_grid(SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_blank(), panel.background = element_rect(fill = "white"), panel.spacing = unit(.08, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none",
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.1,1,0.1), "cm"))+
  geom_vline(xintercept = 0)

Duration

ggarrange(Onset, Peak, End, Duration, labels = c("A) Onset", "B) Peak", "C) End", "D) Duration"), hjust = -0.1, vjust = 0.1, ncol = 4, nrow = 1)+
  theme(plot.margin = margin(0.5,0.1,0.1,0.1, "cm"))

