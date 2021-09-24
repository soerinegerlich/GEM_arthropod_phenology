library(tidyverse)
library(tidyr)
library(RDocumentation)#Denne gør din help() funktion bedre
library(readxl) #Indlæser excel filer, import into R.
library(lubridate) #Beregner datoer og tidspunkter. Det er en toolbox, eller en række tools der hjælper med at fremstille tid/datoer bedre. 
library(mgcv) #funktioner der kan analysere med GAM og generalised additive mixed modeling.
library(MESS) #teste antagelser i GAM, statistiske detaljer.
library(corrplot)
library(stats)
library(ggplot2)



dfOPE <- read.csv("Data/Dataset_for_GAM_NEW/dfOPE_dataframe.csv",sep=",",stringsAsFactors = FALSE, header = TRUE)
#dfOPE$Year<- as.factor(dfOPE$Year)
df8 <- read.csv("Data/Dataset_for_GAM_NEW\\duration.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)
#df8$Year<- as.factor(df8$Year)
df8sub <- read.csv2("Data/Dataset_for_GAM_NEW\\duration_subset.txt",  sep="\t",stringsAsFactors = FALSE, header = TRUE)
#df8sub$Year<- as.factor(df8sub$Year)

df8$End<-as.numeric(df8$End)
df8$Onset<-as.numeric(df8$Onset)
df8$Peak<-as.numeric(df8$Peak)
df8$Duration<-as.numeric(df8$Duration)

print(unique(df8$SpeciesID))

#Linear regression models####
#Første forsøg. Ikke helt rigtig - se hjælp fra Toke nederst
for (i in unique(df8$SpeciesID)){  
  for (j in unique(df8$Plot)){   
    df8a<-subset(df8,SpeciesID==i)   
    df8a<-subset(df8a,Plot==j)   
    mod1<-lm(Onset~Year, data = df8a)
    slopes[[i]][[j]]<-c(summary(mod1)$coefficients[c(2,4,6,8)],summary(mod1)$r.squared,sum(df$n>0))
  }
}

for (i in unique(df8$SpeciesID)){
  print(i)
  for (j in unique(df8$Plot)){
    df8a<-subset(df8,SpeciesID==i)   
    df8a<-subset(df8a,Plot==j)   
    print("plot")
    
    
    for (k in length(df8a$Onset)){
      print('na')
      print(is.na(df8a$Onset[k]))
      
      if(is.na(df8a$Onset[k])){
        df8d<-data.frame(matrix(ncol=7,nrow=0, dimnames=list(NULL, c("Year", "Plot", "SpeciesID", "Onset", "Peak", "End", "Duration"))))
      } 
      else{ 
        mod1 <- lm(Onset ~ Year, data =df8)
        print(summary(mod1))
        
        slopes[[i]][[j]]<-c(summary(mod1)$coefficients[c(2,4,6,8)],summary(mod1)$r.squared,sum(df8a$count),sum(df8a$n>0))
        #slopes<-c(summary(mod1)$coefficients[c(2,4,6,8)],summary(mod1)$r.squared,sum(df$count),sum(df$n>0))
        
      }
    }   
  }
}
  

length(df8$Onset[is.na(df8$Onset)])


####Hjælp fra Toke####
df_summary<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope=numeric(),
                       SD=numeric(),Tvalue=numeric(),Pvalue=numeric(),Rsquare=numeric(),AdjRsquare=numeric(),Count=numeric(),n=numeric())

for (i in unique(df8$SpeciesID)){
  #print(i)
  df8b<-subset(df8,SpeciesID==i)
  pdf(paste("Figures_test",i,".pdf"),width=15,height=10)
  par(mfrow=c(5,10),oma = c(2,2,1,0) ,mar = c(2,1,2,2) + 0.1)
  for (j in unique(df8b$Plot)){
    df8a<-subset(df8b,Plot==j)
    
    if(sum(!is.na(df8a$Onset))<10){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
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
                            n=NA)
    }
    else{
      mod1 <- lm(Onset ~ Year, data =df8a) #Jeg troede det kunne lade sig gøre at bruge sum(na.rm=TRUE) da sande værdier udvælges       
      #pred <- data.frame(Year = seq(1996,2020, by=0.1))
      #pred$Onset <- df8a$Onset
      plot(df8a$Year,df8a$Onset,type="l",col="black",lwd=1,main=j,
      ylim=c(154,238*max(c(max(df8a$Onset,na.rm=TRUE)))))
      points(df8a$Year,df8a$Onset,pch=16,cex=1.5)
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
                          n=sum(!is.na(df8a$Onset)))
      df_summary<-bind_rows(df_summary,df_temp)
    }
    #plot(mod1)
  }
  dev.off()
}
    
    if(sum(!is.na(df8a$Peak))<10){
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
                          n=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Year, data =df8a)        
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
                          n=sum(!is.na(df8a$Peak)))
      df_summary<-bind_rows(df_summary,df_temp)
    }
    
    #plot(mod2)
    
    if(sum(!is.na(df8a$End))<10){
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
                          n=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ Year, data =df8a) 
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
                          n=sum(!is.na(df8a$End)))
      df_summary<-bind_rows(df_summary,df_temp)
      }
    #plot(mod3)
   }
}

write.csv(df_summary, file = "Data/Linear_regression_results\\dfsummary_dataframe.csv", row.names=FALSE)



warnings()

df8%>%
  ggplot(aes(Year,Onset)) + ylab("Onset") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="Acari",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df8%>%
  ggplot(aes(Year,Peak)) + ylab("Peak") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="Acari",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df8%>%
  ggplot(aes(Year,End)) + ylab("End") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="Acari",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))    

df8%>%
  ggplot(aes(Year,Onset)) + ylab("Onset") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="ANMU",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df8%>%
  ggplot(aes(Year,Onset)) + ylab("Onset") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="Collembola",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

df8%>%
  ggplot(aes(Year,Onset)) + ylab("Onset") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="CHCE",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))



####MED LOOP####
#test
df8a<-subset(df8,SpeciesID=="MYSC"&Plot=="Art1")
mod1 <- lm(Onset ~ Year, data =df8a) #Jeg troede det kunne lade sig gøre at bruge sum(na.rm=TRUE) da sande værdier udvælges       
df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1], 
                    Plot=df8a$Plot[1],
                    Pheno_event="Onset",
                    Slope=summary(mod1)$coefficients[2],
                    SD=summary(mod1)$coefficients[4],
                    Tvalue=summary(mod1)$coefficients[6],
                    Pvalue=summary(mod1)$coefficients[8],
                    Rsquare=summary(mod1)$r.squared,
                    AdjRsquare=summary(mod1)$adj.r.squared,
                    Count=sum(df8a$count),
                    n=sum(!is.na(df8a$Onset)))
#####

df_summary1<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope=numeric(),
                       SD=numeric(),Tvalue=numeric(),Pvalue=numeric(),Rsquare=numeric(),AdjRsquare=numeric(),Count=numeric(),n=numeric())

for (i in unique(df8$SpeciesID)){
  #print(i)
  df8b<-subset(df8,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    df8a<-subset(df8b,Plot==j)
    for (k in length(df8a$Onset)){
      #print('na')
      #print(is.na(df8a$Onset[k]))
      
      if(is.na(df8a$Onset[k]>14)){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
        df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                            Plot=df8a$Plot[1],
                            Pheno_event="Onset",
                            Slope=NA,
                            SD=NA,
                            Tvalue=NA,
                            Pvalue=NA,
                            Rsquare=NA,
                            AdjRsquare=NA,
                            Count=NA,
                            n=NA)
      }
      else{
        mod1 <- lm(Onset ~ Year, data =df8a) #Jeg troede det kunne lade sig gøre at bruge sum(na.rm=TRUE) da sande værdier udvælges       
        df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1], 
                            Plot=df8a$Plot[1],
                            Pheno_event="Onset",
                            Slope=summary(mod1)$coefficients[2],
                            SD=summary(mod1)$coefficients[4],
                            Tvalue=summary(mod1)$coefficients[6],
                            Pvalue=summary(mod1)$coefficients[8],
                            Rsquare=summary(mod1)$r.squared,
                            AdjRsquare=summary(mod1)$adj.r.squared,
                            Count=sum(df8a$count),
                            n=sum(!is.na(df8a$Onset>10)))
        df_summary1<-bind_rows(df_summary1,df_temp)
      }


#plot(mod1)
          for (l in length(df8a$Peak)){
if(is.na(df8a$Peak[l]>14)){
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
                      n=NA)
  }
else{ 
  mod2 <- lm(Peak ~ Year, data =df8a)        
  df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                      Plot=df8a$Plot[1],
                      Pheno_event="Peak",
                      Slope=summary(mod2)$coefficients[2],
                      SD=summary(mod2)$coefficients[4],
                      Tvalue=summary(mod2)$coefficients[6],
                      Pvalue=summary(mod2)$coefficients[8],
                      Rsquare=summary(mod2)$r.squared,
                      AdjRsquare=summary(mod2)$adj.r.squared,
                      Count=sum(df8a$count),
                      n=sum(!is.na(df8a$Peak>10)))
  df_summary1<-bind_rows(df_summary1,df_temp)
  }

#plot(mod2)
            for (m in length(df8a$End)){

if(is.na(df8a$End[m]>14)){
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
                      n=NA)
  }
else{ 
  
  mod3 <- lm(End ~ Year, data =df8a)        
  df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                      Plot=df8a$Plot[1],
                      Pheno_event="End",
                      Slope=summary(mod3)$coefficients[2],
                      SD=summary(mod3)$coefficients[4],
                      Tvalue=summary(mod3)$coefficients[6],
                      Pvalue=summary(mod3)$coefficients[8],
                      Rsquare=summary(mod3)$r.squared,
                      AdjRsquare=summary(mod3)$adj.r.squared,
                      Count=sum(df8a$count),
                      n=sum(!is.na(df8a$End>10)))
  df_summary1<-bind_rows(df_summary1,df_temp)
  }
#plot(mod3)
        }
      }
    }
  }
}
