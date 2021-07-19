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

df8sub$End<-as.numeric(df8sub$End)
df8sub$Onset<-as.numeric(df8sub$Onset)
df8sub$Peak<-as.numeric(df8sub$Peak)
df8sub$Duration<-as.numeric(df8sub$Duration)
#df8sub$Year<-as.numeric(df8sub$Year)
#nlevels(df8sub$Year)

df8$End<-as.numeric(df8$End)
df8$Onset<-as.numeric(df8$Onset)
df8$Peak<-as.numeric(df8$Peak)
df8$Duration<-as.numeric(df8$Duration)


#En tom df laves
df8c <- data.frame(matrix(ncol=7,nrow=0, dimnames=list(NULL, c("Year", "Plot", "SpeciesID", "Onset", "Peak", "End", "Duration"))))
df8c$Onset<-as.numeric(df8c$Onset)
df8c$Peak<-as.numeric(df8c$Peak)
df8c$End<-as.numeric(df8c$End)
df8c$Duration<-as.numeric(df8c$Duration)
df8c$Year<-as.integer(df8c$Year)
df8c$Plot<-as.character(df8c$Plot)
df8c$SpeciesID<-as.character(df8c$SpeciesID)

df8d <- bind_rows(df8c,df8)

df8d$Onset<-0
df8d$Peak<-0
df8d$End<-0
df8d$Duration<-0
df8d$Year<-0
df8d$Plot<-0
df8d$SpeciesID<-0

df8$Onset[is.na(df8$Onset)] <- 0
df8$Peak[is.na(df8$Peak)] <- 0
df8$End[is.na(df8$End)] <- 0
df8$Duration[is.na(df8$Duration)] <- 0

#Kode fra Toke####
for (i in unique(df8sub$SpeciesID)){  
  for (j in unique(df8sub$Plot)){   
    df<-subset(df8sub,SpeciesID==i)   
    df<-subset(df,Plot==j)   
    mod1<-lm(((238-154+1)*abundance/trapdays)~year, data = df)   
    slopes[[i]][[j]]<-c(summary(mod1)$coefficients[c(2,4,6,8)],summary(mod1)$r.squared,sum(df$n>0))
                                                                                                                                                                                                      sum(df$count),sum(df$n>0))
}
}


#Linear regression models####
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
  

df_summary<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope=numeric(),
                       SD=numeric(),Tvalue=numeric(),Pvalue=numeric(),Rsquare=numeric(),Count=numeric(),n=numeric())

for (i in unique(df8$SpeciesID)){
  #print(i)
  df8b<-subset(df8,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    df8a<-subset(df8b,Plot==j)
    
    if (j == 'Art1' & i == 'Acari') {
      print(df8a$Onset)
      print(sum(df8a$Onset))
    }
    
    if(is.na(sum(df8a$Onset))){
      #df_temp<-df8a[NA,]
      }
    else{
      mod1 <- lm(Onset ~ Year, data =df8a)        
      df_temp<-data.frame(SpeciesID=df8a$SpeciesID[1],
                          Plot=df8a$Plot[1],
                          Pheno_event="Onset",
                          Slope=summary(mod1)$coefficients[2],
                          SD=summary(mod1)$coefficients[4],
                          Tvalue=summary(mod1)$coefficients[6],
                          Pvalue=summary(mod1)$coefficients[8],
                          Rsquare=summary(mod1)$r.squared,
                          Count=sum(df8a$count),
                          n=sum(df8a$Onset>0))
      df_summary<-bind_rows(df_summary,df_temp)
    }
    
    
    
    if(is.na(sum(df8a$Peak))){
      #df_temp<-df8a[NA,]
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
                          Count=sum(df8a$count),
                          n=sum(df8a$Peak>0))
      df_summary<-bind_rows(df_summary,df_temp)
    }
    if(is.na(sum(df8a$End))){
      #df_temp<-df8a[NA,]
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
                          Count=sum(df8a$count),
                          n=sum(df8a$End>0))
      df_summary<-bind_rows(df_summary,df_temp)
    }
  }
}

#df_summary2<-df_summary

df8%>%
  ggplot(aes(Year,Onset)) + ylab("Onset") + 
  geom_smooth(method="lm")+geom_point()+facet_grid(Plot~SpeciesID=="Acari",scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

    

