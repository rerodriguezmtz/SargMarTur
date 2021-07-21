rm(list=ls(all=TRUE))
Sys.setenv(TZ='GMT')
setwd("~/GitHub/SargMarTur")

#Read Data
Tor <- read.csv("MarTurt.csv", header=T) 

#Load Packages
library(asbio)
library(coin)
library(ggplot2)
library(ggpubr)
library(plyr) 
library(tidyr) 
library(simpleboot)
library(stats)

str(Tor)

#Transform data
Tor$Site=as.factor(Tor$Site)
Tor$Camp=factor(Tor$Camp, levels = c("Contoy Is", "Cancun", "Palace-Cun", "Tamul",
                                     "Punta Venado", "Paamul", "Aventuras DIF", 
                                     "Chemuyil", "Xcacel", "Xel-Ha","Punta Sur", 
                                     "Pta Cadena","Tankah", "Kanzul", "Cahpechen-Lirios", 
                                     "Yu-Yum", "San Juan"))
Tor$Season=as.factor(Tor$Season)
Tor$Variable=as.factor(Tor$Variable)
Tor$SarVolMax=as.factor(Tor$SarVolMax)
Tor$Period=factor(Tor$Period, levels = c("BS", "AS"))

#Convert from wide to long format
Tor_long <- gather(Tor, Species, N, Cm:Cc) 
Tor_long<-na.omit(Tor_long)
Tor_long$Species=factor(Tor_long$Species, levels = c("Cm", "Cc"))

#Add density column (N/km)
Tor_long$Nkm<-((Tor_long$N)/Tor_long$BeachExt)

# Obtain total and mean  of nests and hatches per period for each species
TorTot<- ddply(Tor_long,.(Variable, Species, Period), summarise, N=sum(N), 
         mean = round(mean (Nkm), 3))
TorTot

# Obtain total and mean of nests and hatches per camp
TorTot2<- ddply(Tor_long,.(Variable,Camp, Species, Period), summarise, N=sum(N), 
          meanKm = round(mean (Nkm), 2))
TorTot2

# Nests per Camp per species per Season (before and after sargasso)
TorNest<- Tor_long[Tor_long$Variable=="Nests",]
TorNestN<- ddply(TorNest, c("Species", "Season"), summarise, N=sum(N), 
                 meanKm = round(mean (Nkm), 2)) 
TorNestN

#Test for normality and homogeneity of variances
lm1<-lm(TorNest$Nkm~TorNest$Season)
modlevene.test(residuals(lm1),TorNest$Season)

##Performs the Shapiro-Wilk test of normality.
shapiro.test(TorNest$Nkm)

##Hatchlings
TorHat<- Tor_long[Tor_long$Variable=="Hatchlings",]
TorHatN<- ddply(TorHat, c("Species", "Season"), summarise, N=sum(N), 
                meanKm = round(mean (Nkm), 2)) 
TorHatN

# Obtain 95% CI mean (95% bootstrap)
#For Nests
CmNest<-TorNest[TorNest$Species=="Cm",]
CmNestBS<-CmNest[CmNest$Period=="BS",]
CmNestAS<-CmNest[CmNest$Period=="AS",]

CcNest<-TorNest[TorNest$Species=="Cc",]
CcNestBS<-CcNest[CcNest$Period=="BS",]
CcNestAS<-CcNest[CcNest$Period=="AS",]

#For hatchlings
CmHat<-TorHat[TorHat$Species=="Cm",]
CmHatBS<-CmHat[CmHat$Period=="BS",]
CmHatAS<-CmHat[CmHat$Period=="AS",]

CcHat<-TorHat[TorHat$Species=="Cc",]
CcHatBS<-CcHat[CcHat$Period=="BS",]
CcHatAS<-CcHat[CcHat$Period=="AS",]

a1 <- simpleboot::one.boot(CmNestBS$Nkm, mean, R=1000) #change species and variable
a1
a <- boot::boot.ci(a1, type="basic") 
a

# Comparison betwen periods for each camp (change the number of the site)
CmNestCamp<-CmNest[CmNest$Site=="1",] # Nests C. mydas
wilcox_test(Nkm~Period, data=CmNestCamp)

CcNestCamp<-CcNest[CcNest$Site=="1",] # Nests C. caretta
wilcox_test(Nkm~Period, data=CcNestCamp)

CmHatCamp<-CmHat[CmHat$Site=="1",] # Hatchlings C. mydas
wilcox_test(Nkm~Period, data=CmHatCamp)

CcHatCamp<-CcHat[CcHat$Site=="1",] # Hatchlings C. caretta
wilcox_test(Nkm~Period, data=CcHatCamp)

# Plot nests and hatchlings per camp BS vs AS
ggerrorplot(Tor_long, x = "Camp", y = "Nkm", desc_stat = "mean_ci", color = "Period",
            palette="lancet", size=0.7)+ 
    facet_wrap(Species~Variable, ncol=2, scales = 'free')+
    xlab("Camp")+ ylab(expression(paste("N ", ~Km^-1*"")))+
    scale_y_log10()+
    theme_grey(base_size=12)+ theme(axis.text.x = element_text(angle=90), legend.position = "none")
