###
#REMOVED ONLY VS DISPERSED SEEDS BY AGOUTIS
###

###Packages------
library(tidyverse)
library(fitdistrplus)

## Loading and preparing data-----
main.agouti <- read.csv2("dados/Tidy agouti2.csv")

#filtering only species with information about seed weight
rvd <- main.agouti %>% filter(Seed.weight..g.>0) 

#separating dispersed and removed non-dispersed species
rvd$DispTF <- grepl("x",rvd$Dispersal) ## TRUE for cells tha contains 'x'

#withdrawing species that agouti only eat the pulp
rvd <- rvd %>% filter(Predation!="N")

# filtering duplicate species
rvd <- rvd %>%  group_by(Species) %>% 
  summarise(sw=max(Seed.weight..g.), DispTF=max(DispTF))  
rvdd <- rvd %>% filter(DispTF==TRUE)
rvdr <- rvd %>% filter(DispTF==FALSE)

#withdrawing species that agouti only eat the pulp

### seeing if data is normal----

hist(rvdd$sw)
hist(log10(rvdd$sw)) 
descdist((rvdd$sw), boot = 5000)
descdist(log10(rvdd$sw), boot = 5000)
shapiro.test((rvdd$sw)) ## shapiro test show normality when values are above 0.05
shapiro.test(log10(rvdd$sw))
   #normal when log transformed

hist(rvdr$sw)
hist(log10(rvdr$sw)) 
descdist((rvdr$sw), boot = 5000)
descdist(log10(rvdr$sw), boot = 5000)
shapiro.test((rvdr$sw)) ## shapiro test show normality when values are above 0.05
shapiro.test(log10(rvdr$sw)) 
   # normal when log transformed

## log 10 transformation was required
rvdr$log10sw <- log10(rvdr$sw)
rvdd$log10sw <- log10(rvdd$sw)

####  Comparissions tests-----
#comparing using t-test
a <- t.test(rvdd$log10sw, rvdr$log10sw )

#Wilcox test just in case
wilcox.test(rvdd$log10sw, rvdr$log10sw)
b <- wilcox.test(rvdd$sw, rvdr$sw,  conf.level =0.95, conf.int = T) #non-normal data
   ## same result with and without log transformed because wilcox uses ranks not values

#### Ploting----
## histogram and density curve
rvd$DispTF <- as.factor(rvd$DispTF) 
rvd$log10sw <- log10(rvd$sw)
med <- rvd %>% group_by(DispTF) %>% summarise(meanlog10sw=mean(log10sw),
                                              medianlog10sw=median(log10sw))
rvd$Disp <-  ifelse(rvd$DispTF==1, "Dispersed", "No evidence of dispersal")

ggplot(data=rvd, aes(x=log10sw, fill=DispTF)) +
  geom_histogram(aes(y=..density..,fill=DispTF), col="grey60", position="dodge", alpha=0.8, bins = 20)+
  geom_density(lwd=1,alpha=0.7, adjust=0.85 )+
  geom_vline(data=med, aes(xintercept=medianlog10sw),color=c("#2E668A", "#15401E"),
             linetype="longdash", lwd=1.3)+
  geom_rug(aes(y=0.1, col=DispTF), lwd=1.4,  sides="b")+
  theme_classic()+
  scale_x_continuous(breaks=c(-2,-1,0,1,2), labels=c(0.001, 0.01, 1, 10, 100))+
  xlab("Seed weight (g)")+
  ylab("Density probability")+
  scale_color_manual(values=c("#80B6D9", "#0CDE37"), guide=FALSE)+
  scale_fill_manual(values=c("#80B6D9", "#0CDE37"),name="Agouti-seed interaction",
                    labels=c("No evidence\nof dispersal", "Dispersal"))+
  theme(legend.position=c(.25, .8))+
  guides(fill = guide_legend(reverse = TRUE))

## Mean and median values of seed weight for removed and dispersed species----
10^med$meanlog10sw  
10^med$medianlog10sw
