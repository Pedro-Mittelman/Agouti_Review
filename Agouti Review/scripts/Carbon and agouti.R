### AGOUTI AND CARBON STORAGE#####

# Packages required----
library(tidyverse)
library(fitdistrplus)
library(DHARMa)

## Reading and preparing data----

main.agouti <- read.csv2("dados/Tidy agouti2.csv")

## transforming dispersal in TRUE or FALSE
main.agouti$DispTF <- grepl("x",main.agouti$Dispersal) ## TRUE for cells tha contains 'x'

###correcting dispersal for the same species
# once a species is demonstraded to be dispersed by agoutis in one study
# it is always considered as a 'dispersed species'
wd <- main.agouti %>% group_by(Species) %>%
  summarise(DispTF=as.factor(max(DispTF)), Peso.s= mean(Seed.weight..g., na.rm=T),
            Wdcm3=mean(Wood..density.g.cm³, na.rm=T), 
            ts=mean(Tree.size.max, na.rm=T))

## filtering only species with wood density and tree height
# and creating a column with a proxy of carbon storage ( wood density * tree height)
wdts <- wd %>% filter(ts>0 & Wdcm3>0 & Peso.s>0) %>% mutate(Carbon=ts*Wdcm3)

# Differetiating preyed and dispersed species
# preyed species are those that were seeing as being exclusevely preyed
# or that weighs less than 0,8 gram (the minimum weigh of a dispersed species)
preyed <- main.agouti %>% group_by(Species) %>% summarise(DispTF2=max(DispTF))
preyed <- inner_join(preyed, main.agouti)  %>% filter(Predation=="x", DispTF2==0) %>% 
  distinct(Species, .keep_all=T)
wdts$DispTF2 <- as.character(wdts$DispTF)
wdts$DispTF2 <- if_else(wdts$DispTF2==1,"Dispersed", "Unknown")
wdts$DispTF2[which(wdts$Species %in% preyed$Species)] <- "Eaten"
wdts$DispTF2[which(wdts$Peso.s < 0.8)] <- "Eaten"
wdts$DispTF2 <- as.factor(wdts$DispTF2)



### Model---------
## Creating linear model for log seed weight and the Carbon proxy

model.all <- lm(data=wdts, Carbon~log10(Peso.s ))
summary(model.all)
DHARMa::testResiduals(model.all)
DHARMa::simulateResiduals(fittedModel = model.all, plot = T)
# no over or under dispersion, one outlier but nothing major, no heteroscedascity


#model for dispersed species only 
model.disp <- lm(data=filter(wdts,DispTF==1),log10(Peso.s)~Carbon)
summary(model.disp)
DHARMa::testResiduals(model.disp)
DHARMa::simulateResiduals(fittedModel = model.disp, plot = T)
# no over or under dispersion, no outliers , no heteroscedascity

## Ploting-----
## GRAPH

### median and mean values of Carbon proxy for preyed and dispersed species

med3 <- wdts %>% group_by(DispTF2) %>% summarise(meanCarbon=mean(Carbon),
                                                 medianCarbon=median(Carbon)) %>% 
filter (DispTF2!="Unknown")

#data frame with only dispersed species to generate a regression line in the graph
wdtsdisp <-  wdts %>% filter (DispTF2=="Dispersed")

ggplot(data=wdts, aes(x=log10(Peso.s), y=Carbon))+
  geom_hline(data=med3, aes(yintercept=meanCarbon),color=c( "#3ED65E","#FF9595"),
             linetype="longdash", lwd=1.1, alpha=0.9)+
  geom_point(size=3, aes(col=DispTF2))+
  geom_smooth(data=wdtsdisp, method = "lm", aes(col=DispTF2), show.legend = F,
              alpha=0.1, lwd=1.4)+
  geom_smooth(method = "lm", col="grey30", alpha=0.12,lwd=1.3)+
  xlab("Seed weigh (g)")+
  #geom_rug(data=wdtsdispeaten, aes(x=-0.1, col=DispTF2), lwd=1.3,  sides="l", alpha=0.75,
           #show.legend=FALSE)+
  ylab("Species carbon storage\n Wood density x Tree size")+
  scale_x_continuous(breaks=c(-1,0,1), labels=c(0.01, 1, 10))+
  scale_color_manual(values=c( "#00AC24", "#FF6C6C","#79ACCD"),name="Agouti-plant interactions",
                     labels=c( "Dispersal", "Predation", "Unknown"))+
  guides (colour = guide_legend(override.aes = list(size=3.5)))+
  theme_classic()+
  theme(legend.position=c(0.25, 0.78))


### Comparing dispersed and removed seeds in terms of carbon storage ----
# separating preyed and dispersed species
wdcr <- wdts %>% filter(DispTF2=="Eaten")
wdcd <- wdts %>% filter(DispTF2=="Dispersed")

## seeing if data is normal with and without log transformation
hist(wdcd$Carbon)
hist(log10(wdcd$Carbon), breaks = 12) 
descdist((wdcd$Carbon), boot = 500)
descdist(log10(wdcd$Carbon), boot = 500)
shapiro.test((wdcd$Carbon)) ## shapiro test show normality when values are above 0.05
shapiro.test(log10(wdcd$Carbon))
qqnorm(y = wdcd$Carbon);qqline(y = wdcd$Carbon)
#normal, does not nedd any tranformation


hist(wdcr$Carbon, breaks = 10)
hist(log10(wdcr$Carbon)) 
descdist((wdcr$Carbon), boot = 500)
descdist(log10(wdcr$Carbon), boot = 500)
shapiro.test((wdcr$Carbon)) ## shapiro test show normality when values are above 0.05
shapiro.test(log10(wdcr$Carbon)) 
qqnorm(y = wdcr$Carbon);qqline(y = wdcr$Carbon)
#normal, does not nedd any tranformation

## comparing preyed and dispersed speceis carbon storage with t-test

t.test(wdcr$Carbon, wdcd$Carbon)

# a non parametrical test just in case
wilcox.test(wdcr$Carbon, wdcd$Carbon)


