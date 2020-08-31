### COMPARING LARGE FRUGIVORES AND AGOUTIS
# LARGE FRUGIVORES are Atheles, Brachyteles and Tapirus genera

###packages----
library(tidyverse)
library(gridExtra) # plotting graphs together
library(grid) # plotting graphs together

### Reading and preparing data----
#agouti interactions
main.agouti <- read.csv2("dados/Tidy agouti2.csv")
#frugivores interactions
big.fru <- read.csv2("dados/bigfru3updated.csv")
#selecting  columns for analysis and keeping only distinct lines
big.fru <- big.fru %>% dplyr::select(UpdatedPlantSpecies, Frugivore.Genus,
                               Seed.weigh.g, Wood.density.g.cm3,
                              Tree.size.max.m) %>%  
  distinct() %>% mutate(Species=UpdatedPlantSpecies, DispTFR="TRUE")

## Modifying agouti interactions to merge with frugivores
#pointing out dispersed species
main.agouti$DispTF <- grepl("x",main.agouti$Dispersal) ## TRUE for cells tha contains 'x'
## species that were disperse in one study are always considered dispersed
S.disp <- main.agouti %>% group_by(Species) %>%
  summarise(DispTFR= max(DispTF))
main.agouti <- inner_join(main.agouti,S.disp)
main.agouti$DispTFR <- as.logical(main.agouti$DispTFR) 
agouti.disp <- main.agouti  %>% distinct(Species, .keep_all=TRUE)
#selecting necessary columns
agouti.disp <- agouti.disp %>% dplyr::select(Species,Seed.weight..g.,
                                      Wood..density.g.cm³, Tree.size.max, DispTFR)

#change columns names to match dataset of large frugivores
agouti.disp2 <- agouti.disp %>% 
transmute(Species=Species, Frugivore.Genus="Dasyprocta",
            Wood.density.g.cm3=Wood..density.g.cm³,
         Seed.weigh.g=Seed.weight..g., Tree.size.max.m=Tree.size.max,
         DispTFR=DispTFR)

#merging data frames of agoutis and large frugivore
big.fru.ag<- rbind(agouti.disp2,
                   dplyr::select(big.fru,-UpdatedPlantSpecies))

## withdrawing very small outliers seeds from the anlisys
big.fru.ag.sw <- big.fru.ag %>% filter(Seed.weigh.g>0.009) %>% 
  mutate(log10sw=log10(Seed.weigh.g))

## histogram and density curve and test

# table with information about plant species seed weight consumed by each genus

stat.sw <- big.fru.ag.sw %>% group_by(Frugivore.Genus) %>% summarise(meansw=mean(Seed.weigh.g, na.rm = T),
                                              mediansw=median(Seed.weigh.g, na.rm = T),
                                              maxsw=max(Seed.weigh.g, na.rm = T),
                                              meanlog10sw=mean(log10sw, na.rm = T),
                                              medianlog10sw=median(log10sw, na.rm = T),
                                              maxlog10sw=max(log10sw, na.rm = T),
                                              ME=log10(meansw),
                                              nsw=n());stat.sw
## kruskal wallis test comparing genera
kruskal.test(data=big.fru.ag.sw, Seed.weigh.g~Frugivore.Genus)
#post hoc dunn test with pp-vlaue correction for each comparision
FSA::dunnTest(data=big.fru.ag.sw, Seed.weigh.g~Frugivore.Genus, method="bh")

stat.sw$Frugivore.Genus <- factor(
  stat.sw$Frugivore.Genus, levels=c("Ateles","Brachyteles","Tapirus","Dasyprocta"))

## Plotting------
### resampling species for each genus to make a motther graph

Ateles1000<- sample_n(filter(big.fru.ag.sw, Frugivore.Genus=="Ateles"), 10000, replace = T)
Brachyteles1000<- sample_n(filter(big.fru.ag.sw, Frugivore.Genus=="Brachyteles"), 10000, replace = T)
Dasyprocta1000<- sample_n(filter(big.fru.ag.sw, Frugivore.Genus=="Dasyprocta"), 10000, replace = T)
Tapirus1000<- sample_n(filter(big.fru.ag.sw, Frugivore.Genus=="Tapirus"), 10000, replace = T)

big.fru.ag.sw.40000 <- rbind(Ateles1000,Brachyteles1000,Dasyprocta1000,Tapirus1000) 

big.fru.ag.sw.40000$Frugivore.Genus <- factor(
  big.fru.ag.sw.40000$Frugivore.Genus, levels=c("Ateles","Brachyteles","Tapirus","Dasyprocta"))

#plotting
ggplot(data=big.fru.ag.sw.40000, aes(x=jitter(log10sw,700), fill=Frugivore.Genus)) +
  geom_histogram(aes(y=..density..,fill=Frugivore.Genus), col="grey60", position="dodge", alpha=0.8, bins=50)+
  geom_density(aes(alpha=Frugivore.Genus),lwd=1, adjust=1.5)+
  geom_vline(data=stat.sw, aes(xintercept=meanlog10sw,color=Frugivore.Genus),
             linetype="longdash", lwd=1.5)+
  geom_rug(data=big.fru.ag.sw, aes(x=log10sw ,y=0.1, col=Frugivore.Genus), lwd=1,  sides="b")+
  theme_classic()+
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2), labels=c("0.001",0.01, 0.1, 1, 10, 100))+
  xlab("Seed weigh (g)")+
  ylab("Density probability")+
  scale_alpha_manual(values = c(1,0.9,0.8,0.7),guide=FALSE)+
  scale_color_manual(values=c("#B95F2B","#228CCC","#F3F354", "#00A522"),
                     guide=F)+
  scale_fill_manual(values=c("#B95F2B","#228CCC","#F3F354", "#00A522"),
                    name="Frugivore Genus")+
  theme(legend.position=c(.15, .8))


## plotting separately for each species
 Ateles.plot <- ggplot(data=filter(big.fru.ag.sw.40000,Frugivore.Genus=="Ateles"), aes(x=jitter(log10sw,100)))+
   coord_cartesian(xlim = c(-2, 2), ylim=c(0,1.1))+
  geom_histogram(aes(y=..density..),fill="#B95F2B", col="grey60", position="dodge", alpha=0.8, bins=50)+
  geom_density(lwd=1, adjust=1.5, fill="#B95F2B", alpha=0.7)+
  geom_vline(data=filter(stat.sw,Frugivore.Genus=="Ateles"), aes(xintercept=medianlog10sw),
             linetype="longdash", lwd=1.5)+
  geom_rug(data=filter(big.fru.ag.sw,Frugivore.Genus=="Ateles"), aes(x=log10sw ,y=0.1),
           lwd=1,  sides="b", col="#B95F2B")+
  theme_classic()+
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2), labels=c("0.001",0.01, 0.1, 1, 10, 100))+
    xlab(" ")+
  ylab("Density probability")
   #theme(axis.title.x =element_blank())

 Brachyteles.plot <- ggplot(data=filter(big.fru.ag.sw.40000,Frugivore.Genus=="Brachyteles"), aes(x=jitter(log10sw,100)))+
   coord_cartesian(xlim = c(-2, 2), ylim=c(0,1.1))+
   geom_histogram(aes(y=..density..),fill="#228CCC", col="grey60", position="dodge", alpha=0.8, bins=50)+
   geom_density(lwd=1, adjust=1.5, fill="#228CCC", alpha=0.7)+
   geom_vline(data=filter(stat.sw,Frugivore.Genus=="Brachyteles"), aes(xintercept=medianlog10sw),
              linetype="longdash", lwd=1.5)+
   geom_rug(data=filter(big.fru.ag.sw,Frugivore.Genus=="Brachyteles"), aes(x=log10sw ,y=0.1),
            lwd=1,  sides="b", col="#228CCC")+
   theme_classic()+
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2), labels=c("0.001",0.01, 0.1, 1, 10, 100))+
    xlab(" ")+
   ylab("Density probability")
   #theme(axis.title.x =element_blank())
 
 Tapirus.plot <- ggplot(data=filter(big.fru.ag.sw.40000,Frugivore.Genus=="Tapirus"), aes(x=jitter(log10sw,100)))+
   coord_cartesian(xlim = c(-2, 2), ylim=c(0,1.1))+
   geom_histogram(aes(y=..density..),fill="#F3F354", col="grey60", position="dodge", alpha=0.8, bins=50)+
   geom_density(lwd=1, adjust=1.5, fill="#F3F354", alpha=0.7)+
   geom_vline(data=filter(stat.sw,Frugivore.Genus=="Tapirus"), aes(xintercept=medianlog10sw),
              linetype="longdash", lwd=1.5)+
   geom_rug(data=filter(big.fru.ag.sw,Frugivore.Genus=="Tapirus"), aes(x=log10sw ,y=0.1),
            lwd=1,  sides="b", col="#F3F354")+
   theme_classic()+
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2), labels=c("0.001",0.01, 0.1, 1, 10, 100))+
    xlab(" ")+
   ylab("Density probability")
  #theme(axis.title.x =element_blank())
#  
 
 ##obtainiginformation about mean and median seed weight of dispersed species only for Dasyprocta genus
 Dasyprocta.1000.dispersed<- Dasyprocta1000 %>% filter(DispTFR==TRUE) %>% 
   summarise(medianlog10sw=median(log10sw), meanlog10sw=mean(log10sw))

 Dasyprocta.plot <- ggplot(data=filter(big.fru.ag.sw.40000,Frugivore.Genus=="Dasyprocta"), aes(x=jitter(log10sw,100)))+
   coord_cartesian(xlim = c(-2, 2), ylim=c(0,1.1))+
   geom_histogram(aes(y=..density..),fill="#00A522", col="grey60", position="dodge", alpha=0.8, bins=50)+
   geom_density(aes(fill=DispTFR, alpha=DispTFR),lwd=1, adjust=1.5)+
    scale_alpha_manual(values = c(0.8,0.7),guide=FALSE)+
   scale_fill_manual(values=c("#80B6D9", "#0CDE37"),name="",
                     labels=c("No evidence\nof dispersal", "Dispersed"))+
   #geom_density(lwd=1, adjust=1.5, fill="#00A522", alpha=0.7,trim=T)+
   geom_vline(data=filter(stat.sw,Frugivore.Genus=="Dasyprocta"), aes(xintercept=medianlog10sw),
              linetype="longdash", lwd=1.5)+
   # geom_vline(data=Dasyprocta.1000.dispersed, aes(xintercept=medianlog10sw),
   #            linetype="longdash", lwd=1.5, col="darkgreen")+
   geom_rug(data=filter(big.fru.ag.sw,Frugivore.Genus=="Dasyprocta"), aes(x=log10sw ,y=0.1),
            lwd=1,  sides="b", col="#00A522")+
   theme_classic()+
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2), labels=c("0.001",0.01, 0.1, 1, 10, 100))+
    xlab("Seed mass (g)")+
   ylab("Density probability")+
   theme(legend.position=c(.25, 0.8),
         axis.title.x = element_text(size=12),
         legend.background = element_rect(colour = "transparent", fill = "transparent"))+
   guides(fill = guide_legend(reverse = TRUE))
   
 
### alltogether-
grid.arrange(Ateles.plot, Brachyteles.plot, 
              Tapirus.plot, Dasyprocta.plot,
              ncol=1)

### Comparing Carbon storage capacity of large frugivores and Agoutis----

### creating proxy of carbon storage ( wood density * tree height)
big.fru.ag2<-big.fru.ag %>% mutate(Carbon=Tree.size.max.m*Wood.density.g.cm3) %>% 
  filter(Carbon>4)
# only species that are known to be dispersed by aogutis
big.fru.ag3 <- big.fru.ag2 %>% filter(DispTFR=="TRUE")

## table of Carbon storage of species dispersed of each genus
big.fru.ag3 <- big.fru.ag3 %>% group_by(Frugivore.Genus) %>% 
  summarise(C=mean(Carbon),n=n(), sd=sd(Carbon), se=sd/sqrt(n-1), 
            min=C-(1.96*se), max=C+(1.96*se), med=median(Carbon))

summary( aov(data=big.fru.ag2, Carbon~Frugivore.Genus))
## kruskal wallis and Dunn test
kruskal.test(data=big.fru.ag2, Carbon~Frugivore.Genus)

FSA::dunnTest(data=big.fru.ag2, Carbon~Frugivore.Genus, method="bh")

## Plotting again----------
## 10000 samples for each frugivores to plot
 
Ateles1000C<- sample_n(filter(big.fru.ag2, Frugivore.Genus=="Ateles"), 10000, replace = T)
Brachyteles1000C<- sample_n(filter(big.fru.ag2, Frugivore.Genus=="Brachyteles"), 10000, replace = T)
Dasyprocta1000C<- sample_n(filter(big.fru.ag2, Frugivore.Genus=="Dasyprocta"), 10000, replace = T)
Tapirus1000C<- sample_n(filter(big.fru.ag2, Frugivore.Genus=="Tapirus"), 10000, replace = T)
 
big.fru.ag.2.boot <- rbind(Ateles1000C,Brachyteles1000C,Dasyprocta1000C,Tapirus1000C) 
 
 
## histogram with Carbon proxy
Ateles.plotCarbon <- ggplot(data=filter(big.fru.ag2,Frugivore.Genus=="Ateles"), aes(x=jitter(Carbon)))+
  coord_cartesian(xlim = c(3.5, 50), ylim=c(0,0.105))+
  geom_histogram(aes(y=..density..),fill="#B95F2B", col="grey60", position="dodge", alpha=0.8, bins=50)+
  geom_density(lwd=1, adjust=1.5, fill="#B95F2B", alpha=0.7)+
  geom_vline(data=filter(big.fru.ag3,Frugivore.Genus=="Ateles"), aes(xintercept=C),
             linetype="longdash", lwd=1.5)+
  geom_rug(data=filter(big.fru.ag2,Frugivore.Genus=="Ateles"), aes(x=Carbon ,y=0.1),
           lwd=1,  sides="b", col="#B95F2B")+
  theme_classic()+
  scale_y_continuous(breaks=c(0,0.035,0.070,0.105))+ 
  xlab("  ")+
  ylab("Density probability")
  #theme(axis.title.x =element_blank())

Brachyteles.plotCarbon <- ggplot(data=Brachyteles1000C, aes(x=jitter(Carbon,200)))+
  coord_cartesian(xlim = c(3.5, 50), ylim=c(0,0.105))+
  geom_histogram(aes(y=..density..),fill="#228CCC", col="grey60", position="dodge", alpha=0.8, bins=50)+
  geom_density(lwd=1, adjust=3.6, fill="#228CCC", alpha=0.7)+
  geom_vline(data=filter(big.fru.ag3,Frugivore.Genus=="Brachyteles"), aes(xintercept=C),
             linetype="longdash", lwd=1.5)+
  geom_rug(data=filter(big.fru.ag2,Frugivore.Genus=="Brachyteles"), aes(x=Carbon ,y=0.1),
           lwd=1,  sides="b", col="#228CCC")+
  theme_classic()+
  scale_y_continuous(breaks=c(0,0.035,0.070,0.105))+
  xlab("  ")+
  ylab("Density probability")
  #theme(axis.title.x =element_blank())

Tapirus.plotCarbon <- ggplot(data=filter(big.fru.ag2,Frugivore.Genus=="Tapirus"), aes(x=jitter(Carbon)))+
  coord_cartesian(xlim = c(3.5, 50), ylim=c(0,0.105))+
  geom_histogram(aes(y=..density..),fill="#F3F354", col="grey60", position="dodge", alpha=0.8, bins=50)+
  geom_density(lwd=1, adjust=1.5, fill="#F3F354", alpha=0.7)+
  geom_vline(data=filter(big.fru.ag3,Frugivore.Genus=="Tapirus"), aes(xintercept=C),
             linetype="longdash", lwd=1.5)+
  geom_rug(data=filter(big.fru.ag2,Frugivore.Genus=="Tapirus"), aes(x=Carbon ,y=0.1),
           lwd=1,  sides="b", col="#F3F354")+
  theme_classic()+
  scale_y_continuous(breaks=c(0,0.035,0.070,0.105))+
  xlab("  ")+
  ylab("Density probability")
  #theme(axis.title.x =element_blank())



Dasyprocta.plotCarbon <- ggplot(data=filter(big.fru.ag2,Frugivore.Genus=="Dasyprocta"), aes(x=jitter(Carbon)))+
  coord_cartesian(xlim = c(3.5, 50), ylim=c(0,0.105))+
  geom_histogram(aes(y=..density..),fill="#0CDE37", col="grey60", position="dodge", alpha=0.8, bins=50)+
  geom_density(aes(fill=DispTFR, alpha=DispTFR),lwd=1, adjust=1.5)+
  scale_alpha_manual(values = c(0.8,0.7),guide=FALSE)+
  scale_fill_manual(values=c("#80B6D9", "#0CDE37"),name="",
                    labels=c("No evidence\nof dispersal", "Dispersed"))+
  #geom_density(lwd=1, adjust=1.5, fill="#00A522", alpha=0.7,trim=T)+
  geom_vline(data=filter(big.fru.ag3,Frugivore.Genus=="Dasyprocta"), aes(xintercept=C),
             linetype="longdash", lwd=1.5, col="darkgreen")+
  geom_rug(data=filter(big.fru.ag2,Frugivore.Genus=="Dasyprocta"), aes(x=Carbon ,y=0.1),
           lwd=1,  sides="b", col="#0CDE37")+
  theme_classic()+
  scale_y_continuous(breaks=c(0,0.035,0.070,0.105))+
  ylab("Density probability")+
  xlab("Carbon storage")+
  theme(legend.position=c(.2, 1.05),
        axis.title.x = element_text(size=12),
        legend.background = element_rect(colour = "transparent", fill = "transparent"))+
  guides(fill = guide_legend(reverse = TRUE))

# only dispersed species for Dasyprocta
Dasyprocta.plotCarbon2 <- ggplot(data=filter(big.fru.ag2,Frugivore.Genus=="Dasyprocta"
                                             & DispTFR==TRUE), aes(x=jitter(Carbon)))+
  coord_cartesian(xlim = c(3.5, 50), ylim=c(0,0.105))+
  geom_histogram(aes(y=..density..),fill="#0ACD33", col="grey60", alpha=0.8, bins=50)+
  geom_density(lwd=1, adjust=1, fill="#0ACD33", alpha=0.7)+
  geom_vline(data=filter(big.fru.ag3,Frugivore.Genus=="Dasyprocta"), aes(xintercept=C),
             linetype="longdash", lwd=1.5)+
  geom_rug(data=filter(big.fru.ag2,Frugivore.Genus=="Dasyprocta"
                       & DispTFR==TRUE), aes(x=Carbon ,y=0.1),
           lwd=1,  sides="b", col="#0ACD33")+
  theme_classic()+
  scale_y_continuous(breaks=c(0,0.035,0.070,0.105))+
  ylab("Density probability")+
  xlab("Carbon storage")+
  theme(axis.title.x = element_text(size=12))


### alltogehter-
grid.arrange(Ateles.plotCarbon, Brachyteles.plotCarbon, 
             Tapirus.plotCarbon, Dasyprocta.plotCarbon,
             ncol=1)
### alltogehter-
grid.arrange(Ateles.plotCarbon, Brachyteles.plotCarbon, 
             Tapirus.plotCarbon, Dasyprocta.plotCarbon2,
             ncol=1)

## carbon and seed size together

alltogehter.carbon.plot <- grid.arrange(Ateles.plotCarbon, Brachyteles.plotCarbon, 
                                        Tapirus.plotCarbon, Dasyprocta.plotCarbon2,
                                        ncol=1)

alltogehter.seed.plot <- grid.arrange(Ateles.plot, Brachyteles.plot, 
             Tapirus.plot, Dasyprocta.plot,
             ncol=1)

grid.arrange( alltogehter.seed.plot,alltogehter.carbon.plot, ncol=2)
plot_grid( alltogehter.seed.plot,alltogehter.carbon.plot, ncol=2, labels = "auto", hjust = 0.1)



# plot for fun, lm of seed weight ~ Carbon storage for all seed consumed by the 4 genera-----
big.fru.ag2<-big.fru.ag %>% mutate(Carbon=Tree.size.max.m*Wood.density.g.cm3)
seedcarb <- big.fru.ag2 %>% filter(Seed.weigh.g>0) %>% 
  mutate(log10sw=log10(Seed.weigh.g))
#adjusting carbon for tapirus so we can see lines in thegraph
big.fru.ag3[4,-1] <- (big.fru.ag3[4,-1])-1

seedcarb2 <- left_join(seedcarb,big.fru.ag3)

vec <- seq(-4,2,by=0.01)
vec4 <- sort(rep(vec,4))
vec.frug <- rep(big.fru.ag3$Frugivore.Genus, length(vec4)/4)
ymin <- rep(big.fru.ag3$min, length(vec4))
ymax <- rep(big.fru.ag3$max, length(vec4))
vecC <- rep(big.fru.ag3$C,length(vec4) )
big.fru.ag4 <- data.frame(vec4,vec.frug,ymin,ymax, vecC)

ggplot(data=seedcarb2)+
  geom_point(aes(col=Frugivore.Genus,x=log10sw, y=Carbon), size=2)+
  geom_smooth(aes(x=log10sw, y=Carbon),method = lm, lwd=3, col="black")+
  #geom_smooth(aes(col=Frugivore.Genus),method = lm, ,lwd=1, alpha=0.2)+
  geom_hline(data=big.fru.ag3, aes(yintercept=C,color=Frugivore.Genus),
             linetype="solid", lwd=1.5)+
  #geom_ribbon(data=big.fru.ag4,aes(x=vec4,ymin=ymin,ymax=ymax,fill=vec.frug,col=vec.frug),
              #alpha=0.15)+
  theme_minimal()
  
