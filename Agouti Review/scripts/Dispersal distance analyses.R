##### Packages----
library(tidyverse)
library(lme4)
##### Preparing data-----  

###reading data
main.agouti <- read.csv2("dados/Tidy agouti2.csv")

### transforming dispersal by agoutis in TRUE or FALSE
main.agouti$DispTF <- grepl("x",main.agouti$Dispersal) ## TRUE for cells that contains 'x'

# filtering for studies with information on dispersal distances
seed.distance <- main.agouti %>% filter(Mean.Dispersal.Distance..m.>0)

# new shorter column names
seed.distance$Method <- seed.distance$Methods.for.asseing.dispersal
seed.distance$Distance <-  seed.distance$Mean.Dispersal.Distance..m.

##### Tables with mean and max dispersal distances, sd, n, se and CI----
#according to each method used to assess dispersal

#mean distances
table <- filter(seed.distance, Mean.Dispersal.Distance..m.!=max(seed.distance$Mean.Dispersal.Distance..m.)) %>%
  group_by(Method=Methods.for.asseing.dispersal) %>%
  summarise(Distance=round(mean(Mean.Dispersal.Distance..m.),1), sd= sd(Mean.Dispersal.Distance..m.),n=n())
table <- table %>% mutate(se= sd/(sqrt(n))) %>% mutate(Maxint= Distance+ (1.96*se),Minint= Distance- (1.96*se))
table

#max distences

seed.distance.m <- main.agouti %>% filter(Max.Dispersal.Distance..m.>0)
seed.distance.m$Method <- seed.distance.m$Methods.for.asseing.dispersal
seed.distance.m$Distance <-  seed.distance.m$Max.Dispersal.Distance..m.

table2 <- filter(seed.distance.m, Citation!= "Jansen et al., 2012") %>% 
  group_by(Method=Methods.for.asseing.dispersal) %>% 
  summarise(Distance=round(mean(Max.Dispersal.Distance..m.),1), sd= sd(Max.Dispersal.Distance..m.),n=n())
table2 <- table2 %>% mutate(se= sd/(sqrt(n))) %>% mutate(Maxint= Distance+ (1.96*se),Minint= Distance- (1.96*se))
table2[3,7] <- 0 # adjusting because no negative distances are possible
table2

### both tables together

table3<- rbind(table, table2) %>% mutate(Type=as.factor(c("Mean","Mean","Mean","Mean","Mean",
                                                          "Max","Max","Max","Max","Max")))
table3$Type <- relevel(table3$Type,"Mean")

table3

#Barplot-----

ggplot(data=table3, aes(x=reorder(Method, Distance), y=Distance, fill=Type)) +
  geom_bar(aes(fill=Type),stat="identity", position=position_dodge())+
  coord_flip()+
  scale_x_discrete(labels=c("Flagged line", "Metal implants",
                            "Active search /\n Visual observation", 
                            "Spool and line","Seed radiotrack" ))+
  geom_text(aes(label=Distance), hjust=-0.6, vjust=-0.25, size=3.5,position = position_dodge(1.05), fontface="bold")+
  # geom_text(data=table,aes(label=Distance), hjust=-0.2, vjust=1.8, size=3.5, fontface="bold")+
  # geom_text(data=table2,aes(label=Distance), hjust=-0.2, vjust=-1.4, size=3.5, fontface="bold")+
  scale_y_continuous(breaks = c(0,50,100,150,200,250))+
  scale_fill_manual(values=c("#85E47A", "#37AD2A"))+
  ylab("Dispersal distance (m)")+
  xlab("Assessment method ")+
  labs(fill = " ")+
  theme_minimal()+
  theme(legend.position=c(.75, .3))+
  geom_errorbar(aes(ymin=Minint, ymax=Maxint), width=.2 ,size=0.72,
                position=position_dodge(1))+
  guides(fill = guide_legend(reverse = TRUE))



## Linear model with seed weight and dispersal distances-----

m.seed.distance<- lm(data=seed.distance, Mean.Dispersal.Distance..m.~Seed.weight..g.)
m.seed.distance.log<- lm(data=seed.distance, (Mean.Dispersal.Distance..m.)~log10(Seed.weight..g.))

summary(m.seed.distance.log)
plot(data=seed.distance, Mean.Dispersal.Distance..m.~log10(Seed.weight..g.))
abline(m.seed.distance.log)
# seed weigh not statistically significative


