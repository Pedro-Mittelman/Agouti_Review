##### Packages----
library(tidyverse)
library(lme4)
library(DHARMa)

##### Preparing data-----  

###reading data
main.agouti <- read.csv2("dados/Tidy agouti2.csv")

### transforming dispersal by agoutis in TRUE or FALSE
main.agouti$DispTF <- grepl("x",main.agouti$Dispersal) ## TRUE for cells that contains 'x'

###correcting dispersal for the same species
### once a  plant species is seen being dispersed, it is considered dispersed by agoutis
main.agouti2 <- main.agouti %>% group_by(Species) %>%
  summarise(DispTFR=max(DispTF), Peso.s= mean(Seed.weight..g., na.rm=T))
# filtering only dispersed and exclusively preyed species
main.agouti.P <- filter(main.agouti, Predation=="x")
# merging
main.agouti3 <- inner_join(main.agouti.P,main.agouti2)

### filtering duplicate species (one specie per line)
main.agouti4 <- main.agouti3 %>% distinct(Species, .keep_all=TRUE)

### filtering only species with known seed weight
seed.weigh <- filter(main.agouti4, Peso.s>0)
seed.weigh$Seed.weigh.in.g <- seed.weigh$Peso.s

#seed weight in miligrams
seed.weigh$Seed.weigh.in.mg.log10 <- log10(seed.weigh$Seed.weigh.in.g*1000)


##### Building generalized (binomial) linear model----
#model
m.seed.weigh<- glm(data=seed.weigh, DispTFR~Seed.weigh.in.g, family = "binomial" )
summary(m.seed.weigh)
#log of seed weight
m.seed.weigh.log10<- glm(data=seed.weigh, DispTFR~Seed.weigh.in.mg.log10, family = binomial(link="probit"))
summary(m.seed.weigh.log10)

## testing models' residuals 
DHARMa::testResiduals(m.seed.weigh.log10)
simulateResiduals(fittedModel = m.seed.weigh.log10, plot = T)
    # no over or under dispersion, no outliers a little bit of acceptable heteroscedascity

##### Plotting-----
# General prediction
g.seed.weigh.log10<-expand.grid(Seed.weigh.in.mg.log10 = seq(1, 5, 0.01))
predict.g.seed.weigh.log10<-predict(m.seed.weigh.log10, newdata = g.seed.weigh.log10, type="response", re.form=NA)
df.predict.g.seed.weigh.log10 <- data.frame(pred = predict.g.seed.weigh.log10, g.seed.weigh.log10)

seed.weigh$DispTFR <- as.logical(seed.weigh$DispTFR)

ggplot(data = seed.weigh, aes(x = Seed.weigh.in.mg.log10, y = jitter(as.numeric(DispTFR),0.05), 
                              col=DispTFR))+
  geom_line(data = df.predict.g.seed.weigh.log10, aes(y = pred, x = Seed.weigh.in.mg.log10 ), 
            col= "black", size=4, alpha=0.65) +    
  geom_point(size=4, alpha=0.6)+
  scale_color_manual(values=c("#F65252","#00AC24"))+
  xlab("Seed weigh (g)")+
  ylab("Probability of species dispersal by agoutis")+
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c(0.001, 0.01, 1, 10, 100))+
  theme_classic()+   
  theme(
    legend.position = "none")


##### Calculating inflection point of binomial curve-----
p <- 0.5 # 50 / 50 probability on the curve
x <- (log(p/(1-p)) - coef(m.seed.weigh.log10)[1] ) / coef(m.seed.weigh.log10)[2]
x
10^x/1000 # converting weigh from log10 mg to grams

###calculating weigh tat there is a 95% probability of species dispersal
p <- 0.95 
x <- (log(p/(1-p)) - coef(m.seed.weigh.log10)[1] ) / coef(m.seed.weigh.log10)[2]
x
10^x/1000 # converting weigh from log10 mg to grams
