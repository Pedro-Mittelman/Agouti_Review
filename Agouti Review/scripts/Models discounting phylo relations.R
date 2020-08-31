##packages
library(tidyverse)
library(PVR)
library(phytools)
library(splancs)
library(ape)

## example of model that will incorporate philogeny (species seed weight vs disersed or not)----
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


### building model

#log of seed weight
m.seed.weigh.log10<- glm(data=seed.weigh, DispTFR~Seed.weigh.in.mg.log10, family = binomial(link="probit"))
summary(m.seed.weigh.log10)

### loading phylogenetic tree of soecies in the model, after phylomatic----
## reading tree
arvsw <- read.tree("dados/arvseedweigh.txt")
##computing branch lenghts
arvsw <- compute.brlen(arvsw,method="Grafen")


### computing PVR: vectors of phlylogenetic autocorrelation-----
PVR.arvsw<- PVRdecomp(arvsw)


## seeing how many eigen vectors I neeed-----
## PVR.arvsw@Eigen$values shows how much each eigenvectros explains
# the phylogenictic distances, total sum of eigenvalues=67.01465
ev.cumul <- cumsum(PVR.arvsw@Eigen$values)*(100/(sum(PVR.arvsw@Eigen$values)))
ev.cumul ## we need 24 first eigen vectors to explain phylogentic variation

### making data frame of eigenvectors
Eigen.vecs <- PVR.arvsw@Eigen[["vectors"]]


### testing if residuals are correlated by Moran test----
###residuals are agregated/clustered if Moran I value>0 and p.value<0.005


## MAKE SURE ORDER OF SPECIES ON THE TREE/MATRIX IS THE SAME AS IN THE RESIDUALS (DATA FRAME OF THE MODEL) 
## residuals of the model
x <- residuals(m.seed.weigh.log10)
### correlation between observations (plant species), is the oposite of the diStance between
# each specis
## obtaning matrix
w <- 1/cophenetic(arvsw)
## set the diagonal w[i,i] = 0 (instead of Inf...):
diag(w) <- 0
Moran.I(x, w)

## Model with with the first eigen vector
m.seed.weigh.eig1.log<- glm(data=seed.weigh, DispTFR~Seed.weigh.in.mg.log + Eigen.vecs2[,2], family = "binomial" )
summary(m.seed.anyweigh.eig1.log)

##ADD ONE EIGEN VECTOR AT TIME UNTIL MORAN I VALUES FOR RESIDUALS IS NOT SIGNIFICATIVE ANYMORE
