# 			Phylogenetic sign analysis for binary data		        ###
 
## Loading packages and functions-------------
require(phytools); require (geiger); require(diversitree); require (caper)

firstlower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

## Reading and preparing data-----
# Reading the phylogeny of plant families of the world
# According to Harris & Davis 2016 PLos One
All.families.tree<- read.tree("dados/knb.1177.1.data")

### Reading list of Neotropical families according to Ulloa et al., 2017
# and transformin it into a vector with minuscules letters
Neotropic.families<- read.csv2("dados/Neotropic vascular plants families.csv")
Neotropic.families$Families <-  stringr::word(Neotropic.families$Families, 1)  
Neotropic.families<- as.vector(firstlower(Neotropic.families$Families))

my.phylo <-  All.families.tree

#trimming tree of all families to only contain Neotropical families
Neotropic.families.tree<-drop.tip(All.families.tree, setdiff(All.families.tree$tip.label, Neotropic.families));


# Loading data of which species are dispersed or not by agoutis
hoarding <- read.csv2("dados/Tidy agouti2.csv")

###PLANTS THAT AGOUTIS INTERACT

# Creating the dataset with plant families that agoutis interact
# for each agouti species
i.families <-table(hoarding$Family, hoarding$Agouti.species)
### number of species per family
i.families<-as.data.frame.matrix(i.families)
i.families$Family <- rownames(i.families)

## data frame with families that agoutis interact
i.families <- mutate(i.families, Agouti=1)
i.families <- transmute(i.families, Family, Agouti)
i.families$Family <- firstlower(i.families$Family)

 
# neotropical families as a data.frame
plant.families<-data.frame(Family=Neotropic.families.tree$tip.label)

# Merging two datasets, all species and only agouti consumed species

dispersers<- merge(i.families, plant.families, by = "Family", all=TRUE)

# All NA to zero
dispersers[is.na(dispersers)]<-0

# All values higher than one to 1
#dispersers$Agouti[which(dispersers$Agouti>1)] <- 1



#### Doing phylogenetic signal test ----------------------
# Matching tree and dataset
data<-dispersers
tree<-Neotropic.families.tree
tree<-multi2di(tree)
row.names(data)<-data$Family
data<-data[match(tree$tip.label, data$Family),]

# Running phylogenetic signal for each dispersal group
unds <- phylo.d(data, di2multi(tree), names.col=Family, binvar= Agouti, permut = 1000, rnd.bias=NULL)

unds

### Explanation of results
# the phylogenetic dispersion “D” (Fritz & Purvis, 2010) 
# D usually varies from 0 to 1. D = 0 denotes that the trait evolved according
# to a Brownian phylogenetic structure (phylogenetic conserved trait);
# D = 1 indicates that the trait has a random distribution in the phylogeny. 

###PLANTS THAT AGOUTIS DISPERSE

# Creating the dataset with plant families that agoutis disperse
hoarding$DispTF <- grepl("x",hoarding$Dispersal)## TRUE for cells tha contains 'x'
hoarding <- hoarding %>% filter(DispTF==TRUE)
d.families <-table(hoarding$Family, hoarding$Agouti.species)
d.families<-as.data.frame.matrix(d.families)
d.families$Family <- rownames(d.families)
d.families <- mutate(d.families, Agouti=1)
d.families <- transmute(d.families, Family, Agouti)
d.families$Family <- firstlower(d.families$Family)

# Loading the dataset with all plant families (from Harris & Davis 2016)
# trimmed with only neotropical families
plant.families<-data.frame(Family=Neotropic.families.tree$tip.label)

# Merging two datasets

dispersers<- merge(d.families, plant.families, by = "Family", all=TRUE)

# All NA to zero
dispersers[is.na(dispersers)]<-0

# All values higher than one to 1
dispersers$Agouti[which(dispersers$Agouti>1)] <- 1


## Doing phylogenetic signal ---------------------------------------------------------
# Matching tree and dataset
data<-dispersers
tree<-Neotropic.families.tree
tree<-multi2di(tree)
row.names(data)<-data$Family
data<-data[match(tree$tip.label, data$Family),]

# Running phylogenetic signal for each dispersal group
unds <- phylo.d(data, di2multi(tree), names.col=Family, binvar= Agouti, permut = 1000, rnd.bias=NULL)

unds

