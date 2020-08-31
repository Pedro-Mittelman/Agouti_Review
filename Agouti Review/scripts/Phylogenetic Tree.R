#CIRCULAR COLORED CALIBRATED PHYLOGENETIC TREE

#### Packages needed-----
library(tidyverse)
library(phytools)
library(ggtree)
library(Biostrings)
library(treeio)

## weird instalation of packages  ggtree and Biostrings (uncoment to run)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")
# BiocManager::install("Biostrings")

### Reading and preparing data-----

#phylogenetic tree was obtained through phylodiversity.net/phylomatic/
#species list was inserted using the following structure (order/family/genus_species) or (family/genus/genus_species)
# e.g: Asparagales/Asparagaceae/Maianthemum_trifolium
#Note that colons and semicolons are not required between different species;

## reading tree obtained through phylomatic
rawtree <- read.tree("dados/Updatd_agouti_tree.txt")
plot(rawtree)
calib.tree <- read.tree("dados/knb.1177.1.data")
##computing branch lengths---
rawtree <- compute.brlen(rawtree,method="Grafen")
plot(rawtree)

## calibrating tree root is the origin of gimnosperms in million years
mycalibration <- makeChronosCalib(rawtree, node="root", age.max=390)

#You hand your tree, smoothing parameter, model choice and the calibration over to the chronos function:
mytimerawtree<- chronos(rawtree, lambda = 1, model = "relaxed", calibration = mycalibration, control = chronos.control() )

## ploting 1
plotTree(mytimerawtree,type="fan",fsize=0.4,lwd=2,
         ftype="i", no.margin=T)

### Using ggtree to edit tree----
## writng and reading again so I can use ggtree
write.tree(mytimerawtree, "dados/testtree.txt")

testtree <- read.tree("dados/testtree.txt")

# plotting again
ggtree(testtree, size=1, layout = "circular")+
  geom_tiplab2(size=2)+
  geom_nodelab2()

### creating list of dispersed and non dispersed species
# to insert in the tree object so we can discrimante them

## reading data
main.agouti <- read.csv2("dados/Tidy agouti2.csv")

#creating new column for Megafauna species and dispersed species
rvd <- main.agouti %>% mutate(DispTF= grepl("x",main.agouti$Dispersal)) ## TRUE for cells tha contains 'x'
rvd$Mega <- as.numeric(ifelse(rvd$Megafauna.Fruit=="Yes","2",
                   ifelse(rvd$Megafauna.Fruit=="No", "1","0")))

# new data frame with seed weight, dispersed or not and megafaunal dispersal for each species
#removing duplicate species
rvd <- rvd %>%  group_by(UpdatedPlantSpecies) %>% 
  summarise(sw=max(Seed.weight..g.), DispTF=max(DispTF), Mega=max(Mega))  

## fixing errors in species names to match tree names

### putting in a data frame
#function to put first letter upper case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

testtree$tip.label <- firstup(gsub("_", " ",testtree$tip.label))
datatree<- data.frame(UpdatedPlantSpecies=testtree$tip.label, tipnumber=c(1:169))
datatree <- inner_join(datatree,rvd)

#fixing error because of first minuscule letters
datatree$node <- datatree$tipnumber
datatree$DispTF <- as.logical(datatree$DispTF)
datatree$Mega <- as.factor(datatree$Mega)

str(testtree)

class(testtree)

## plotting 
tree<- ggtree(testtree, size=2, layout = "circular", col="grey35")

## plotting and adding data about species to map the aesthetics of the tree
tree %<+% datatree +
  geom_tree(aes(col = DispTF),size=2)+
  geom_tiplab2(aes(col = DispTF),size=4,
              geom = "text",  #text not outter labels
              )+
    scale_color_manual(values=c( "#53778E", "#017618") ,guide=F)+
  theme(plot.margin=unit(c(3,3,3,3),"cm"))
