#### Figure 1. Network showing recorded interactions between plant species and agouti species  ####
#packages---
library(bipartite)
library(tidyverse)

# Reading ad preparing data----
##reading data

main.agouti <- read.csv2("dados/Tidy agouti2.csv")

df.net <- main.agouti %>% select(Agouti.species,UpdatedPlantSpecies)
df.net <- as.data.frame(df.net %>% mutate(webID=1))

## transforming agouti plant interactions into a matrix
agouti_matrix <- frame2webs(df.net,varnames=c("UpdatedPlantSpecies","Agouti.species","webID"))
agouti_matrix <- agouti_matrix[[1]]
class(agouti_matrix)


## Plotting----

plotweb(agouti_matrix, method="normal",  text.rot="90", labsize=0.5, ybig = 1,  y.width.low = 0.1,
        col.interaction="280", bor.col.interaction ="grey30", high.spacing=0.152, low.spacing = 0.004)

