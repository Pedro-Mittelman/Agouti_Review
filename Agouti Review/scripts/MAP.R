#MAP of studies locations of agouti-plant interactions

# Packages----------

library(ggmap)
library(tidyverse)
library(ggplot2)
library(raster)
library(maptools)
library(measurements)
library(devtools)
library(scales)

## Reading and preparing data------

main.agouti <- read.csv2("dados/Tidy agouti2.csv", na.strings = "")

## filtering only one study per line
main.agouti <- main.agouti %>% mutate(Study.unique=paste(Lat, Long, Citation ))

agouti.studies <- main.agouti[!duplicated(main.agouti$Study.unique),]


### converting lat long from degrees to decimal using "measurements" package
#changing symbols to space and cleaning

#LONG

agouti.studies$Long.dec <- gsub('° ', ' ', agouti.studies$Long)
agouti.studies$Long.dec <- gsub("' ", " 00", agouti.studies$Long.dec)
agouti.studies$Long.dec <- gsub("W", "", agouti.studies$Long.dec)
agouti.studies$Long.dec <- conv_unit(agouti.studies$Long.dec, from = "deg_min_sec", to="dec_deg")
agouti.studies$Long.dec <- as.numeric(agouti.studies$Long.dec)*-1


#LAT
agouti.studies$Lat.dec <- gsub('° ', ' ', agouti.studies$Lat)
agouti.studies$Lat.dec <- gsub("' ", " 00", agouti.studies$Lat.dec)
Lat.dec.South <- grepl("S",agouti.studies$Lat.dec) ## saving South to multiply them for -1
agouti.studies$Lat.dec <- gsub("S", "", agouti.studies$Lat.dec)
agouti.studies$Lat.dec <- gsub("N", "", agouti.studies$Lat.dec)
agouti.studies$Lat.dec <- conv_unit(agouti.studies$Lat.dec, from = "deg_min_sec", to="dec_deg")
agouti.studies$Lat.dec <- ifelse(Lat.dec.South, as.numeric(agouti.studies$Lat.dec)*-1, agouti.studies$Lat.dec)

##
## rounding latitude and longitude to two 2 decimals
#and filtering works that coordinates could not be found
loc.studies <- agouti.studies %>% transmute(latitude= round(as.numeric(Lat.dec),2),
                                            longitude=round(as.numeric(Long.dec),2),
                                            Agouti_Species =Agouti.species) %>% filter(latitude!=0)

## Run to make little changes in points coordinates in order to 
# set very close locations as only one bigger point
# loc.studies$latitude[loc.studies$latitude==-12.02] <- -12.25 
# loc.studies$latitude[loc.studies$latitude==-13.82] <- -13.83
# loc.studies$longitude[loc.studies$longitude==-39.13] <- -39.17
# loc.studies$latitude[loc.studies$latitude==-22.43] <- -22.45
# loc.studies$longitude[loc.studies$longitude==-42.02] <- -42.03
# loc.studies$latitude[loc.studies$latitude==-23.45] <- -23.52 
# loc.studies$latitude[loc.studies$latitude==-27.77] <- -27.7
# loc.studies$longitude[loc.studies$longitude==-48.55] <- -48.5
# loc.studies$latitude[loc.studies$latitude==6.75] <- 6.85
# loc.studies$longitude[loc.studies$longitude==-75.1] <- -75.13
# loc.studies$latitude[loc.studies$latitude==-19.23] <- -19.57
# loc.studies$longitude[loc.studies$longitude==-57.02] <- -56.23
# loc.studies$latitude[loc.studies$latitude==-18.98] <- -19.57
# loc.studies$longitude[loc.studies$longitude==-56.65] <- -56.23
# loc.studies$latitude[loc.studies$latitude==9.1] <- 9.17
# loc.studies$longitude[loc.studies$longitude==-79.67] <- -79.85
# loc.studies$latitude[loc.studies$latitude==9.02] <- 9.17
# loc.studies$longitude[loc.studies$longitude==-79.62] <- -79.85
# loc.studies$latitude[loc.studies$latitude==-21.55] <- -20.05
# loc.studies$latitude[loc.studies$latitude==10.43] <- 10.20
# loc.studies$longitude[loc.studies$longitude==-84.00] <- -84.70
# loc.studies$latitude[loc.studies$latitude==10.30] <- 10.20
# loc.studies$longitude[loc.studies$longitude==-84.80] <- -84.70
# loc.studies$latitude[loc.studies$latitude==-11.90] <- -12.25
# loc.studies$longitude[loc.studies$longitude==-71.32] <- -71.75
# loc.studies$latitude[loc.studies$latitude==-11.90] <- -12.25
# loc.studies$longitude[loc.studies$longitude==-71.32] <- -71.75
# loc.studies$latitude[loc.studies$latitude==6.02] <- 4.68
# loc.studies$longitude[loc.studies$longitude==-75.13] <- -74.15

## obtaining number of studie per location
#lat and long in one row as location
loc.studies <- mutate(loc.studies, Location= paste(latitude, longitude))

#number of studies according to the new row 'Location'
loc.studies2 <- loc.studies %>% group_by(Location) %>% 
  count() %>% transmute(Total.studies=as.numeric(n))

loc.studies3 <- merge(loc.studies2, loc.studies)

#filtering duplicated locations as the number of studies for each location
# is now already on the data frame
loc.studies4 <- loc.studies3[!duplicated(loc.studies3$Location),]
         


## Plotting------------------
# Making map than using it in ggplot 

mapa <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                     "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
                                     "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
                                     "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
                                     "Dominica", "Saba"), 
               # fill = "grey70", 
               colour = "black")

## ggplot with agouti species name as legend

ggplot() + mapa + theme_bw() + xlab("") + ylab("") + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_blank())+
  geom_point(data = loc.studies4, aes(x = longitude, y = latitude,
    col=Agouti_Species, size=log(Total.studies,2), alpha= 1/Total.studies), shape=16)+
scale_radius(range=c(4,24),breaks =c(0,2,3.5,4.5),
             name = "Number of Studies", labels = c(1,4,12,24), guide = "none")+
  scale_alpha_continuous(range = c(0.55,0.95), guide="none")+
  scale_color_manual(name="Agouti Species", values = c("#F8766D", "#CD9600", "#7CAE00", "#7FAD7C", "#00BE67",
                                "#00A9FF", "#B5B5B5", "#FF61CC"))+
  guides (colour = guide_legend(override.aes = list(size=4)))+
theme(
  axis.text = element_text(size = 12),
  legend.title = element_text(size=14),
  legend.text = element_text(size=11, face = "italic"),
legend.position=c(.25, .34),
legend.background = element_rect(fill="transparent"))

## Run to pick circle size legend 
# 
# ggplot() + mapa + theme_bw() + xlab("") + ylab("") + 
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_line(colour = "grey90"), 
#         panel.grid.minor = element_blank())+
#   geom_point(data = loc.studies4, aes(x = longitude, y = latitude,
#                                       col=Agouti_Species, size=log(Total.studies,2), alpha= 1/Total.studies), shape=16)+
#   scale_radius(range=c(4,24),breaks =c(0,1,2.3,3.3,4.321,4.906),
#                name = "Number of Studies", labels = c(1,2,5,10,20,30))+
#   scale_alpha_continuous(range = c(0.55,0.95), guide="none")+
#   scale_color_manual(name="Agouti Species", values = c("#F8766D", "#CD9600", "#7CAE00", "#7FAD7C", "#00BE67",
#                                                        "#00A9FF", "#B5B5B5", "#FF61CC"),guide="none")+
#   theme(
#     axis.text = element_text(size = 12),
#     legend.title = element_text(size=14),
#     legend.text = element_text(size=11),
#     legend.position="right")


 