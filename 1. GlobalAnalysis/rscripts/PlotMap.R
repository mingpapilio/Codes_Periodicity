# Load packages
library(data.table)
library(ggplot2)
library(ggpubr)
theme_set(theme_minimal())
# Read in temperature dataset
TMP<- fread("../data/TMP_CRU1901-2020_26Oct.csv")
# Make maps
TMPavg<- ggplot(as.data.frame(TMP), aes(lon, lat, fill= avg)) + 
  geom_tile()+
  scale_fill_viridis_c(option = "plasma", name= "", na.value= "white")+
  xlab("Longitude")+ ylab("Latitude")+
  theme(legend.key.height= unit(1, 'cm'))
TMPsd<- ggplot(as.data.frame(TMP), aes(lon, lat, fill= sqrt(sd))) + 
  geom_tile()+
  scale_fill_viridis_c(option = "plasma", name= "", na.value= "white")+
  xlab("Longitude")+ ylab("Latitude")+
  theme(legend.key.height= unit(1, 'cm'))
# Read in precipitation dataset
PRE<- fread("../data/PRE_CRU1901-2020_26Oct.csv")
# Make maps
PREavg<- ggplot(as.data.frame(PRE), aes(lon, lat, fill= sqrt(avg))) + 
  geom_tile()+
  scale_fill_viridis_c(option = "plasma", name= "", na.value= "white")+
  xlab("Longitude")+ ylab("Latitude")+
  theme(legend.key.height= unit(1, 'cm'))
PREsd<- ggplot(as.data.frame(PRE), aes(lon, lat, fill= log(sd))) + 
  geom_tile()+
  scale_fill_viridis_c(option = "plasma", name= "", na.value= "white")+
  xlab("Longitude")+ ylab("Latitude")+
  theme(legend.key.height= unit(1, 'cm'))
# Assign titles for each map
TMPavg_2<- annotate_figure(TMPavg, top= text_grob("(a) Average temperature",  hjust=1.7, face="bold"))
TMPsd_2<- annotate_figure(TMPsd, top= text_grob("(b) SD of temperature",  hjust=1.88, face="bold"))
PREavg_2<- annotate_figure(PREavg, top= text_grob("(c) Average precipitation",  hjust=1.7, face="bold"))
PREsd_2<- annotate_figure(PREsd, top= text_grob("(d) SD of precipitation",  hjust=1.88, face="bold"))
# Combine maps and save
BasicMap<- ggarrange(TMPavg_2, PREavg_2, TMPsd_2, PREsd_2, ncol=2, nrow=2)
ggsave("BasicMap.jpg", BasicMap, width= 14, height = 8)
