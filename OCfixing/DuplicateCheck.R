install.packages("tidyverse")
install.packages("raster")
install.packages("sp")
install.packages("rgdal")

library(tidyverse)
library(sp)
library(raster)
library(rgdal)


#Load Kazanlak survey units
Kaz <- read.csv2("RStudio/KazUnits.csv")
dim(Kaz)
length(unique(Kaz$SUID))
length(Kaz$SUID)


#Load Yambol survey units

Yam <- read_csv("YamSurveyUnits_20190129.csv")
dim(Yam)
length(unique(Yam$SUID))

Kaz %>% 
Yam %>% 
  group_by(SUID) %>% 
  filter(n()>1)