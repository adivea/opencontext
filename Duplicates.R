# Get Tidyverse
install.packages("tidyverse")
library(tidyverse)

# Get data and check for issues
units <- read_csv("SurveyUnitsKaz.csv")
problems(units)
units

# Detect duplicates in Survey Units
units %>% 
  group_by(SUID) %>% 
  filter(n()>1)


## checking for Duplicates in Survey Units without tidyverse

units <- read.csv("SurveyUnitsKaz.csv", header = TRUE)
head(units)
dim(units)
uniqunits <- unique(units$SUID)
length(units$SUID)-length(unique(units$SUID))

#Detect duplicates in Mounds

library(tidyverse)
# read in exported mound IDs from 200910Verified.shp
vm <-  read_csv("C:/Users/Adela/Documents/Professional/Projects/MQNS/Data/Export_Output.txt")

vm %>% 
  group_by(TRAP) %>% 
  filter(n()>1)  
  
