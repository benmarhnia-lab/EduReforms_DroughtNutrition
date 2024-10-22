###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######               School reforms                    ####### 
#######                                                 ####### 
#######           CODE: Add climate data                ####### 
###############################################################
###############################################################
rm(list =ls())

library(sf)
library(raster)
library(viridis)
library(rgdal)
library(rworldmap)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)




setwd("D:/School reforms/Data/")

data <- read.csv("data_for_analysis.csv")[-1]
sort(unique(data$birthYr))
unique(data$CountryName)

data <- data %>% 
  #filter(CountryName!="Ethiopia" & CountryName!="Cote d'Ivoire" & CountryName!="Gabon" & CountryName!="Guinea" & CountryName!="Madagascar") %>% 
  #filter(!is.na(wasted)) %>% 
  mutate(intMo = ifelse(intMo=="june", 6, intMo)) %>% 
  mutate(intMo = ifelse(intMo=="july", 7, intMo)) %>% 
  mutate(intMo = ifelse(intMo=="august", 8, intMo)) %>% 
  mutate(intMo = ifelse(intMo=="september", 9, intMo)) %>% 
  mutate(intMo = ifelse(intMo=="october", 10, intMo)) %>% 
  mutate(intMo = ifelse(intMo=="november", 11, intMo)) %>% 
  mutate(intMo = as.numeric(intMo)) %>% 
  mutate(intYr = as.numeric(intYr))

unique(data$intMo)

  
psu <- data %>% 
  dplyr::select(CountryName, SurveyId, psu, LATNUM, LONGNUM) %>% 
  unique()

# create a spatial points dataframe
sp <- psu 
# convert to spatial points
coordinates(sp) <- c("LONGNUM", "LATNUM")
# assign CRS
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
plot(sp)



################################################################################
### Generate exposure measures - 2 to 5 month SPEI
################################################################################

load("D:/Data/SPEI_generated/spei02.RData")

cent_spei <- raster::extract(sd_SPEI,         # raster layer
                             sp,              # SPDF with centroids for buffer
                             df=TRUE)         # return a dataframe? 

cent_spei <- cent_spei[-c(1:949)]

df0 <- cbind(sp@data, cent_spei)

df0 <- df0 %>%
  gather(key = date, value = spei, -psu, -SurveyId, -CountryName) %>%
  mutate(date = as.Date(substring(date, 2), format = "%Y.%m.%d"),
         year = format(date, format = "%Y"),
         month = format(date, format = "%m")) %>% 
  dplyr::select(-c(date)) %>% 
  mutate(year = as.numeric(year),
         month=as.numeric(month))


df0 <- df0 %>% na.omit()
df0 <- df0 %>% unique()


data_matched <- data %>% 
  #mutate(birthYr_plus1 = birthYr+1) %>% 
  left_join(df0, by = c("CountryName" = "CountryName", "SurveyId" = "SurveyId", "psu" = "psu", "intYr" = "year", "intMo" = "month")) %>% 
  rename(spei_intMo = spei)


write.csv(data_matched, "data_for_analysis_spei02.csv")


sort(unique(data$intMo))
sort(unique(data$intYr))


check <- data %>% 
  group_by(SurveyId) %>% 
  summarise(n=sum(n.wt))
