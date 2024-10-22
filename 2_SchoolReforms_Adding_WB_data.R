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

setwd("D:/Anna/Dropbox/Projects/2024_San Diego/School reforms/Data/")

#install.packages("wbstats")
#devtools::install_github("nset-ornl/wbstats")
library(tidyverse)
library(wbstats)

str(wb_cachelist, max.level = 1)
new_cache <- wb_cache()

health_inds <- wb_search("health expenditure")
gdp_inds<- wb_search("GDP per capita")
urban_inds <- wb_search("urban population")

health_data <- wb_data("SH.XPD.GHED.GD.ZS", start_date = 1987, end_date = 2022)
health_data <- health_data %>% 
  subset(country %in% c("Benin", "Burundi", "Lesotho", "Namibia", "Sierra Leone", "Togo", "Zimbabwe",
                        "Cameroon", "Malawi",  "Nigeria", "Rwanda", "Tanzania")) %>% 
  dplyr::rename(health_exp = SH.XPD.GHED.GD.ZS) %>% 
  dplyr::select(country, date, health_exp)


gdp_data <- wb_data("NY.GDP.PCAP.KD.ZG", start_date = 1987, end_date = 2022)
gdp_data <- gdp_data %>% 
  subset(country %in% c("Benin", "Burundi", "Lesotho", "Namibia", "Sierra Leone", "Togo", "Zimbabwe",
                        "Cameroon", "Malawi",  "Nigeria", "Rwanda", "Tanzania")) %>% 
  dplyr::rename(gdp_growth = NY.GDP.PCAP.KD.ZG) %>% 
  dplyr::select(country, date, gdp_growth)


urban_data <- wb_data("SP.URB.TOTL.IN.ZS", start_date = 1987, end_date = 2022)
urban_data <- urban_data %>% 
  subset(country %in% c("Benin", "Burundi", "Lesotho", "Namibia", "Sierra Leone", "Togo", "Zimbabwe",
                        "Cameroon", "Malawi",  "Nigeria", "Rwanda", "Tanzania")) %>% 
  dplyr::rename(urban_pop = SP.URB.TOTL.IN.ZS) %>% 
  dplyr::select(country, date, urban_pop)


wb_data <- health_data %>% 
  left_join(gdp_data) %>% 
  left_join(urban_data)


sum(is.na(wb_data$gdp_growth))
  
## Merge with the DHS data

data <- read.csv("data_for_analysis.csv")

data <- data %>% 
  dplyr::select(-X) %>% 
  mutate(surveyyr_minus1 = surveyYr-1) %>% 
  left_join(wb_data, by= c("CountryName"="country", "surveyyr_minus1"="date"))


write.csv(data, "data_for_analysis.csv")

