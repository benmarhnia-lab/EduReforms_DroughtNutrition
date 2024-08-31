###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######               School reforms                    ####### 
#######                                                 ####### 
#######        CODE: Retrieve the DHS data              ####### 
###############################################################
###############################################################

## Contents:
## 1. Retrieve the survey data for sub-Saharan Africa
## 2. Harmonize and clean the survey data
## 3. Recalculate children's anthropometric scores (HAZ, WAZ & WHZ)  
## 4. Add school reform information


## Search "NOTE" for important notes


rm(list =ls())
library(foreign)
#install.packages("tidyverse")
library(tidyverse)
library(data.table)
#install.packages("zoo")
library(zoo)
#install.packages("psych")
library(psych)
library(lubridate)
#install.packages("MAPLES")
library(MAPLES)
#library("readxl")
#install.packages("rdhs")
library(rdhs)      


options(scipen=999)
options(digits=5)


#setwd("C:/Users/annak/Dropbox/Projects/2024_San Diego/School reforms/Data/")
setwd("D:/Anna/Dropbox/Projects/2024_San Diego/School reforms/Data/")

################################################################################
## 1. Retrieve the survey data for sub-Saharan Africa
################################################################################
## Set up your DHS credentials 
set_rdhs_config(email = "XXX",
                project = "XXX",
                config_path = "rdhs.json",
                cache_path = "dhs",
                global = FALSE)

## Make a list of eligible surveys and download them
surveys <- dhs_datasets() %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Children's Recode")

## Add world region and filter to Sub-Saharan Africa only
library(countrycode)
surveys$Region <- countrycode(surveys$CountryName, 'country.name', 'region')  
surveys <- surveys %>% 
  subset(Region == "Sub-Saharan Africa") 

unique(surveys$CountryName)
unique(surveys$SurveyId)

downloads <- get_datasets(surveys$FileName, reformat=TRUE, clear_cache = TRUE)
#print(downloads)
#vec <- unlist(downloads)
#vec

## Select relevant variables
vars = c(
  #maternal & household characteristics
  "caseid", "bidx","midx", "v001", "v002", "v003", "v005", "v006", "v007", "v008","v008a", "v009", "v010", "v011",
  "v012", "v016", "v017", "v021", "v023", "v024", "v025", "v040", "v104", "v106","v113",
  "v115", "v116", "v133", "v134", "v135", "v137", "v139", "v140", "v149", "v151", "v152", 
  "v155", "v157", "v158", "v159", "v160",  "v190", "v191", "v438", "v445", "v467d", "v467b", 
  "v465","v502", "v632", "v704", "v705", "v716", "v717", "v208", "v212",
  #child characteristics and anthropometric measures
  "bidx", "bord", "b0", "b1", "b2" ,"b3", "b4", "b5", "b8", "b9", "b11", "b17", "b18", "b19", "b20",
  "hw70", "hw71", "hw72", "hw73", "hw1", "hw2", "hw3", "hw13", "hw16", "hw17", "hw18", "hw19"
)

questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)

## Extract the data (adding geographical covariates: add_geo = TRUE)
extract <- extract_dhs(questions, add_geo = T)

## Quick check 
head(extract[1])

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE)

CountryName <- surveys %>%
  select(SurveyId, CountryName)

## Keep only observations with valid GPS coordinates
df0 <- df0 %>% 
  filter(!is.na(LATNUM)) %>% 
  filter(!is.na(LONGNUM)) %>% 
  filter(LATNUM!=0 | LONGNUM!=0) %>%                   #LAT=0 and LONG=0 are missing coordinates  
  filter(LATNUM >= 0.00005 | LATNUM <= -0.00005) %>%   #missing obs. --> remove
  mutate(DHSCC = substr(SurveyId, 1, 2)) %>%           #add country code identifier 
  left_join(CountryName)                               #add country names   


unique(df0$SurveyId)
unique(df0$CountryName)

save.image(file='DHS_data_extract.RData')

rm(list =ls())

################################################################################
## 2. Harmonize and clean the survey data
################################################################################
load('DHS_data_extract.RData')

rm(list=setdiff(ls(), c("df0")))

df0 <- df0 %>% 
  dplyr::mutate(SurveyYear = substr(SurveyId, 3, 6))

sort(substr(unique(df0$SurveyId), 3, 6))
sort(unique(df0$SurveyId))

#removing upper case letters for easier harmonization
obj <- c("b5", "b0", "bord","b4","v106","v151","v135","v158","v025")

df0 <- df0 %>%
  mutate_at(vars(obj), funs(tolower(.)))

## Harmonize the data
df1 <- df0 %>% 
  filter(b5 == "yes") %>%             #keep observation only if child is alive
  dplyr::rename(psu           = v001, 
                hh.id         = v002,
                woman.id      = v003,
                intYr         = v007, #year of interview
                intMo         = v006, #month of interview
                intDay        = v016, #day of interview
                intCMC        = v008, #interview date in CMC format (century month code)
                birthYr       = b2,
                birthMo       = b1,
                birthCMC      = b3,
                birthYr_mother  = v010,
                #birthMo_mother  = v009,
                birthCMC_mother = v011,
                age_child_years  = b8,   #age of the child measured in years 
                age_child_months = hw1,
                sex           = b4,
                #birth.int     = b11,  #preceding birth interval - only for 2nd and higher order births
                #children.u5   = v137, #number of children under 5 in the household
                age_mother    = v012,
                #edu.level     = v106,
                #edu.years     = v133,
                hh.head       = v151,
                #hh.size       = v136, 
                region        = v024,
                residence     = v025,
                stratum       = v023) %>%
  #generate woman's weights
  mutate(wt=v005/1000000) %>% 
  mutate(intYr = ifelse(nchar(gsub("\\D", "", intYr))==2, paste("19",intYr, sep=""), intYr)) %>% 
  #mutate(intYr = ifelse(is.na(intYr), SurveyYear, intYr)) %>% 
  mutate(intYr = as.numeric(intYr)) %>% 
  mutate(birthYr = ifelse(nchar(gsub("\\D", "", birthYr))==2, paste("19",birthYr, sep=""), birthYr)) %>% 
  mutate(birthYr = as.numeric(birthYr)) %>% 
  mutate(birthYr_mother = ifelse(nchar(gsub("\\D", "", birthYr_mother))==2, paste("19",birthYr_mother, sep=""), birthYr_mother)) %>% 
  mutate(birthYr_mother = as.numeric(birthYr_mother)) %>% 
  #age of the mother at giving birth
  mutate(twin      = ifelse(b0=="single birth", 0, 1)) %>% 
  #usual resident at the place of interview
  mutate(resident = ifelse(v135=="visitor", 0,
                           ifelse(v135==9, NA, 1))) %>% 
  #remove children who are not usual residents at place of interview
  filter(resident == 1) %>%   
  #generate variable for years living at the place of residence
  mutate(years_at_residence = v104) %>% 
  mutate(years_at_residence = ifelse(v104 == "always", 50, years_at_residence)) %>% 
  #mutate(years_at_residence = ifelse(v104 == "ne sait pas", NA, years_at_residence)) %>% 
  mutate(years_at_residence = ifelse(v104 == "visitor", NA, years_at_residence)) %>% 
  mutate(years_at_residence = ifelse(v104 == "99", NA, years_at_residence)) %>% 
  mutate(years_at_residence = as.numeric(years_at_residence)) %>% 
  # undernutrition variables
  mutate(hw72 = as.numeric(hw72)) %>% 
  mutate(hw70 = as.numeric(hw70)) %>% 
  mutate(hw71 = as.numeric(hw71)) %>% 
  mutate(whz                = ifelse(hw72 <= 500, hw72/100, NA)) %>%
  #mutate(wasted             = ifelse(whz <= -2, 1, 0)) %>% 
  #mutate(wasted_severe      = ifelse(whz <= -3, 1, 0)) %>% 
  mutate(haz                = ifelse(hw70 <= 600, hw70/100, NA))  %>%
  #mutate(stunted            = ifelse(haz <= -2, 1, 0)) %>% 
  #mutate(stunted_severe     = ifelse(haz <= -3, 1, 0)) %>% 
  mutate(waz                = ifelse(hw71 <= 500, hw71/100, NA)) %>% 
  #mutate(underweight        = ifelse(waz <= -2, 1, 0)) %>% 
  #mutate(underweight_severe = ifelse(waz <= -3, 1, 0)) %>% 
  #generate variable for age of the mother at giving birth
  mutate(age_mother_at_birth = (birthCMC - birthCMC_mother)/12) %>% 
  mutate(age_mother_at_birth = round(age_mother_at_birth, digits = 0)) 


################################################################################
## 3. Recalculate children's anthropometric scores (HAZ, WAZ & WHZ)  
################################################################################

## (surveys after 2006 use the new WHO guides, for earlier surveys measures need to be recalculated)
#https://cran.r-project.org/web/packages/anthro/anthro.pdf
#https://stackoverflow.com/questions/67185028/anthropometric-z-scores-who-package-anthro-and-keeping-the-ids
#install.packages("anthro")
library(anthro)

datain <- df1 %>% 
  dplyr::select(whz, waz, haz, hw2, hw3, age_child_months, sex) %>% 
  mutate(sex = ifelse(sex=="male", "M", "F")) %>% 
  mutate(height = as.numeric(hw3)/10) %>% 
  mutate(weight = as.numeric(hw2)/10)

dataout <- with(
  datain,
  anthro_zscores(
    sex = sex, age = age_child_months, is_age_in_month= TRUE,
    weight = weight, lenhei = height))

datain <- datain %>% 
  cbind(dataout) %>% 
  rename(haz_new = zlen,
         waz_new = zwei,
         whz_new = zwfl) %>% 
  mutate(whz_new = ifelse(fwfl == 1, NA, whz_new),      #remove implausible values
         waz_new = ifelse(fwei == 1, NA, waz_new),      #remove implausible values
         haz_new = ifelse(flen == 1, NA, haz_new)) %>%  #remove implausible values
  dplyr::select(whz_new, waz_new, haz_new)  

df1 <- df1 %>% 
  cbind(datain) %>% 
  mutate(wasted             = ifelse(whz_new <= -2, 1, 0)) %>% 
  mutate(wasted_severe      = ifelse(whz_new <= -3, 1, 0)) %>% 
  mutate(stunted            = ifelse(haz_new <= -2, 1, 0)) %>% 
  mutate(stunted_severe     = ifelse(haz_new <= -3, 1, 0)) %>% 
  mutate(underweight        = ifelse(waz_new <= -2, 1, 0)) %>% 
  mutate(underweight_severe = ifelse(waz_new <= -3, 1, 0)) 

df1 <- df1 %>% 
  filter(!is.na(haz_new) | !is.na(whz_new) | !is.na(waz_new)) 

sort(unique(df1$SurveyId))


var <- c(
  ##general
  "SurveyId", "CountryName", "DHSCC", "LATNUM", "LONGNUM", "psu", "bidx", "wt", "hh.id", "woman.id", 
  "region", "residence", "stratum",
  "intYr", "intMo", "intCMC", 
  "birthYr", "birthMo", "birthCMC", "age_child_years", "age_child_months", 
  "birthYr_mother", "birthCMC_mother", "age_mother", "age_mother_at_birth",
  "sex", "bord", "twin",
  "years_at_residence", "resident",
  ##undernutrition
  "whz_new", "wasted", "wasted_severe", "haz_new", "stunted", "stunted_severe", "waz_new", "underweight", "underweight_severe"
)


df1 <- df1 %>% 
  dplyr::select(var) 

sort(unique(df1$intYr))

## Remove countries with only one survey round
df1 <- df1 %>%
  group_by(CountryName) %>%
  mutate(unique_surveys = n_distinct(SurveyId)) %>% 
  ungroup() %>% 
  filter(unique_surveys>1) 
  

unique(df1$SurveyId)
unique(df1$CountryName)

################################################################################
## 4. Add school reform info
################################################################################

data <- df1 %>% 
  ## add primary reform years (from paper: Martin (2024) The intergenerational effect of tuition-free lower-secondary education on children s nutritional outcomes in Africa)
  mutate(primReform_year = NA) %>% 
  mutate(primReform_year = ifelse(CountryName == "Zambia", 2002, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Rwanda", 2003, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Benin", 2006, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Burkina Faso", 2007, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Burundi", 2005, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Mozambique", 2005, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Liberia", 2006, primReform_year)) %>% ## the authors use different years in different papers - 2003 or 2006
  # additional countries from paper: Koski (2018) The impact of eliminating primary school fees on child marriage in SSA
  mutate(primReform_year = ifelse(CountryName == "Cameroon", 2000, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Ethiopia", 1995, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Ghana", 1996, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Kenya", 2003, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Malawi", 1994, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Uganda", 1997, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Lesotho", 2006, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Tanzania", 2002, primReform_year)) %>% 
  # additional countries from paper: Bose & Heymann (2019) Effects of tuition-free primary education on women's access to family planning and on health decision-making: A cross-national study
  mutate(primReform_year = ifelse(CountryName == "Mali", 1999, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Namibia", 2001, primReform_year)) %>% ## exclude exposed cohorts!
  mutate(primReform_year = ifelse(CountryName == "Senegal", 2004, primReform_year)) %>% 
  # additional countries from paper: Bhuwania (2023) Impact of Tuition-Free Education Policy on Child Marriage and Early Childbearing: Does Secondary Matter More?
  mutate(primReform_year = ifelse(CountryName == "Congo Democratic Republic", 2015, primReform_year)) %>% 
  # Other sources
  mutate(primReform_year = ifelse(CountryName == "Togo", 2008, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Sierra Leone", 2018, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Nigeria", 1999, primReform_year)) %>% 
  # Unclear (IMAGINE program implemented in Niger to improve access to schooling for girls in 2008)
  mutate(primReform_year = ifelse(CountryName == "Niger", 2008, primReform_year)) %>%   
  # No information (not previously included in the studies)
  mutate(primReform_year = ifelse(CountryName == "Cote d'Ivoire", NA, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Gabon", NA, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Guinea", NA, primReform_year)) %>% 
  mutate(primReform_year = ifelse(CountryName == "Madagascar", NA, primReform_year)) %>% 
  ## Estimate the age of the mother when the primary and secondary reforms were introduced
  mutate(age_mother_primReform = primReform_year - birthYr_mother) %>% 
  ## Determine exposure to primary reform based on the year when the reform took place and the starting school age at the time
  ## e.g. the primary reform in Zambia happened in 2003 and the starting primary school age in 2003 was 7 (+2 years for late entries)
  mutate(primReformExp = 0) %>% 
  mutate(primReformExp = ifelse(CountryName == "Zambia" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Rwanda" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Liberia" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Benin" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Burkina Faso" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Burundi" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Mozambique" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Cameroon" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Congo Democratic Republic" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Cote d'Ivoire" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Ethiopia" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Gabon" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Ghana" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Guinea" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Kenya" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Lesotho" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Madagascar" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Malawi" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Mali" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Namibia" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Niger" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Nigeria" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Senegal" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Sierra Leone" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Tanzania" & age_mother_primReform <= 7+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Togo" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  mutate(primReformExp = ifelse(CountryName == "Uganda" & age_mother_primReform <= 6+2, 1, primReformExp)) %>% 
  #add survey year 
  mutate(surveyYr = substr(SurveyId, 3, 6)) %>%      
  filter(twin==0)

write.csv(data, "data_for_analysis.csv")


