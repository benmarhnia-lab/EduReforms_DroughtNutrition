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
## 4. Recalculate the wealth index 
## 5. De-normalize the survey weights
## 6. Add school reform information


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
setwd("D:/School reforms/Data/")

################################################################################
## 1. Retrieve the survey data for sub-Saharan Africa
################################################################################
## Set up your DHS credentials 
set_rdhs_config(email = XXX,
                project = XXX)


set_rdhs_config(email = XXX,
                project = XXX,
                config_path = "rdhs.json",
                cache_path = "dhs",
                global = FALSE)

## Make a list of eligible surveys and download them
surveys <- dhs_datasets() %>% 
  #dplyr::filter(SurveyType == "DHS") %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Children's Recode")

## Add world region and filter to Sub-Saharan Africa only
library(countrycode)
surveys$Region <- countrycode(surveys$CountryName, 'country.name', 'region')  
surveys <- surveys %>% 
  subset(Region == "Sub-Saharan Africa") %>% 
  subset(CountryName %in% c("Cameroon", "Malawi", "Nigeria", "Rwanda", "Tanzania", "Benin", "Burundi", "Lesotho", "Namibia", "Sierra Leone", "Togo", "Zimbabwe"))

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
  "hw70", "hw71", "hw72", "hw73", "hw1", "hw2", "hw3", "hw13", "hw16", "hw17", "hw18", "hw19",
  #healthcare during pregnancy, delivery and breastfeeding
  #"m1", "m4", "m5", "m15", "m14", "m18", "m19", "m3a", "m3b", "m3c", "m3d", "m3e", "m3f", "m66",
  #variables for recalculating the wealth index
  "v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b"
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

################################################################################
## 4. Recalculate the wealth index 
################################################################################

#removing upper case letters for easier harmonization
obj <- c("v113", "v115", "v116","v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b")
obj
df1 <- df1 %>%
  mutate_at(vars(obj), funs(tolower(.)))


df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\?")

unique(df1$v127)

df1 <- df1 %>% 
  #Floor material 
  mutate(floor = v127) %>% 
  mutate(floor = ifelse(v127=="mud/clay/earth" 
                        | v127=="mud / clay / earth"
                        | v127=="clay / sand"
                        | v127=="argile, banco"                                
                        | v127=="earth"                                       
                        | v127=="sand"  
                        | v127=="sable"
                        | v127=="dung"                                        
                        | v127=="dirt"                                       
                        | v127=="earth/sand"                                 
                        | v127=="earth /sand"                                
                        | v127=="Earth/ sand"                                
                        | v127=="terre/sable"                                # earth/sand
                        | v127=="natural floor - earth/sand"                 
                        | v127=="natural - earth / sand"                     
                        | v127=="natural floor mud/earth"                    
                        | v127=="natural - dung"                             
                        | v127=="earth, sand"                                                              
                        | v127=="Earth, sand"                                
                        | v127=="dirt/sand"                                                               
                        | v127=="earth/sand/clay"                             
                        | v127=="earth/mud/dung"                             
                        | v127=="earth/sand/mud"                             
                        | v127=="earth / sand"                                 
                        | v127=="earth (\"terra batida\")/sand"              
                        | v127=="earth (terra batida)"                        
                        | v127=="earth (terra n?o batida)" 
                        | v127=="earth (terra n?o batida)"
                        | v127=="earth/sand/clay"                             
                        | v127=="earth, sand, mud"                           
                        | v127=="earth, sand, dung"                          
                        | v127=="earth, sand/dung"                           
                        | v127=="earth/Sand/Mud"                             
                        | v127=="mud/earth/dung"                             
                        | v127=="earth and dung"                             
                        | v127=="animal dung"                                
                        | v127=="dung"                                       
                        | v127=="mud stones"
                        | v127=="dirt/earth"
                        | v127=="earth, mud, dung"
                        | v127=="earth/ sand"
                        | v127=="earth/bamboo"
                        | v127=="earth/bamboo (katcha)"
                        | v127=="earth/clay"
                        | v127=="manure"
                        | v127=="mud mixed with dung"
                        | v127=="natural floor"
                        | v127=="earth (terra batida)"
                        | v127=="earth (terra n?o batida)"
                        | v127=="earth, sand, dung"
                        | v127=="mud plasterwork"
                        | v127=="tierra"
                        | v127=="stone"                                      # not sure
                        | v127=="stones"                                     # not sure 
                        | v127=="11"                                         # natural
                        | v127=="12"                                         # natural 
                        | v127=="13"     
                        | v127=="nothing/unpaved earth" # natural 
                        | v127=="earth/sand", 1, floor)) %>%
  mutate(floor  = ifelse(v127=="palm / bamboo" 
                         | v127=="palm, bamboo"                              
                         | v127=="bamboo"                                    
                         | v127=="palm/bambou"                               
                         | v127=="palm / bamboo"                             
                         | v127=="palm/bamboo"                               
                         | v127=="Wood planks, bamboo, plam"                  # not sure if bamboo or wood planks category
                         | v127=="palm, bamboo, leeds"                       
                         | v127=="palm/bamboo/leeds"                         
                         | v127=="palm/bamboo/leaves"
                         | v127=="palm/ bamboo"
                         | v127=="reed, bamboo"                              
                         | v127=="cane"    
                         | v127=="palms"    
                         | v127=="reed / bamboo"                             
                         | v127== "palm/bamboo", 2, floor)) %>% 
  mutate(floor  = ifelse(v127=="raw wood planks" 
                         | v127=="floor wood planks"                         
                         | v127=="bare wood planks"                          
                         | v127=="wood"                                      
                         | v127=="wood bats (\"tacos de madeira\")"          
                         | v127=="planks"   
                         | v127=="rudimentary floor - wood planks"
                         | v127=="wood planks, palm, bamboo"
                         | v127=="wood/palm/bamboo"
                         | v127=="wood planks, bamboo, plam"                  # not sure 
                         | v127=="rudimentary floor wood planks"             
                         | v127=="rudimentary wood planks"                   
                         | v127=="rudimentary - wood planks"                 
                         | v127=="bois/autres végétaux"                     # wood/other plants
                         | v127=="wood planks"   
                         | v127=="wood plank"
                         | v127=="wood planks/timber"
                         | v127=="wood planks, bamboo, plam"
                         | v127=="bois/autres v?g?taux" 
                         | v127=="house boat"
                         | v127=="tablets / wood planks"
                         | v127=="makeshift/salvaged/improvised materials"
                         | v127=="21"                                        
                         | v127=="22", 2, floor)) %>% 
  mutate(floor  = ifelse(v127=="brick"                                      
                         | v127=="cement / bricks"                            # not sure if cement or brick category
                         | v127=="bricks without cement"                     
                         | v127=="broken bricks"  
                         | v127=="tiles/ bricks"
                         | v127=="bricks"                                    
                         | v127=="brick/concrete" 
                         |  v127=="adobe (paved earth)"  
                         | v127=="adobe", 3, floor)) %>% 
  mutate(floor  = ifelse(v127=="cement"                                      
                         | v127=="ciment"   
                         | v127=="cement, concrete"
                         | v127=="cement, tiles"
                         | v127=="cement/ tile"
                         | v127=="cement/brick"
                         | v127=="cement/gravel"
                         | v127=="stone, brick"
                         | v127=="cement / bricks"                            # not sure 
                         | v127=="cement screed"                             
                         | v127=="cement tiles"                              
                         | v127=="cement tiles (mosaic)"                     
                         | v127=="cement/concrete"                           
                         | v127=="concrete/cement"                           
                         | v127=="concrete, cement"                          
                         | v127=="cement screed"                             
                         | v127=="concrete cement"    
                         | v127=="ceramic tiles/terazzo"
                         | v127=="cement tiles/mosaic"
                         | v127=="cements"
                         | v127=="granite"
                         | v127=="concrete"                                  
                         | v127=="finished - cement", 3, floor)) %>%
  mutate(floor = ifelse(v127=="vinyl or asphalt strips"                     
                        | v127=="asphlat tiles/vynil"                        
                        | v127=="Asphlat tiles/vynil"                        
                        | v127=="vinyl or aspalt strips"                     
                        | v127=="vinyl or asphalt"                           
                        | v127=="vinyl,asphalt strips"                       
                        | v127=="Vinyl, asphalt strips"                      
                        | v127=="vinyl /asphalt strips"                      
                        | v127=="vinyl/ asphalt strips"
                        | v127=="vinyl"  
                        | v127=="vinyl / asphalt"
                        | v127=="vinyl or linoleum"                          
                        | v127=="vinyl, linoleum"                            
                        | v127=="vinyl tile/vinyl carpet"                    
                        | v127=="vinyl, asphalt strips"                      
                        | v127=="floor mat, linoleum, vinyl"                 
                        | v127=="floor mat/linoleum/vinyl"                   
                        | v127=="vinyl(pvc) or asphalt strips"               
                        | v127=="vinyl (pvc) or asphalt strips"              
                        | v127=="finished - vinyl or asphalt strips"         
                        | v127=="floor mat, linoleum, vinyl"                 
                        | v127=="vinyl sheets/tiles"                         # not sure if vinyl or tiles cat
                        | v127=="vinyl tile/vinyl carpet" 
                        | v127=="vinyl / asphalt strips"
                        | v127=="vinyl or alphalt strips"
                        | v127=="vinyl,  linoleum"
                        | v127=="vinyl, linoleum, ceramic"
                        | v127=="vinyl/asphalt strips"
                        | v127=="vynil or asphalt strips"
                        | v127=="lino/gerflex"
                        | v127=="linoleum, carpet"
                        | v127=="linoleum"                                   
                        | v127=="linoleum/rubber carpet", 4, floor)) %>% 
  mutate(floor  = ifelse(v127=="laminated or polished wood"                 
                         | v127=="parquet or polished wood"                  
                         | v127=="Parquet or polished wood"                  
                         | v127=="parquet, polished wood"                   
                         | v127=="parquet,polished wd"                       
                         | v127=="parquet, polished wd"                      
                         | v127==" parquet, polished wood"                   
                         | v127=="parquet /polish wood"                      
                         | v127=="Parquet/ polished wood"                    
                         | v127=="Parquet, polished wood"                    
                         | v127=="finished floor parquet or polished wood"   
                         | v127=="finished - parquet or polished wood"   
                         | v127=="finished floor - parquet or polished wood or laminate"
                         | v127=="finished floor - vinyl or linoleum"
                         | v127=="parket"
                         | v127=='"machimbre" / parquet'
                         | v127=="parquet / polished wood"
                         | v127=="parquet/ polished wood"
                         | v127=="parquet/polished wood"
                         | v127=="polished wood, parquet"
                         | v127=="polished wood/parquet"
                         | v127=="coconut lumber"
                         | v127=="31"                                         # finished
                         | v127=="32"                                         # finished
                         | v127=="33"                                         # finsihed 
                         | v127=="34"                                         # finsihed 
                         | v127=="35"                                         # finished 
                         | v127== "polished wood", 5, floor)) %>% 
  mutate(floor  = ifelse(v127=="ceramic mosaics"                             
                         | v127=="mosaic or tiles" 
                         | v127=="mosaic/tile" 
                         | v127=="ceramic tiles" 
                         | v127=="ceramic tiles, terrazzo"
                         | v127=="Ceramic tiles"                             
                         | v127=="Ceramic tiles/Terazzo"                     
                         | v127=="ceramic tiles/terrazo"                     
                         | v127=="ceramic/terrazzo tiles"                    
                         | v127=="ceramic tiles/ terrazo tiles"              
                         | v127=="ceramic tiles, terazzo"                   
                         | v127=="ceramic/marble/porcelain tiles / terrazo" 
                         | v127=="chips/terrazzo"         
                         | v127=="ceramic / mosaic / tiles"
                         | v127=="mosaic/ceramic"                            
                         | v127=="tile"                                      
                         | v127=="tiles"                                     
                         | v127=="cement tiles/brick"                         # not sure
                         | v127=="ceramic tiles/coastal brick"               
                         | v127== "brick tiles"                               # not sure if tiles or brick cat
                         | v127=="finished - ceramic tiles"                  
                         | v127=="carrelage"                                  # floor tile
                         | v127=="ceramic"
                         | v127=="ceramic tiles, marble chips"
                         | v127=="ceramic tiles, vinyl, bricks"
                         | v127=="ceramic tyles"
                         | v127=="ceramic/ marble tile"
                         | v127=="ceramic/ marmol"
                         | v127=="ceramic/marble tiles"
                         | v127=="ceramic/marble tiles tiles"
                         | v127=="ceramic/marble/granite"
                         | v127=="ceramics tiles"
                         | v127=="granite / marble / ceramic"
                         | v127=="granite, ceramic tiles"
                         | v127=="marble, ceramic tile"
                         | v127=="finished floor - carpeted"
                         | v127=="finished floor - cement"
                         | v127=="finished floor - ceramic or marble tiles"
                         | v127=="chips/terrazo"
                         | v127=="tile, ceramic"
                         | v127=="tile/ cement"
                         | v127=="terrazo"
                         | v127=="terrazzo"
                         | v127=="Tiles/ bricks", 5, floor)) %>%
  mutate(floor = ifelse(v127=="marble"                                       
                        | v127=="marble/ceramic tiles"   
                        | v127=="marbre,carre,granito"
                        | v127=="marble/granite"    
                        | v127=="polished stone / marble / granite"
                        | v127=="ceramic/marble/porcelain/tiles/terrazo"
                        | v127=="polished stone/marble/granite", 5, floor)) %>%
  mutate(floor = ifelse(v127=="other"                                        
                        | v127=="autre"                                      
                        | v127=="oTHER"                                      
                        | v127=="other finished"                             
                        | v127=="autres matériaux moderne"                  # other modern materials
                        | v127=="floating house", 5, floor)) %>% 
  mutate(floor  = ifelse(v127=="carpeted"                                     
                         | v127=="carpet"                                    
                         | v127=="carpet/ rug"
                         | v127=="wall to wall carpet"
                         | v127=="rug, carpet"
                         | v127=="woolen carpets/synthetic carpets"
                         | v127=="woolen carpet/synthetic carpet"
                         | v127=="mats"                                       
                         | v127=="mat"                                       
                         | v127=="finished - carpet"                         
                         | v127=="woolen carpets/ synthetic carpet"          
                         | v127=="autres mat?riaux moderne", 5, floor)) %>% 
  mutate(floor = ifelse(v127=="not a de jure resident"                       
                        | v127=="not a de jure resident"                     
                        | v127=="not a dejure resident"                      
                        | v127=="not de-jure resident"                       
                        | v127=="not de jure"                                
                        | v127=="not de jure resident"                       
                        | v127=="not dejure resident"                        
                        | v127=="not a de jure member"
                        | v127=="not dejure member"
                        | v127=="97"                                         
                        | v127=="99", NA, floor))


aggregate(floor ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

unique(df1$floor)


df1 <- df1 %>%  
  #cooking fuel
  mutate(fuel = v161) %>% 
  mutate(fuel = ifelse(v161=="coal, lignite"                 
                       | v161=="Coal, lignite"               
                       | v161=="coal/lignite"                
                       | v161=="briquette"                    #solid, biofuel subst for coal/charcoal
                       | v161=="charcoal"                    
                       | v161=="cardboard/paper"             
                       | v161=="straw/shrubs/grass"          
                       | v161=="straw / shrubs / grass"      
                       | v161=="maize or other crop waste"   
                       | v161=="agricultural crop"           
                       | v161=="crop waste"                  
                       | v161=="saw dust"                    
                       | v161=="sawdust/wood chips"          
                       | v161=="firewood/straw"              
                       | v161=="firewood, straw"             
                       | v161=="wood chips"                  
                       | v161=="animal dung"   
                       | v161=="animal dung/waste"                  
                       | v161=="garbage/plastic"
                       | v161=="dung"
                       | v161=="improved smokeless chulo"
                       | v161=="coal, wood"
                       | v161=="firewood, charcoal"
                       | v161=="crop residue/grass"
                       | v161=="mineral coal"
                       | v161=="sawdust"
                       | v161=="sawdust / wood chips"
                       | v161=="traditional firewood/charcoal/dung"
                       | v161=="wood"
                       | v161== "processed biomass (pellets) or woodchips"
                       | v161=="firewood"
                       | v161=="9"                            # Straw/shrubs/grass
                       | v161=="17"
                       | v161=="15"
                       | v161=="wood", 0, fuel)) %>% 
  mutate(fuel = ifelse(v161=="biogas" 
                       | v161=="biogaz" 
                       | v161=="bottled gas" 
                       | v161=="piped natural gas"
                       | v161=="natural gas"                 
                       | v161=="natural gas/biogas"
                       | v161=="gasoline/diesel"  
                       | v161=="cylinder gas"
                       | v161=="alcohol/ethanol"
                       | v161=="gasoline"
                       | v161=="propane gas"
                       | v161=="kerosene/paraffin/petroleum"
                       | v161=="kerosene, oil, cocinol, diesel, gasoline, alcohol" , 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="electricity"                   
                       | v161=="electricity from generator"  
                       | v161=="electricity from other source", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="kerosene"                      
                       | v161=="paraffin"  
                       | v161=="paraffin/kerosene"
                       | v161=="liquefied petroleum gas (lpg)/cooking gas"
                       | v161=="oil/paraffin/kerosene"
                       | v161=="kerosene/paraffin"           
                       | v161=="paraffin/kerosine"              
                       | v161=="parafin/ kerosene"           
                       | v161=="jelly"                        # not sure
                       | v161=="petroleum/kerosene", 1, fuel)) %>%  
  mutate(fuel = ifelse(v161=="lpg"                           
                       | v161=="lpg/natural gas" 
                       | v161=="lpg / natural gas"           
                       | v161=="lpg, natural gas"            
                       | v161=="lpg/cylinder gas", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="solar energy"                  
                       | v161=="solar power"                 
                       | v161=="solar", 1, fuel)) %>%    
  mutate(fuel = ifelse(v161=="other", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="no food cooked in house" 
                       | v161=="no cocina"
                       | v161=="no food cooked in hh"        
                       | v161=="no food cooked in household" 
                       | v161=="do not cook"                 
                       | v161=="does not cook" 
                       | v161=="don't cook"  
                       | v161=="no food cooked in house", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="97"                            
                       | v161=="99", NA, fuel)) %>% 
  mutate(fuel = ifelse(v161=="not a de jure resident"        
                       | v161=="not a dejure resident"       
                       | v161=="not de-jure resident"        
                       | v161=="not dejure resident"         
                       | v161=="not dejure member", NA, fuel))



aggregate(fuel ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

unique(df1$fuel)


aggregate(v116 ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)
unique(df1$toilet.improved)

df1$v116 <- str_remove(df1$v116, "\\ÿ")
unique(df1$v116)

df1 <- df1 %>% 
  # Check again if all categories have been included (some seem to be coded as numeric)
  mutate(toilet.improved   = ifelse(v116=="flush toilet"                                 
                                    | v116=="flush"                                      
                                    | v116=="own flush toilet"                           
                                    | v116=="private flush toilet"                       
                                    | v116=="flush to piped sewer system"                 
                                    | v116=="Flush to piped sewer system"                
                                    | v116=="flush to septic tank"                       
                                    | v116=="flush to latrine"                           
                                    | v116=="flush to pit latrine"                       
                                    | v116=="Flush to pit latrine"                       
                                    | v116=="flush to somewhere else"                    
                                    | v116=="flush, don't know where"                    
                                    | v116=="flushed to piped sewer system"              
                                    | v116=="flush other"                                 
                                    | v116=="ventilated improved pit latrine (vip)"       
                                    | v116=="covered pit latrine, with slab"             
                                    | v116=="covered pit latrine with slab"              
                                    | v116=="uncovered pit latrine with slab"            
                                    | v116=="pit latrine with slab"                        
                                    | v116=="pit latrine with washable slab"             
                                    | v116=="pit latrine - with slab"                    
                                    | v116=="composting toilet"                           
                                    | v116=="flush - to pit latrine"                     
                                    | v116=="flush - to piped sewer system"               
                                    | v116=="flush - to septic tank"                     
                                    | v116=="flush - to somewhere else"                  
                                    | v116=="flush - don't know where"                   
                                    | v116=="flush or pour flush toilet"                  
                                    | v116=="flush - don't know"                     
                                    | v116=="flush connected to sewer/with septic tank"  
                                    | v116=="flush to pipe connected to ground water"    
                                    | v116=="flush to sewer system"                      
                                    | v116=="flush to vault (bayara)"                    
                                    | v116=="flush toileet"                              
                                    | v116=="flush toilet to piped sewer system"         
                                    | v116=="flush toilet to pit latrine"                
                                    | v116=="flush toilet to septic tank"                
                                    | v116=="flush toilet to somewhere else"             
                                    | v116=="flush toilet: own"                          
                                    | v116=="flush toilet: shared"                       
                                    | v116=="flust to pipe connected to canal"           
                                    | v116=="flust, don't know where"                    
                                    | v116=="indoors: flush to open pit (ditch or river)"
                                    | v116=="inside yard: flush to open pit (ditch or river)"
                                    | v116=="own flush toilet outsite/yard"              
                                    | v116=="own flust toilet into residence"            
                                    | v116=="share flush toilet into residence"          
                                    | v116=="share flush toilet outside/yard"            
                                    | v116=="share latrine with slab"                    
                                    | v116=="septic tank/modern toilet"                  
                                    | v116=="toilet connected to plot/yard"              
                                    | v116=="toilet connected to septic well"            
                                    | v116=="toilet connected to sewer"                  
                                    | v116=="toilet inside"                              
                                    | v116=="toilet outside"                             
                                    | v116=="vent. imp. pit latr."                       
                                    | v116=="vented improved pit latrine"                
                                    | v116=="ventilated improved privy"                  
                                    | v116=="ventilated/improved pit latrine"            
                                    | v116=="water sealed/slab la"                       
                                    | v116=="water sealed/slab latrine"                  
                                    | v116=="own flush toilet"                          
                                    | v116=="ventilated improved pit latrine"           
                                    | v116=="indoors: flush to piped public system"      
                                    | v116=="indoors: flush to septic tank"              
                                    | v116=="inside yard: flush to piped public system"  
                                    | v116=="inside yard: flush to septic tank"          
                                    | v116=="out of yard: flush to piped public system"  
                                    | v116=="out of yard: flush to septic tank"          
                                    | v116=="out of yard: latrine to septic tank"        
                                    | v116=="indoors: latrine to piped public system"    
                                    | v116=="indoors: latrine to septic tank"            
                                    | v116=="inside yard: latrine to piped public system"
                                    | v116=="inside yard: latrine to septic tank"        
                                    | v116=="out of yard: latrine to piped public system"
                                    | v116=="out of yard: latrine to septic tank"        
                                    | v116=="out of yard: flush to open pit (ditch or river)"
                                    | v116=="toilet with flush"                          
                                    | v116=="toilet without flush"                       
                                    | v116=="improved latrine"                           
                                    | v116=="traditional improved latrine"               
                                    | v116=="improved pit toilet latrine"                
                                    | v116=="improved pit latrine"                       
                                    | v116=="non-vip pit latrine with slab"              
                                    | v116=="Pit latrine - ventilated improved pit (VIP)"
                                    | v116=="pit latrine - ventilated improved pit (vip)"
                                    | v116=="ventilated improved pit latrine (laa)"      
                                    | v116=="vent.imp.pit latrine"                       
                                    | v116=="ventilated improved pit latrine"            
                                    | v116=="ventilated improved pit lat"                
                                    | v116=="ventilated improved pit"                    
                                    | v116=="ventilated improv pt"                       
                                    | v116=="vip latrine"                                
                                    | v116=="vent. imp. pit latrine"                   
                                    | v116=="(vip) latrine/blair toilet"                 
                                    | v116=="ventilated improved pit/latrine (vip blair toilet)"
                                    | v116=="ventilated improved pit (vip) latrine"      
                                    | v116=="ventilated improved pit latrine (vip)"      
                                    | v116=="pit latrine, ventilated improved"           
                                    | v116=="pit latrine, with slab"                     
                                    | v116=="private latrine with slab"                  
                                    | v116=="private with septic tank"                   
                                    | v116=="improved (ventilated) pit latrine"          
                                    | v116=="pit latrine ventilated improved pit latrine"
                                    | v116=="pit latrine - ventilated improved"  
                                    | v116=="uncovered pit latrine, with slab"
                                    | v116=="ventilated latrine"                          #not sure
                                    | v116=="covered latrine"                             #not sure 
                                    | v116=="shared flush toilet"                        
                                    | v116=="public flush toilet"
                                    | v116=="flush toilet connected to a septic tank"
                                    | v116=="flush -  to septic tank"
                                    | v116=="flush toilet connected to sewer system"
                                    | v116=="flush toilet does not know connection"
                                    | v116=="latrine connected to sewer/with septic tank"
                                    | v116=="latrine with siphon"
                                    | v116=="vent imp pit latr"
                                    | v116=="ventimppit latrine"
                                    | v116=="vent imp pit latrine"
                                    | v116=="21"                                         
                                    | v116=="22"                                         
                                    | v116=="11"                                         
                                    | v116=="12"                                         
                                    | v116=="ecosan"                                     
                                    | v116=="composting toilet/ecosan"                   
                                    | v116=="Composting toilet"                          
                                    | v116=="composting toilet/arbo loo"                 
                                    | v116=="mobile chemical toilet"                     
                                    | v116=="modern flush"                               
                                    | v116=="modern flush toilet"                        
                                    | v116=="dry toilet", 1,
                                    ifelse(v116 == "99"                                  
                                           | v116=="non de jure resident"
                                           | v116=="not dejure member"
                                           | v116=="not de jure resident"                
                                           | v116=="not a de jure resident"              
                                           | v116=="not a dejure resident"               
                                           | v116=="not dejure resident"                 
                                           | v116=="not de-jure resident"                
                                           | v116 == "97"
                                           | v116=="", NA, 0)))




## Water
df1$v113 <- str_remove(df1$v113, "\\ÿ")

unique(df1$v113)

df1 <- df1 %>% 
  mutate(water.premise = ifelse(v115=="on premises"                   
                                | v115=="On premises"                   
                                | v115=="delivered to dwelling"         
                                | v115=="delivered water"  
                                | v115=="0"
                                | v115=="996", 1,
                                ifelse(v115=="997"                        #not a dejure resident 
                                       | v115=="999"                     #missing
                                       | v115=="998"                     #dont know
                                       | v115=="don't know ***"         
                                       | v115=="don't know"             
                                       | v115=="Don't know" 
                                       | v115=="dk"             
                                       | v115=="Not a de jure resident" 
                                       | v115=="not a dejure resident"  
                                       | v115=="not a de jure resident" 
                                       | v115=="Not dejure resident"    
                                       | v115=="Not a dejure resident"  
                                       | v115=="not dejure resident", NA, 0 ))) %>% 
  mutate(water.safe = ifelse(v113=="unprotected well"                            
                             | v113=="unprotected spring"                        
                             | v113=="surface water"                            
                             | v113=="surface water(river/dam/lake/pond/stream/canal/irrigation channel" 
                             | v113=="river/dam/lake/ponds/stream/canal/irrigation channel"              
                             | v113=="river/dam/lake/pond/stream/canal/irrigation channel"                
                             | v113=="river/dam/lake/ponds/stream/canal/irirgation channel"               #104
                             | v113=="lake/pond/river/channel/irrigation channel"                        
                             | v113=="river,spring,pond /ma"                     
                             | v113=="pond/lake/dam"                             
                             | v113=="dam/lake/pond"                             
                             | v113=="river,spring,surf. w"                      
                             | v113=="river/stream not protected"                
                             | v113=="pond, river, stream"                       
                             | v113=="river,stream"                              
                             | v113=="river or stream"                           
                             | v113=="lake or stream"                            
                             | v113=="lake/pond/river/channel/irrigation channel"
                             | v113=="pond lake"                                 
                             | v113=="pond,lake"                                 
                             | v113=="pond/lake"                                 
                             | v113=="pond, lake"                                
                             | v113=="sea, lake"                                 
                             | v113=="river/stream"                              
                             | v113=="river, stream"                             
                             | v113=="ocean/lake"                                
                             | v113=="open spring"                               
                             | v113=="other spring"                              
                             | v113=="Spring"                                    
                             | v113=="spring"                                    
                             | v113=="river"                                     
                             | v113=="resevoir"                                  
                             | v113=="canal"                                     
                             | v113=="spring, not improved"                      
                             | v113=="unprotected public well/spring"            
                             | v113=="unprotected well/spring in yard/plot"      
                             | v113=="open well /hole/cesspool in residence"     
                             | v113=="open well /hole/cesspool outside residence"
                             | v113=="unprotected dug well"                      
                             | v113=="Neighbor's open well"                      
                             | v113=="Neighbor's house"                           # not sure                                                         
                             | v113=="well without cover"                        
                             | v113=="well in res, yard"                          # not sure 
                             | v113=="well outside residence"                     # not sure 
                             | v113=="well without handpum"                       # not sure 
                             | v113=="non protected well"                        
                             | v113=="Unprotected well"                          
                             | v113=="....dug - well unprotected"                
                             | v113=="....spring water unprotected"              
                             | v113=="..nile/canals"                             
                             | v113=="34"                                        
                             | v113=="dugout"                                    
                             | v113=="lake, pond"                                
                             | v113=="lake/ pond/ stream"                        
                             | v113=="nile, canal"                               
                             | v113=="open dug well"                             
                             | v113=="open well with sump pump"                  
                             | v113=="open well without sump pump"               
                             | v113=="outside house"                             
                             | v113=="pond/tank/lake"                            
                             | v113=="rier/dam/lake/ponds/stream/canal/irrigation channel"
                             | v113=="river, lake, sea"                          
                             | v113=="river, stream, pond, lake"                 
                             | v113=="river/dam/"                                
                             | v113=="river/stream/pond/lake"                    
                             | v113=="river/stream/pond/lake/dam"                
                             | v113=="river/stream/spring"                       
                             | v113=="semi-protected well"                       
                             | v113=="shallow tubewell"                        
                             | v113=="souce not protected"                       
                             | v113=="source"                                  
                             | v113=="surface water (river/dam)"                 
                             | v113=="surface water (river/dam/lake/pond/stream/canal/irrigation c"
                             | v113=="surface well/other well"                   
                             | v113=="surface/other well"                        
                             | v113=="ubprotected well"                          
                             | v113=="undeveloped spring"                        
                             | v113=="unprotectd well"                           
                             | v113=="unprotected well to yard"                  
                             | v113=="?piped into residence"                     
                             | v113=="?pond/lake"                                
                             | v113=="?public well"                              
                             | v113=="?river/stream"                             
                             | v113=="?spring"                                   
                             | v113=="open well"                                 
                             | v113=="open well in yard/plot"                    
                             | v113=="open well in dwelling"                     
                             | v113=="public well, traditional"                   # not sure 
                             | v113=="public well, cement, not covered"          
                             | v113=="well in compound"                           # not sure
                             | v113=="well in residence"                          # not sure
                             | v113=="?well in residence/yard/plot"              
                             | v113=="..well in yard/plot"                       
                             | v113=="open public well"                          
                             | v113=="well with handpump"                         # not sure 
                             | v113=="well without hndpump"                       # not sure 
                             | v113=="well in dwelling"                           # not sure
                             | v113=="public well, traditional"                   # not sure 
                             | v113=="public well, cement, not covered"           
                             | v113=="well without handpum"                      
                             | v113=="dam"                                        
                             | v113=="other"                                     
                             | v113=="others"                                     # not sure 
                             | v113=="forage"                                     # other
                             | v113=="along the road"                            
                             | v113=="in the courtyard"                           # not sure
                             | v113=="of a neighbor"                              # not sure
                             | v113=="in the house"                               # not sure
                             | v113=="in the courtyard"                           # not sure
                             | v113=="of a neighbor"                              # not sure
                             | v113=="32"                                         # unprotected well
                             | v113=="42"                                         # unprotected spring
                             | v113=="43"                                         # surface water
                             | v113=="22"                                         # not sure 
                             | v113=="23"                                         # not sure
                             | v113=="33"                                         # not sure 
                             | v113=="44"                                         # not sure
                             | v113=="gravity flow scheme"                        # not sure 
                             | v113=="gravity flow water"                         # not sure
                             | v113=="neighbor's tap, nawasa (others recode)"
                             | v113=="neighbor's tap, source unknown (others recode)"
                             | v113=="public well"
                             | v113=="public/neighbor's well"
                             | v113=="well in house/yard/plot"
                             | v113=="well in residence/yard/compound"
                             | v113=="well inside dwelling"
                             | v113=="well into dwelling/yard/plot"
                             | v113=="neighbor's open well"
                             | v113=="public and others unprotected well"
                             | v113=="river/lake"
                             | v113=="spring/stream/waterhole"
                             | v113=="sprong/kuwa"
                             | v113=="river/irrigation channel"
                             | v113=="autre", 0,
                             ifelse(v113=="99"                                   
                                    | v113=="not a dejure resident"              
                                    | v113=="not dejure resident"                
                                    | v113=="not in de jure sample"              
                                    | v113=="not a de jure resident"             
                                    | v113=="not de jure"                        
                                    | v113=="not de-jure resident"               
                                    | v113=="not de jure resident"               
                                    | v113=="not a de-jure resident"             
                                    | v113=="not a dejure place of residence"    
                                    | v113=="97", NA, 1)))  #Note that the updated JMP guidelines from 2017 have been used to classify water source (i.e. packaged or delivered water considered as safe/improved)





aggregate(water.safe ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)
aggregate(toilet.improved ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

df1 %>% 
  group_by(water.safe, v113) %>% 
  summarise(n=n())



unique(df1$v124)

## Ownership of select assets
## NOTE: some missing obs - can we impute them? for now missing ons are assumed to be 0!
df1 <- df1 %>% 
  #Assets - electricity, radio, TV, fridge, bicycle, motorbike, car
  mutate(electricity = ifelse(v119=="yes", 1, 0))  %>% 
  mutate(radio = ifelse(v120=="yes", 1, 0))  %>% 
  mutate(tv = ifelse(v121=="yes", 1, 0))  %>% 
  mutate(fridge = ifelse(v122=="yes", 1, 0))  %>% 
  mutate(bike = ifelse(v123=="yes", 1, 0))  %>% 
  mutate(motorbike = ifelse(v124=="yes", 1, 0)) %>%  
  mutate(car = ifelse(v125=="yes", 1, 0)) 

aggregate(car ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

df1 <- df1 %>% 
  #filter(!is.na(electricity) & !is.na(radio) & !is.na(tv) & !is.na(fridge) & !is.na(bike)& !is.na(motorbike) & !is.na(car) & !is.na(floor) & !is.na(fuel)) %>% 
  mutate(floor = as.numeric(floor)) %>% 
  mutate(fuel = as.numeric(fuel))


## Convert floor material to series of dummies
#install.packages("fastDummies")
library('fastDummies')
df1 <- dummy_cols(df1, select_columns = 'floor')


## Perform PCA
#install.packages("factoextra")
library(factoextra)
library(tidyverse)  

pca.df <- df1 %>% 
  dplyr::select(floor_1, floor_2, floor_3, floor_4, floor_5, radio, tv, bike, motorbike, car) %>% 
  na.omit()

str(pca.df) #all variables need to be numeric

pca.model <- prcomp(pca.df, scale = TRUE)
fviz_eig(pca.model)
pca.pred <- predict(pca.model, newdata = df1)
pca.pred.df <- as.data.frame(pca.pred) %>% 
  dplyr::select(PC1) %>% 
  rename(wealth_score_new = PC1)

df1 <- df1 %>% #add the new scores to the main data
  cbind(pca.pred.df) 


df1 <- df1 %>% 
  mutate(wealth_score_old = v191/1000000) %>% 
  mutate(wealth_score = wealth_score_new)

ggplot(df1, aes(x=wealth_score_old, y=wealth_score)) + geom_point()
hist(df1$wealth_score)

## Generate 5 equally sized wealth groups - 1 (poorest) to 5 (wealthiest)
#install.packages("Hmisc")
library(Hmisc) # cut2
df1$wealth_new <- as.numeric(cut2(df1$wealth_score, g=5))

unique(df1$wealth_new)
df1$wealth <- NA
df1$wealth[df1$wealth_new == 1] <- "poorest"
df1$wealth[df1$wealth_new == 2] <- "poorer"
df1$wealth[df1$wealth_new == 3] <- "middle"
df1$wealth[df1$wealth_new == 4] <- "richer"
df1$wealth[df1$wealth_new == 5] <- "richest"



################################################################################
## 6. Add school reform info
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


