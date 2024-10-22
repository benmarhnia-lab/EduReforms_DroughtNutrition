###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######               School reforms                    ####### 
#######                                                 ####### 
#######               CODE: Analysis                    ####### 
###############################################################
###############################################################


rm(list =ls())

#install.packages("lmtest")
library(lmtest)
library(sandwich)
library(tidyverse)
library(ggpubr)
library(ggplot2)
#install.packages("sjPlot")
library(sjPlot)
library(car)
options(scipen=999)

setwd("D:/School reforms/Data/")

results = data.frame(speiScale=as.character(), ageGr=as.numeric(), primReformExp=as.character() , coef=as.numeric(), SE=as.numeric(), pvalue=as.numeric(), wald.test.pval=as.character())

scale = 2
for (scale in c(2, 3, 4, 5)) {
  
  data <- read.csv(paste("data_for_analysis_spei0", scale, ".csv", sep=""))
  
  countries = c("Benin" , "Burundi", "Lesotho", "Namibia", "Sierra Leone", "Togo", "Zimbabwe", "Cameroon", "Malawi", "Nigeria", "Rwanda", "Tanzania" )
  vars = c("CountryName", "SurveyId", "intYr", "intMo", "wt", "wasted", "sex", "age_child_months", "bord", "age_mother", "birthYr_mother", "primReformExp","spei_intMo", "drought", "wealth_new", "toilet.improved", "water.safe", "gdp_growth",  "urban_pop")
  
  data <- data %>% 
    filter(age_mother>=15 & age_mother<=30) %>% 
    mutate(drop = ifelse(CountryName %in% c("Benin", "Burundi", "Lesotho", "Namibia", "Sierra Leone", "Togo", "Zimbabwe") & primReformExp==1, 1, 0)) %>% 
    filter(drop==0) %>% 
    filter(SurveyId!="BJ2017DHS" & SurveyId!="NM2013DHS") %>% 
    mutate(drought = ifelse(spei_intMo <= -1, 1, 0)) %>% 
    filter(CountryName %in% countries) %>% 
    dplyr::select(all_of(vars)) %>% 
    drop_na() %>% 
    mutate(drought_primReformExpYes = ifelse(drought==1 & primReformExp==1, 1, 0)) %>% 
    mutate(drought_primReformExpNo = ifelse(drought==1 & primReformExp==0, 1, 0))
  
  ### All children under 5
  model_1 <- glm(wasted ~ drought_primReformExpYes + drought_primReformExpNo + primReformExp + as.factor(sex) + as.factor(age_child_months) + bord + as.factor(age_mother) + as.factor(birthYr_mother) + as.factor(intYr) + as.factor(intMo) + as.factor(CountryName) + as.factor(wealth_new) + as.factor(toilet.improved) + as.factor(water.safe) + gdp_growth + urban_pop, weight=wt, family=binomial(link='logit'), data=data)   
  #summary(model_1)
  #exp(coef(model_1))
  #library(broom)
  #tidy(model_1, exponentiate = TRUE)
  #test_1 <- linearHypothesis(model_1, c("drought_primReformExpYes=drought_primReformExpNo"))

  ## Get the robust standard errors clustered at the country level 
  model_1_coef <- coeftest(model_1, vcov. = vcovCL(model_1, cluster = data$CountryName, type = "HC0"))
  model_1_coef_df <- as.data.frame(model_1_coef[,]) %>% 
    as_tibble() %>%
    mutate(variable = rownames(model_1_coef))
  
  ## Extract the results
  results[nrow(results) + 1,] = c(scale, "under 5", "Yes", model_1_coef_df[2,1],  model_1_coef_df[2,2], model_1_coef_df[2,4])
  results[nrow(results) + 1,] = c(scale, "under 5", "No" , model_1_coef_df[3,1],  model_1_coef_df[3,2], model_1_coef_df[3,4])

  ### Children under 3
  data_under3 = data %>% 
    filter(age_child_months<36)
  
  model_2 <- glm(wasted ~ drought_primReformExpYes + drought_primReformExpNo + primReformExp + as.factor(sex) + as.factor(age_child_months) + bord + as.factor(age_mother) + as.factor(birthYr_mother) + as.factor(intYr) + as.factor(intMo) + as.factor(CountryName) + as.factor(wealth_new) + as.factor(toilet.improved) + as.factor(water.safe) + gdp_growth + urban_pop, weight=wt, family=binomial(link='logit'), data=data_under3)   

  ## Get the robust standard errors clustered at the country level 
  model_2_coef <- coeftest(model_2, vcov. = vcovCL(model_2, cluster = data_under3$CountryName, type = "HC0"))
  model_2_coef_df <- as.data.frame(model_2_coef[,]) %>% 
    as_tibble() %>%
    mutate(variable = rownames(model_2_coef))
  
  ## Extract the results
  results[nrow(results) + 1,] = c(scale, "under 3", "Yes", model_2_coef_df[2,1],  model_2_coef_df[2,2], model_2_coef_df[2,4])
  results[nrow(results) + 1,] = c(scale, "under 3", "No" , model_2_coef_df[3,1],  model_2_coef_df[3,2], model_2_coef_df[3,4])

  ### Children under 2
  data_under2 = data %>% 
    filter(age_child_months<24)

  model_3 <- glm(wasted ~ drought_primReformExpYes + drought_primReformExpNo + primReformExp + as.factor(sex) + as.factor(age_child_months) + bord + as.factor(age_mother) + as.factor(birthYr_mother) + as.factor(intYr) + as.factor(intMo) + as.factor(CountryName) + as.factor(wealth_new) + as.factor(toilet.improved) + as.factor(water.safe) + gdp_growth + urban_pop, weight=wt, family=binomial(link='logit'), data=data_under2)   

  ## Get the robust standard errors clustered at the country level 
  model_3_coef <- coeftest(model_3, vcov. = vcovCL(model_3, cluster = data_under2$CountryName, type = "HC0"))
  model_3_coef_df <- as.data.frame(model_3_coef[,]) %>% 
    as_tibble() %>%
    mutate(variable = rownames(model_3_coef))
  
  ## Extract the results
  results[nrow(results) + 1,] = c(scale, "under 2", "Yes", model_3_coef_df[2,1],  model_3_coef_df[2,2], model_3_coef_df[2,4])
  results[nrow(results) + 1,] = c(scale, "under 2", "No" , model_3_coef_df[3,1],  model_3_coef_df[3,2], model_3_coef_df[3,4])
  
  ### Children under 1
  data_under1 = data %>% 
    filter(age_child_months<12)
  
  model_4 <- glm(wasted ~ drought_primReformExpYes + drought_primReformExpNo + primReformExp + as.factor(sex) + as.factor(age_child_months) + bord + as.factor(age_mother) + as.factor(birthYr_mother) + as.factor(intYr) + as.factor(intMo) + as.factor(CountryName) + as.factor(wealth_new) + as.factor(toilet.improved) + as.factor(water.safe) + gdp_growth + urban_pop, weight=wt, family=binomial(link='logit'), data=data_under1)   
  
  ## Get the robust standard errors clustered at the country level 
  model_4_coef <- coeftest(model_4, vcov. = vcovCL(model_4, cluster = data_under1$CountryName, type = "HC0"))
  model_4_coef_df <- as.data.frame(model_4_coef[,]) %>% 
    as_tibble() %>%
    mutate(variable = rownames(model_4_coef))
  
  ## Extract the results
  results[nrow(results) + 1,] = c(scale, "under 1", "Yes", model_4_coef_df[2,1],  model_4_coef_df[2,2], model_4_coef_df[2,4])
  results[nrow(results) + 1,] = c(scale, "under 1", "No" , model_4_coef_df[3,1],  model_4_coef_df[3,2], model_4_coef_df[3,4])

  ### Children 1 to 5
  data_1to5 = data %>% 
    filter(age_child_months>=12)
  
  model_5 <- glm(wasted ~ drought_primReformExpYes + drought_primReformExpNo + primReformExp + as.factor(sex) + as.factor(age_child_months) + bord + as.factor(age_mother) + as.factor(birthYr_mother) + as.factor(intYr) + as.factor(intMo) + as.factor(CountryName) + as.factor(wealth_new) + as.factor(toilet.improved) + as.factor(water.safe) + gdp_growth + urban_pop, weight=wt, family=binomial(link='logit'), data=data_1to5)   
  
  ## Get the robust standard errors clustered at the country level 
  model_5_coef <- coeftest(model_5, vcov. = vcovCL(model_5, cluster = data_1to5$CountryName, type = "HC0"))
  model_5_coef_df <- as.data.frame(model_5_coef[,]) %>% 
    as_tibble() %>%
    mutate(variable = rownames(model_5_coef))
  
  ## Extract the results
  results[nrow(results) + 1,] = c(scale, "1 to 5", "Yes", model_5_coef_df[2,1],  model_5_coef_df[2,2], model_5_coef_df[2,4])
  results[nrow(results) + 1,] = c(scale, "1 to 5", "No" , model_5_coef_df[3,1],  model_5_coef_df[3,2], model_5_coef_df[3,4])
  
  ### Children 2 to 5
  data_2to5 = data %>% 
    filter(age_child_months>=24)
  
  model_6 <- glm(wasted ~ drought_primReformExpYes + drought_primReformExpNo + primReformExp + as.factor(sex) + as.factor(age_child_months) + bord + as.factor(age_mother) + as.factor(birthYr_mother) + as.factor(intYr) + as.factor(intMo) + as.factor(CountryName) + as.factor(wealth_new) + as.factor(toilet.improved) + as.factor(water.safe) + gdp_growth + urban_pop, weight=wt, family=binomial(link='logit'), data=data_2to5)   
  
  ## Get the robust standard errors clustered at the country level 
  model_6_coef <- coeftest(model_6, vcov. = vcovCL(model_6, cluster = data_2to5$CountryName, type = "HC0"))
  model_6_coef_df <- as.data.frame(model_6_coef[,]) %>% 
    as_tibble() %>%
    mutate(variable = rownames(model_6_coef))
  
  ## Extract the results
  results[nrow(results) + 1,] = c(scale, "2 to 5", "Yes", model_6_coef_df[2,1],  model_6_coef_df[2,2], model_6_coef_df[2,4])
  results[nrow(results) + 1,] = c(scale, "2 to 5", "No" , model_6_coef_df[3,1],  model_6_coef_df[3,2], model_6_coef_df[3,4])
  
  
}

write.csv(results, "D:/School reforms/Results/results.csv")

## Wald test to determine statistically significant differences in the outcomes: https://www.geeksforgeeks.org/how-to-perform-a-wald-test-in-r/#

### Plot the results
setwd("D:/School reforms/Results/")

results <- read.csv("results.csv")[-1]

results <- results %>% 
  mutate(CILow  = coef - 1.96*SE,               #add 95% CIs
         CIHigh = coef + 1.96*SE) %>% 
  mutate(OR = exp(coef),
         CILow = exp(CILow),
         CIHigh = exp(CIHigh)) 

unique(results$speiScale)  


results <- results %>% 
  mutate(ageGr = ifelse(ageGr=="under 5", "Age <5 years", 
                        ifelse(ageGr=="under 3", "Age <3 years", 
                               ifelse(ageGr=="under 2", "Age <2 years",
                                      ifelse(ageGr=="under 1", "Age <1 year",
                                             ifelse(ageGr=="1 to 5", "Age 1 to 5 years",
                                                    ifelse(ageGr=="2 to 5", "Age 2 to 5 years", NA))))))) %>% 
  mutate(speiScale = ifelse(speiScale=="2", "2-month drought",
                            ifelse(speiScale=="3", "3-month drought",
                                   ifelse(speiScale=="4", "4-month drought", 
                                          ifelse(speiScale=="5", "5-month drought", "6-month drought")))))


ord1 <- c("Age <5 years", "Age 1 to 5 years", "Age <1 year", "Age <3 years", "Age 2 to 5 years", "Age <2 years")
results$ageGr <- factor(results$ageGr,levels=ord1)
results <- results %>% mutate(ageGr = fct_reorder(ageGr, desc(ageGr))) 

ord2 <- c("2-month drought", "3-month drought", "4-month drought", "5-month drought")
results$speiScale <- factor(results$speiScale,levels=rev(ord2))
results <- results %>% mutate(speiScale = fct_reorder(speiScale, desc(speiScale))) 

results_1 <- results %>% 
  filter(speiScale=="2-month drought") %>% 
  filter(ageGr %in% c("Age <5 years", "Age <1 year", "Age 1 to 5 years"))

results_2 <- results %>% 
  filter(ageGr=="Age <5 years") %>% 
  filter(speiScale %in% c("3-month drought", "4-month drought", "5-month drought"))

### Prepare figures

plot_a <-  ggplot(results_1, aes(OR, primReformExp, colour = ageGr)) +
    geom_point(size = 3.5) +
    geom_errorbar(aes(xmin = CILow, xmax = CIHigh), width = 0, size=0.75) +
    geom_vline(xintercept = 1, lty = 3, colour="darkgray") +
    facet_wrap(~ageGr) + #, scales = "free_y"
    coord_flip() +
    labs(
    x = "Odds ratio",
    y = "Mother's cohort exposed to free primary education") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
    theme_classic() +  # Nicer theme
    #theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
    theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
    theme(text = element_text(size = 14),  axis.ticks = element_blank(), panel.border = element_blank()) +           
    theme(legend.position = "none") +
    theme(panel.spacing=unit(0, "cm", data=NULL)) +
    theme(strip.text = element_text(face = "italic")) +
    #scale_x_continuous(limits = c(0.8, 1.55)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 12))
    
    

 plot_b <- ggplot(results_2, aes(OR, primReformExp, colour = speiScale)) +
    geom_point(size = 3.5) +
    geom_errorbar(aes(xmin = CILow, xmax = CIHigh), width = 0, size=0.75) +
    geom_vline(xintercept = 1, lty = 3, colour="darkgray") +
    facet_wrap(~speiScale) + #, scales = "free_y"
    coord_flip() +
    labs(
      x = "Odds ratio",
      y = "Mother's cohort exposed to free primary education") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
    theme_classic() +  # Nicer theme
    #theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
    theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
    theme(text = element_text(size = 14),  axis.ticks = element_blank(), panel.border = element_blank()) +           
    theme(legend.position = "none") +
    theme(panel.spacing=unit(0, "cm", data=NULL)) +
    theme(strip.text = element_text(face = "italic")) +
    scale_x_continuous(limits = c(0.8, 1.55))  +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 12))
  

plot_ab = ggarrange(plot_a, plot_b, align = "h", ncol = 1, nrow=2, labels = c("a", "b")) 
plot_ab

save_plot("Fig_1.png", fig = plot_ab, width=17, height=13)
save_plot("Fig_1.svg", fig = plot_ab, width=17, height=13)



