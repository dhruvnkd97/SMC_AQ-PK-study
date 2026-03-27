#AQ PK study
#Author - Dhruv Darji
#Exploratory analysis

library(readr)
library(dplyr)
library(tidyverse)
library(iddoverse)
library(labelled)
library(ggplot2)


#Clean SDTM dataset by each domain 
files <- list.files("data/raw", pattern = "\\.csv$", full.names = TRUE)

#Keep only EPQAC data
epqac <- lapply(files, function(f) {
  read.csv(f) %>%
    filter(STUDYID == "EPQAC")
})

#Name by SDTM domain (using names of raw csv files)
  names(epqac) <- basename(files)
    # - Now I have one dataframe per domain in the epqac object
  
  dm <- epqac$`DM.csv 2026-03-20.csv`
  vs <- epqac$`VS.csv 2026-03-20.csv`


##Exploratory analyse    
 #Age
  #Subset with age, sex and treatment arm
 dm_2 <- prepare_domain(dm,
                          "dm",
                          variables_include = c("AGE", "SEX", "ARMCD"))

 dm_2$age  <- as.numeric(dm$AGE)
 
 
 #Allocate dosing for Amodiaquine arms
 dm_2 <- dm_2 %>% 
   mutate(
     aq_dose = case_when(
       ARMCD == "SP-AQ" & age < 12 ~ 75,
       ARMCD == "SP-AQ" & age >= 12 ~ 150,
       TRUE ~ NA_real_
     )
   )
 
 
       #Counts of unique study ID by treatment group
       aq_dosing %>%
         group_by(ARMCD) %>%
         summarise(n_subjects = n_distinct(USUBJID))
       
      

 #VS data
 vs_2 <- prepare_domain(vs,
                        "vs", 
                        variables_include = c("WEIGHT", "MUARMCIR"), 
                        include_LOC = TRUE)
 
 names(vs_2)[names(vs_2) == "WEIGHT_\\N_kg"] <- "WEIGHT_kg"
 names(vs_2)[names(vs_2) == "MUARMCIR_\\N_cm"] <- "MUAC_cm"
 
 vs_2$weight <- as.numeric(vs_2$WEIGHT_kg)
 vs_2$muac <- as.numeric(vs_2$MUAC_cm)
 
 summary(vs_2$weight)
 hist(vs_2$weight)
 summary(vs_2$muac)
 hist(vs_2$muac)
 
 
 #Join the DM and VS data
 aq_dosing <- left_join(dm_2, vs_2 %>% select(USUBJID, weight, muac), 
                        by = "USUBJID")
 
 #Remove extra variable
 aq_dosing <- aq_dosing %>% 
   select(-c(AGE))
 
 #Total AQ dose
 aq_dosing <- aq_dosing %>%
   mutate(
     aq_dose_total = aq_dose*3
   )
 
 #AQ mg/kg dose
 aq_dosing <- aq_dosing %>%
   mutate(
     aq_dose_mgkg = aq_dose/weight
   )
 
 
 
 var_label(aq_dosing$age) <- "Age (months)"
 var_label(aq_dosing$aq_dose) <- "AQ daily dose (mg)"
 var_label(aq_dosing$aq_dose_total) <- "AQ total dose (mg)"
 var_label(aq_dosing$aq_dose_mgkg) <- "AQ dose (mg/kg)"
 var_label(aq_dosing$weight) <- "Weight (kg)"
 var_label(aq_dosing$muac) <- "MUAC (cm)"
 
 
 #Age vs AQ (mg/kg)
 ggplot(aq_dosing, aes(x = age, y = aq_dose_mgkg)) +
   geom_point()
 #Weight vs AQ (mg/kg)
 ggplot(aq_dosing, aes(x = weight, y = aq_dose_mgkg)) +
   geom_point()
 
 #Weight vs AQ (mg/kg)
 ggplot(aq_dosing, aes(x = age, y = weight)) +
   geom_point()
 
 #Weight vs AQ (mg/kg)
 ggplot(aq_dosing, aes(x = weight, y = muac)) +
   geom_point()
 
 

 
