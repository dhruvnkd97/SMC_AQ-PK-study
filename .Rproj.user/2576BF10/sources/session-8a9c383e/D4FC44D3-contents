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
  pc <- epqac$`PC.csv 2026-03-20.csv`

  
 #DM data
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
 
 #PC data
 pc$PCSTRESN <- pc$PCORRES    #Replacing values of PCSTRESN, to allow prepare_domain to pick up data

 pc_2 <- prepare_domain(pc,
                        "PC", 
                        timing_variables = c("VISIT", "VISITDY", "EPOCH"),   #Specify timing of variables
                        include_LOC = FALSE)
 
 names(pc_2)[names(pc_2) == "AMODIQNE_\\N"] <- "aq"
 names(pc_2)[names(pc_2) == "DSTHAMDQ_\\N"] <- "deaq"
 names(pc_2)[names(pc_2) == "PIPERQNE_\\N"] <- "ppq"
 names(pc_2)[names(pc_2) == "PYRMTHMN_\\N"] <- "pyr"
 names(pc_2)[names(pc_2) == "SULFADXN_\\N"] <- "sfdxn"
 
 
 
 
 
 #Merge domain data
 dm_vs <- left_join(dm_2, 
                        vs_2 %>% select(USUBJID, weight, muac), 
                        by = "USUBJID")
 
 dosing_lg <- left_join(dm_vs, 
                           pc_2, 
                           by = "USUBJID")
 

 #Reshape from long to wide
 dosing_wd <- pivot_wider(
   data = dosing_lg,
   names_from = "TIME",
   values_from = c("aq", "deaq", "ppq", "pyr", "sfdxn")
 )
  
    #Quick checks for joining
     anti_join(dm_vs, pc_2, by = "USUBJID") #Checks which rows in dm_vs are missing values in pc_2
     nrow(dosing_wd) #Checks number of rows in df
     filter(dosing_wd, USUBJID == "2265") #checks values for all variables for a specific subject ID
 
 
 #Counts of unique study ID by treatment group
 dosing_wd %>%
   group_by(ARMCD) %>%
   summarise(n_subjects = n_distinct(USUBJID)) #DP  496; SPAQ  504; Per clin guide  250??
 
 
 #Remove extra variable
 dosing_wd <- dosing_wd %>% 
   select(-c(AGE, STUDYID.y, TIME_SOURCE))
 
 #Total AQ dose
 dosing_wd <- dosing_wd %>%
   mutate(
     aq_dose_total = aq_dose*3
   )
 
 #AQ mg/kg dose
 dosing_wd <- dosing_wd %>%
   mutate(
     aq_dose_mgkg = aq_dose/weight
   )
 
 
 #Make dosing variables numeric
 dosing_wd <- dosing_wd %>%  
   mutate(across(c(
     `aq_Day 7`, `aq_Day 28`, `aq_NA`,
     `deaq_Day 7`, `deaq_Day 28`, `deaq_NA`,
     `ppq_Day 7`, `ppq_Day 28`, `ppq_NA`,
     `pyr_Day 7`, `pyr_Day 28`, `pyr_NA`,
     `sfdxn_Day 7`, `sfdxn_Day 28`, `sfdxn_NA`,
   ), as.numeric))  
 
 
 dosing_lg <- dosing_lg %>%  
   mutate(across(c(
     `aq`,
     `deaq`,
     `ppq`,
     `pyr`,
     `sfdxn`,
   ), as.numeric))  
 
          dosing_lg$TIME <- factor(dosing_lg$TIME,
                          levels = c("Day 7", "Day 28"))
          
          dosing_lg$aq_dose <- as.character(dosing_lg$aq_dose,
                                            levels = c(75, 150),
                                            labels = c("75mg", "150mg"))
 
 #Label variables
 labels <- c(
   age = "Age (months)",
   aq_dose = "AQ daily dose (mg)",
   aq_dose_total = "AQ total dose (mg)",
   aq_dose_mgkg = "AQ dose (mg/kg)",
   weight = "Weight (kg)",
   muac = "MUAC (cm)",
   `aq_Day 7` = "Day 7 AQ (ng/mL)",
   `aq_Day 28` = "Day 28 AQ (ng/mL)",
   `deaq_Day 7` = "Day 7 DEAQ (ng/mL)",
   `deaq_Day 28` = "Day 28 DEAQ (ng/mL)",
   `ppq_Day 7` = "Day 7 PPQ (ng/mL)",
   `ppq_Day 28` = "Day 28 PPQ (ng/mL)",
   `pyr_Day 7` = "Day 7 PYR (ng/mL)",
   `pyr_Day 28` = "Day 28 PYR (ng/mL)",
   `sfdxn_Day 7` = "Day 7 SFDXN (ng/mL)",
   `sfdxn_Day 28` = "Day 28 SFDXN (ng/mL)"
 )
 
 for (var in names(labels)) {
   var_label(dosing_wd[[var]]) <- labels[[var]]
 }
 
 
 #Save analysis dataset
 saveRDS(dosing_wd, "output/dosing_wd.rds")
 saveRDS(dosing_lg, "output/dosing_lg.rds")
 
 
 summary(dosing_wd)
 
 
 #Age vs AQ (mg/kg)
 ggplot(dm_vs, aes(x = age, y = aq_dose_mgkg)) +
   geom_point()
 #Weight vs AQ (mg/kg)
 ggplot(dm_vs, aes(x = weight, y = aq_dose_mgkg)) +
   geom_point()
 
 #Weight vs AQ (mg/kg)
 ggplot(dm_vs, aes(x = age, y = weight)) +
   geom_point()
 
 #Weight vs AQ (mg/kg)
 ggplot(dm_vs, aes(x = weight, y = muac)) +
   geom_point()
 
 #Age vs DEAQ day 28
 ggplot(dosing_wd, aes(x = age, y = `deaq_Day 28`)) +
   geom_point()
 
 boxplot(dosing_wd$`deaq_Day 28`)
 

 ggplot(dosing_wd, aes(x = "", y = `deaq_Day 28`)) +
   geom_boxplot() +
   geom_jitter(aes(color = factor(aq_dose)),
               width = 0.1, alpha = 0.6) +
   labs(x = NULL, color = "AQ dose")
 

 

 ggplot(dosing_lg, aes(x = TIME, y = `deaq`)) +
   geom_boxplot(outlier.shape = NA) +
   geom_jitter(aes(color = aq_dose),
               width = 0.1, alpha = 0.6) +
   labs(x = "Day", y = "DEAQ (ng/mL)", color = "AQ dose")
 
 
 