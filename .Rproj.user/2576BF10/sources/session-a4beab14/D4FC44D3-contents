#AQ PK study
#Author - Dhruv Darji
#Exploratory analysis

library(readr)
library(dplyr)
library(tidyverse)
library(iddoverse)
library(labelled)
library(ggplot2)
library(GGally)


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
                                            labels = c("75mg (<12 months)", 
                                                       "150mg (12-60 months)"))
 
          
          ##***** NEED TO CLEAN THE VARIABLE LABELS HERE *** ###
          
  
 #Log-transformed drug concentrations
          dosing_lg$log_deaq <- log(dosing_lg$deaq)
          dosing_lg$log_aq <- log(dosing_lg$aq)
          dosing_lg$log_ppq <- log(dosing_lg$ppq)
          dosing_lg$log_pyr <- log(dosing_lg$pyr)
          dosing_lg$log_sfdxn <- log(dosing_lg$sfdxn)
 
 #Recode AQ dose
          dosing_lg$aq_agedose <- recode(dosing_lg$aq_dose,
                                      "150" = "150mg (12-60 months)",
                                      "75" = "75mg (<12 months)")
                                      
                   
          
 #Label variables
 labels <- c(
   age = "Age (months)",
   aq_dose = "AQ daily dose (mg)",
   aq_agedose = "AQ daily age-based dose (mg)",
   aq_dose_total = "AQ total dose (mg)",
   aq_dose_mgkg = "AQ dose (mg/kg)",
   weight = "Weight (kg)",
   muac = "MUAC (cm)"#,
  # `aq_Day 7` = "Day 7 AQ (ng/mL)",
  #`aq_Day 28` = "Day 28 AQ (ng/mL)",
  # `deaq_Day 7` = "Day 7 DEAQ (ng/mL)",
  # `deaq_Day 28` = "Day 28 DEAQ (ng/mL)",
  # `ppq_Day 7` = "Day 7 PPQ (ng/mL)",
  # `ppq_Day 28` = "Day 28 PPQ (ng/mL)",
  # `pyr_Day 7` = "Day 7 PYR (ng/mL)",
  # `pyr_Day 28` = "Day 28 PYR (ng/mL)",
  # `sfdxn_Day 7` = "Day 7 SFDXN (ng/mL)",
  # `sfdxn_Day 28` = "Day 28 SFDXN (ng/mL)"
 )
 
 for (var in names(labels)) {
   var_label(dosing_lg[[var]]) <- labels[[var]]
 }
 
 
 #Save analysis dataset
 saveRDS(dosing_wd, "output/dosing_wd.rds")
 saveRDS(dosing_lg, "output/dosing_lg.rds")
 
 
 summary(dosing_wd)
 
 
 ##***** START ANALYSES HERE ***
 
 
 #Load analysis datasets
 dosing_wd <- readRDS("output/dosing_wd.rds")
 dosing_lg <- readRDS("output/dosing_lg.rds")
 
 ggpairs(dosing_wd)
 
 #Age vs AQ (mg/kg)
 ggplot(dosing_wd, aes(x = age, y = aq_dose_mgkg)) +
   geom_point() +
   labs(title = "AQ exposure vs age",
        x = "Age (months)", 
        y = "AQ exposure (mg/kg)", 
        )
 
 #Weight vs AQ (mg/kg)
 ggplot(dosing_wd, aes(x = weight, y = aq_dose_mgkg)) +
   geom_point() +
   labs(title = "AQ exposure vs weight",
        x = "Weight (kg)", 
        y = "AQ exposure (mg/kg)", 
   )
 
 
 #Weight vs AQ (mg/kg)
 ggplot(dosing_wd, aes(x = age, y = weight)) +
   geom_point() +
   labs(title = "Age vs weight",
        x = "Age (months)", 
        y = "Weight (kg)", 
   )
 
 
 #Weight vs MUAC
 ggplot(dosing_wd, aes(x = weight, y = muac)) +
   geom_point() +
   labs(title = "Weight vs MUAC",
        x = "Age (months)", 
        y = "Weight (kg)", 
   )
 
 
 
 
 #Raw PK drug concentrations

  #Checking raw level count
 table(dosing_lg$aq_dose, useNA = "ifany")
 table(dosing_wd$aq_dose, useNA = "ifany")

 #Median DEAQ concentrations by AQ dose
 med_deaq28 <- dosing_lg %>%
   filter(TIME == "Day 28", !is.na(aq_agedose), !is.na(deaq)) %>%
   group_by(aq_agedose) %>%
   summarise(med_deaq = median(deaq))
 
 med_deaq7 <- dosing_lg %>%
   filter(TIME == "Day 7", !is.na(aq_agedose), !is.na(deaq)) %>%
   group_by(aq_agedose) %>%
   summarise(med_deaq = median(deaq))
 
 
 #Boxplot / jitterplot for DEAQ concentrations by AQ dose
 ggplot(dosing_lg %>% filter(TIME == "Day 28", !is.na(aq_agedose), !is.na(deaq)), 
        aes(x = TIME, y = `deaq`)) +
   geom_boxplot(outlier.shape = NA) +
   geom_jitter(aes(color = aq_agedose),
               width = 0.1, alpha = 0.6) +
   labs(x = "", 
        y = "DEAQ concentration (ng/mL)", 
        color = "AQ age-based\ndose (daily)")
 
 
 #Histogram of DEAQ concentrations by AQ dose
 ggplot(dosing_lg %>% filter(TIME == "Day 28", !is.na(aq_agedose), !is.na(deaq)), 
        aes(x = deaq, fill = factor(aq_agedose))) +
   geom_histogram(binwidth = 20, 
                  position = "identity",
                  alpha = 0.5,
                  color = "white") +
   geom_vline(data = med_deaq28,
              aes(xintercept = med_deaq),
              linetype = "dashed",
              color = "black",
              linewidth = 1
              ) +
   geom_text(data = med_deaq28,
             aes(x = med_deaq,
                 y = 175,
                 label = round(med_deaq, 1)),
                 color = "black",
                 hjust = -0.05 #moves test slightly away from line
   ) +
   labs(title = "Day 28 DEAQ by AQ dose",
        x = "DEAQ concentration (ng/mL)", 
        y = "Number of particpants",
        fill = "AQ age-based\ndose (daily)")
   
 
 

 #Boxplots log DEAQ by AQ dose / Age on Day 7 and Day 28
 ggplot(dosing_lg %>% filter (!is.na(aq_agedose), !is.na(deaq)), 
        aes(x = TIME, y = `log_deaq`)) +
   geom_boxplot(outlier.shape = NA) +
   geom_jitter(aes(color = aq_agedose),
               width = 0.1, alpha = 0.6) +
   labs(title = "Log DEAQ by AQ dose",
        x = "", 
        y = "log [DEAQ] (ng/mL)", 
        color = "AQ age-based\ndose (daily)")
 
 
 #Age vs DEAQ day 7 and day 28 drug concentrations
 ggplot(dosing_wd, aes(x = age)) +
  # geom_point(aes(y = `deaq_Day 28`), color = "black") +
   geom_point(aes(y = `deaq_Day 7`), color = "red") + 
   scale_x_continuous(breaks = seq(0, 60, by = 12)) +
   labs(y = "DEAQ concentration (ng/mL)", x = "Age (months)")
 
 
 #Weight vs DEAQ day 7 and day 28 drug concentrations
 ggplot(dosing_wd, aes(x = weight)) +
   geom_point(aes(y = `deaq_Day 28`), color = "black") +
   geom_point(aes(y = `deaq_Day 7`), color = "red") + 
   labs(y = "DEAQ concentration (ng/mL)", x = "Weight (kg)")
 
 
 
 
 
 
 
 
 
 
 