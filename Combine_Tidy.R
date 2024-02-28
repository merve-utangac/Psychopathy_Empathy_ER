library(tidyverse)
library(dplyr)
library(visdat)
library(xlsx)
library(Hmisc) 
library(MASS)
library(car) 
library(olsrr) 
library(purrr)
library(ggcorrplot)
library(performance) 
library(readxl) 

#COMBINING DIFFERENT DATA SET

#Combining different MET files from different versions of the study and tidying.

met_files <- c("MET_v4.csv",
         "MET_v5.csv",
         "MET_v7.csv")

met_tidy <- lapply(met_files, read_csv) %>% 
  bind_rows() %>%
  dplyr::select("Participant Private ID", "Response", "Correct","Zone Type","image_label") %>%
  set_names(c("ID", "MET_response", "MET_correct", "Zone", "Label")) 

met_sum_ce <- met_tidy %>%
  filter(Zone == "response_button_text", Label != "example.jpeg") %>%
  group_by(ID) %>%
  summarise(MET_SUM_CE = sum(MET_correct))

met_mean_ae <- met_tidy %>%
  filter(Zone == "response_rating_scale_likert_active", Label != "example.jpeg") %>%
  group_by(ID) %>%
  mutate(MET_response = as.numeric(MET_response)) %>%
  summarise(MET_MEAN_AE = mean(MET_response)) %>%
  round(digits = 2)
  
MET <- met_sum_ce %>%
  full_join(met_mean_ae) %>%
  mutate(ID = factor(ID))


#Combining different Face Recognition files from different versions of the study and tidying. 

ER_files <- c("Face_recognition_v4.csv",
         "Face_recognition_v5.csv",
         "Face_recognition_v7.csv")

ER_tidy_1 <- lapply(ER_files, read_csv) %>% 
  bind_rows() 

ER_tidy <- ER_tidy_1 %>%
  dplyr::select("Participant Private ID", "Response","Spreadsheet Row", "Reaction Time","Correct") %>%
  set_names(c("ID", "ER_Response", "Row","ER_RT","ER_Correct")) %>%
  mutate(ID = factor(ID),
         Row = as.numeric(Row),
         ER_Correct = as.numeric(ER_Correct),
         ER_RT = as.numeric(ER_RT))

ER_RT <- ER_tidy_1 %>%
  dplyr::select("Participant Private ID","Reaction Time", "Response") %>%
  set_names(c("ID", "ER_RT", "ER_Response")) %>%
  filter(ER_RT != "NA", ER_Response != "NA") %>%
  group_by(ID, ER_Response) %>% 
  summarise(ER_RT = mean(ER_RT)) %>%
  mutate(ID = factor(ID))

ER_RT_cor <- ER_RT %>%
  dplyr::select("ID", "ER_RT") %>%
  group_by(ID) %>%
  summarise(ER_RT = mean(ER_RT))

ER <- read_excel("ER_tidy.xlsx") %>%
  filter(ER_Response != "NA") %>%
  filter(ER_Correct == "1") %>%
  mutate(ID = factor(ID),
         ER_Correct = as.numeric(ER_Correct))

ER_Intensity <- ER %>%
  group_by(ID, ER_Intensity) %>% 
  summarise(ER_SUM = sum(ER_Correct)) %>%
  pivot_wider(names_from = ER_Intensity, values_from = ER_SUM) %>%
  rename(LOW_ER_SUM = Low,
         HIGH_ER_SUM = High,
         NEUT_ER_SUM = Neutral)


ER_Intensity_ANOVA <- ER %>%
  dplyr::select("ID","ER_Response", "ER_Intensity", "ER_Correct") %>%
  group_by(ID,ER_Response, ER_Intensity) %>%
  summarise(ER_SUM = sum(ER_Correct))


ER_Total <- ER %>%
  group_by(ID) %>% 
  summarise(ER_SUM_ALL = sum(ER_Correct)) 

str(ER_Total)

ER_Emotions_Longer <- ER %>%
  group_by(ID, ER_Response) %>% 
  summarise(ER_SUM = sum(ER_Correct)) 

ER_Emotions_Wider <- ER_Emotions_Longer %>%
  pivot_wider(names_from = ER_Response, values_from = ER_SUM)

ER_Emotions_Wider <- replace(ER_Emotions_Wider,is.na(ER_Emotions_Wider),0)
                               
#Reverse Coding for PPTS

PPTS_scale <- 1:5
reverse_questions_PPTS <- c("Q10_Cognitive_Responsiveness_Reverse", "Q22_Cognitive_Responsiveness_Reverse")

PPTS_reversed <- PPTS_com %>%
  mutate(PPTS_Response_R = case_when(PPTS_Type %in% reverse_questions_PPTS ~ min(PPTS_scale) - PPTS_Response + max(PPTS_scale),
                                     TRUE ~ as.integer(PPTS_Response)))

#Combining PPTS-R files from different versions of the study and tidying. 

PPTS_files <- c("PPTS-R_v4.csv",
                "PPTS-R_v5.csv",
                "PPTS-R_v7.csv")

PPTS_com <- lapply(PPTS_files, read_csv) %>% 
  bind_rows() %>%
  filter(Key == "quantised") %>%
  dplyr::select("Participant Private ID","Response", "Object Name") %>%
  set_names("ID", "PPTS_Response", "PPTS_Type") %>%
  mutate(ID = factor(ID),
         PPTS_Response = as.numeric(PPTS_Response)) %>%
  mutate(PPTS_Response_R = case_when(PPTS_Type %in% reverse_questions_PPTS ~ min(PPTS_scale) - PPTS_Response + max(PPTS_scale),
                                     TRUE ~ as.integer(PPTS_Response))) 

PPTS_ss <- PPTS_com %>%
  mutate(psychopathy_type = case_when(
    endsWith(PPTS_Type, "Affective_Responsiveness") ~ "Affective_Responsiveness_PPTS",
    endsWith(PPTS_Type, "Egocentricity") ~ "Egocentricity_PPTS",
    endsWith(PPTS_Type, "Manipulation") ~ "Interpersonal_Manipulation_PPTS",
    endsWith(PPTS_Type, "Cognitive_Responsiveness") ~ "Cognitive_Responsiveness_PPTS",
    endsWith(PPTS_Type, "Cognitive_Responsiveness_Reverse") ~ "Cognitive_Responsiveness_PPTS",
    endsWith(PPTS_Type, "Check") ~ "PPTS_Attention"),
    .after = psychopathy_type) %>%
  dplyr::select("ID", "psychopathy_type", "PPTS_Response_R") 
  

PPTS <- PPTS_ss %>%
  pivot_wider(names_from = psychopathy_type, values_from = PPTS_Response_R, values_fn = list(PPTS_Response_R = sum)) %>%
  rowwise() %>%
  mutate(PPTS_SUM = sum(c_across(2:5)))

#Creating variables for reverse scores

QCAE_scale <- 1:4
reverse_questions_QCAE <- c("Q1_CE_Online_Simulation_R", "Q2_AE_Peripheral_Responsivity_R",
                            "Q17_AE_Peripheral_Responsivity_R","Q29_AE_Peripheral_Responsivity_R")

QCAE_reversed <- QCAE %>%
  mutate(QCAE_Response_R = case_when(QCAE_Type %in% reverse_questions_QCAE ~ min(QCAE_scale) - QCAE_Response + max(QCAE_scale),
                                     TRUE ~ as.integer(QCAE_Response)))


#Combining QCAE files from different versions of the study and tidying. 

QCAE_files <- c("QCAE_v4.csv",
                "QCAE_v5.csv",
                "QCAE_v7.csv")

QCAE_com <- lapply(QCAE_files, read_csv) %>% 
  bind_rows() %>%
  filter(Key == "quantised") %>%
  dplyr::select("Participant Private ID","Response", "Object Name") %>%
  set_names("ID", "QCAE_Response", "QCAE_Type") %>%
  mutate(empathy_type = case_when(
    endsWith(QCAE_Type, "al_Responsivity_R") ~ "QCAE_AE",
    endsWith(QCAE_Type, "al_Responsivity") ~ "QCAE_AE",
    endsWith(QCAE_Type, "mal_Responsivity") ~ "QCAE_AE",
    endsWith(QCAE_Type, "Taking") ~ "QCAE_CE",
    endsWith(QCAE_Type, "Contagion") ~ "QCAE_AE",
    endsWith(QCAE_Type, "Check") ~ "QCAE_Attention",
    endsWith(QCAE_Type, "Simulation") ~ "QCAE_CE",
    endsWith(QCAE_Type, "Simulation_R") ~ "QCAE_CE"),
    .after = empathy_type) %>%
  mutate(QCAE_Response = as.numeric(QCAE_Response),
          ID = factor(ID)) %>%
  mutate(QCAE_Response_R = case_when(QCAE_Type %in% reverse_questions_QCAE ~ min(QCAE_scale) - QCAE_Response + max(QCAE_scale),
                                     TRUE ~ as.integer(QCAE_Response))) 


QCAE <- QCAE_com %>%
  dplyr::select("ID", "empathy_type", "QCAE_Response_R") %>%
  pivot_wider(names_from = empathy_type, values_from = QCAE_Response_R, values_fn = list(QCAE_Response_R = sum))

#Reading and tidying demographics

demographics <- read_excel("Demograhics.xlsx") %>%
  rename(ID = `Participant Private ID`) %>%
  dplyr::select("ID","Response_Numbered","Question") %>%
  pivot_wider(names_from = Question, values_from = Response_Numbered) %>%
  mutate(ID = factor(ID))


#Combining all tasks by taking demographics as a base data. 

final_data <- demographics %>%
  full_join(MET) %>%
  full_join(PPTS) %>%
  full_join(QCAE) %>%
  full_join(ER_Total) %>%
  full_join(ER_Emotions_Wider) %>%
  na.omit(final_data) %>%
  filter(QCAE_Attention == 4 & PPTS_Attention == 5) %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031)

str(final_data)

vis_miss(final_data)

lapply(final_data,as.numeric) 

write.csv(final_data, "final_data.csv")
write.xlsx(final_data, "final_data.xlsx")


final_data %>% 
  summary(Age)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#POWER ANALYSIS CALCULATION

install.packages("pwr")
library(pwr)

pwr.anova.test(k = 10, f = 0.1 , sig.level = 0.01 , power = 0.80)

