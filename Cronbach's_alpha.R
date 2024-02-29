library(tidyverse)
library(ltm)

#Cronbach's alpha for total PPTS

remove_ID <- c(8609541, 9120041, 9147329)

CA_PPTS <- PPTS_com %>%
  dplyr::select("ID","PPTS_Type","PPTS_Response_R") %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031) %>%
  group_by(ID) %>%
  pivot_wider(names_from = PPTS_Type, values_from = PPTS_Response_R) 

CA_PPTS_tot <- CA_PPTS[,-c(1,26)]
  
cronbach.alpha(CA_PPTS_tot)

#Cronbach's alpha for subscales of  PPTS

colnames(CA_PPTS_tot)

affec <- CA_PPTS_tot[,c(2, 10, 15, 17, 21, 27, 28)]
cogn <- CA_PPTS_tot[,c(7, 8, 9, 16, 19, 20, 23)]
inter <- CA_PPTS_tot[,c(4, 6, 18, 25, 26, 11, 3)]
ego <- CA_PPTS_tot[,c(1, 5, 12, 13, 14, 22, 24)]

cronbach.alpha(affec)
cronbach.alpha(cogn)
cronbach.alpha(inter)
cronbach.alpha(ego)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Cronbach's alpha for total QCAE

CA_QCAE <- QCAE_com %>%
  dplyr::select("ID","QCAE_Type","QCAE_Response_R") %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031) %>%
  group_by(ID) %>%
  pivot_wider(names_from = QCAE_Type, values_from = QCAE_Response_R) 

CA_QCAE_tot <- CA_QCAE[,-c(1,18)]

cronbach.alpha(CA_QCAE_tot)


#Cronbach's alpha for subscales of  PPTS

colnames(CA_QCAE_tot)

perspec <- CA_QCAE_tot[,c(2,4,8,9,21,23,24,26,27,30)]
online <- CA_QCAE_tot[,c(10,12,13,15,18,20,25,28,29)]
emotion <- CA_QCAE_tot[,c(5,6,16,17)]
peripheral <- CA_QCAE_tot[,c(1,3,14,22)]
proximal <- CA_QCAE_tot[,c(7,11,19,31)]


cronbach.alpha(perspec)
cronbach.alpha(online)
cronbach.alpha(emotion)
cronbach.alpha(peripheral)
cronbach.alpha(proximal)

#-------------------------------------------------------------------------------

#Demographichs

demographics_met<- demographics %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031) 

demo_total <- demographics_met %>%
  summarise(Total_Age_Mean = mean(Age), 
            Total_Age_SD = sd(Age))

demo_male <- demographics_met %>%
  filter(Gender == 0) %>%
  summarise(Male_Age_MEAN = mean(Age),
            Male_Age_SD = sd(Age),
            Male_Number = n())

demo_female <- demographics_met %>%
  filter(Gender == 1) %>%
  summarise(Female_Age_MEAN = mean(Age),
            Female_Age_SD = sd(Age),
            Female_Number = n())

demo_sum <- demo_male %>%
  cross_join(demo_female)


#-------------------------------------------------------------------------------

#Cronbach's alpha for CE MET

CA_MET_CE_1 <- met_tidy %>%
  filter(Zone == "response_button_text", Label != "example.jpeg") %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031) %>%
  dplyr::select("ID","MET_correct","Label") %>%
  group_by(ID) %>%
  pivot_wider(names_from = Label, values_from = MET_correct) 

CA_MET_CE <- CA_MET_CE_1[,-1]
cronbach.alpha(CA_MET_CE)

#Cronbach's alpha for AE MET

CA_MET_AE_1 <- met_tidy %>%
  filter(Zone == "response_rating_scale_likert_active", Label != "example.jpeg") %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031) %>%
  dplyr::select("ID","MET_response","Label") %>%
  group_by(ID) %>%
  pivot_wider(names_from = Label, values_from = MET_response) 

CA_MET_AE <- CA_MET_AE_1[,-1]
cronbach.alpha(CA_MET_AE)


#-------------------------------------------------------------------------------










