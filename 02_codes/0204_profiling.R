#-------------------------------------------------------------------
# Project: RPJ paper
# Organization: SFedU Future Skills Research Lab
# Objective: Profiling 
# Author: Valeria Egorova
# Date: 16 June 2023
#-------------------------------------------------------------------

profile_binary <- 
  data_for_glm %>%
  group_by(group_res) %>%
  summarise(SC006 = mean(SC006, na.rm = T),
            SC007 = mean(SC007, na.rm = T),
            SC008 = mean(SC008, na.rm = T),  
            SC009 = mean(SC009, na.rm = T),  
            SC010 = mean(SC010, na.rm = T),
            SC011 = mean(SC011, na.rm = T),
            SC012 = mean(SC012, na.rm = T),
            SC013 = mean(SC013, na.rm = T),
            SC014 = mean(SC014, na.rm = T),
            SC015 = mean(SC015, na.rm = T),
            SC016 = mean(SC016, na.rm = T),
            SC017 = mean(SC017, na.rm = T),
            SC018 = mean(SC018, na.rm = T),
            SC019 = mean(SC019, na.rm = T),
            SC020 = mean(SC020, na.rm = T),
            SC021 = mean(SC021, na.rm = T),
            SC022 = mean(SC022, na.rm = T),
            SC023 = mean(SC023, na.rm = T),
            SC024 = mean(SC024, na.rm = T),
            SC025 = mean(SC025, na.rm = T),
            SC026 = mean(SC026, na.rm = T),
            SC027 = mean(SC027, na.rm = T),
            SC028 = mean(SC028, na.rm = T),
            SC029 = mean(SC029, na.rm = T),
            SC030 = mean(SC030, na.rm = T),
            SC031 = mean(SC031, na.rm = T)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_binary.xlsx"))

profile <- 
  data_for_glm %>%
  mutate(S1 = ifelse(SC004 == 0, 1,0),
         S2 = ifelse(SC004 == 1, 1,0),
         S3 = ifelse(SC004 == 2, 1,0),
         S4 = ifelse(SC004 == 3, 1,0)) %>%
  group_by(group_res) %>%
  summarise(S1 = mean(S1, na.rm = T),
            S2 = mean(S2, na.rm = T),
            S3 = mean(S3, na.rm = T),
            S4 = mean(S4, na.rm = T)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile.xlsx"))