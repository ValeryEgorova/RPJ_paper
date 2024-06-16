#-------------------------------------------------------------------
# Project: RPJ paper
# Organization: SFedU Future Skills Research Lab
# Objective: Checking data for GLM
# Author:  Valeria Egorova
# Date: 16 June 2023
#-------------------------------------------------------------------
data_for_glm <- read_dta(file.path(outData,"scales.dta"))

data_desc <- 
  data_for_glm %>%
  select(growth,  grit,  PI,  sci_env, 
         gm_intelligence, ac_ach, research_potential, gm_personality, self_mot,
         self_efficacy)

varD <- c("growth",  "grit",  "PI",  "sci_env", 
          "gm_intelligence", "ac_ach", "research_potential", "gm_personality", 
          "self_mot", "self_efficacy")

lapply(data_for_glm[varD], function(x) psych::describe(x))

#dh <- 
  #data_for_glm %>%
  #group_by(group_res) %>%
  #summarise(growth = mean( growth, na.rm =  T),  
            #grit = mean(grit , na.rm =  T),  
            #PI = mean(PI , na.rm =  T),  
           #sci_env = mean(sci_env , na.rm =  T), 
            #gm_intelligence = mean(gm_intelligence , na.rm =  T), 
            #research_potential = mean(research_potential , na.rm =  T), 
            #gm_personality = mean(gm_personality , na.rm =  T))

#varsss <- c("growth",  "grit",  "PI",  "sci_env", 
            #"gm_intelligence", "ac_ach", "gm_personality")
#lapply(data_for_glm[c(varsss)], function(x) t.test(x ~ data_for_glm$group_res))


to_check2 <- 
  data_for_glm %>%
  select(group_res, PI,  sci_env,  sex, grade8, grade9, grade10,  grade11,
         growth,  grit,  gm_intelligence, ac_ach, self_efficacy)

corr_matrix2 <- cor(to_check2)
p.mat2 = cor_pmat(to_check2)
ggcorrplot(corr_matrix2,  type = "lower",
           lab = TRUE, p.mat = p.mat2, tl.cex = 10, lab_size = 2.5)
