#-------------------------------------------------------------------
# Project: RPJ paper
# Organization: SFedU Future Skills Research Lab
# Objective: Conducting logistic regression and it`s goodness of fit
# Author:  Valeria Egorova
# Date: 16 June 2023
#-------------------------------------------------------------------

data_for_glm <- read_dta(file.path(outData,"scales.dta")) %>%
  filter(grade > 1) %>%
  mutate(grade7 = ifelse(grade == 2,1,0),
         grade8 = ifelse(grade == 3,1,0),
         grade9 = ifelse(grade == 4,1,0),
         grade10 = ifelse(grade == 5,1,0),
         grade11 = ifelse(grade == 6,1,0))

mod2 <- glm(group_res ~ PI + sci_env  +  as.factor(sex) + as.factor(ac_ach) 
            +  as.factor(grade8) + as.factor(grade9) + as.factor(grade10) 
            + as.factor(grade11) + as.factor(sc_type) + grit 
            + gm_intelligence + gm_personality + self_mot + self_efficacy, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod2)

margin <- margins(mod2)
margin

vif(mod2)

PseudoR2(mod2, which = "all")

dat2 <- 
  data_for_glm %>%
  select(group_res, PI,  sci_env,  sex, grade8, grade9, grade10,  grade11,  sc_type,
         growth,  grit,  gm_intelligence, ac_ach) %>%
  drop_na() # check variables as finished

predicts2 <- as.numeric(mod2$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts2), as.factor(dat2$group_res))



lm3 <- read_excel("03_outputs/0302_tables/for_pic.xlsx") %>%
  mutate(value = value * 100)

lm3 <- read_excel("03_outputs/pic.xlsx") %>%
  mutate(value = value * 100,
         name = factor(name, ordered = T, 
                       levels = c("Самомотивация", "Мотивация саморазвития", "Упорство", "Мышление роста: интеллект",  "Исследовательский потенциал",
                                  "Класс: 11", "Класс: 10", "Класс: 9", "Класс: 8",  "Тип образовательного учреждения: Школы при университете",
                                  "Тип образовательного учреждения: Лицеи", "Пол: Женский", "Удовлетворенность успеваемостью", "Успеваемость: Хорошисты", 
                                  "Успеваемость: Отличники", "Обогащенность среды", "Включенность родителей в процесс обучения")))

ggplot(lm3, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "#e38734", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  ylim(-20, 30) +
  labs(y = "Предельные эффекты", x = "")

