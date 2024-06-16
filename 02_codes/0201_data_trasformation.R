#-------------------------------------------------------------------
# Project: RPJ paper
# Organization: SFedU Future Skills Research Lab
# Objective: Prepare a dataset for the analysis
# Author:  Valeria Egorova
# Date: 16 June 2023
#-------------------------------------------------------------------

# Read the script to uploaded necessary libraries
source(file.path(rcodes, "0200_load_packages.R"))


# Read files and delete differing variables
data_general <- read_excel(file.path(inputData,"data_general.xlsx")) %>%
  mutate(dataset = "general",
         grade = as.numeric(grade),
         edu_m = as.numeric(edu_m),
         edu_f = as.numeric(edu_f)) %>%
  select(-316)


data_lyceum <- read_excel(file.path(inputData,"data_lyceum.xlsx")) %>%
  mutate(dataset = "lyceum", 
         grade = as.numeric(grade)) %>%
  select(-c(2:5), -316)


data_aesc <- read_excel(file.path(inputData,"data_aesc.xlsx")) %>%
  mutate(dataset = "aesc") %>%
  select(-c(2:5), -8)


#Merging datasets in one
data <- 
  data_general %>%
  bind_rows(data_lyceum) %>%
  bind_rows(data_aesc)


#Creating vectors for reversed questions

#self-control reversed scales
scR <- c("PSC02", "PSC03", "PSC04", "PSC05", "PSC07", "PSC09", "PSC10", 
         "PSC11", "PSC13")

#self_efficacy reversed scales
seR <- c("PSE02", "PSE04", "PSE05", "PSE06", "PSE07", "PSE10", "PSE11", 
         "PSE12", "PSE16", "PSE17")

#self_respect reversed scales
srR <- c("PSR02", "PSR05",  "PSR08", "PSR09", "PSR10")

#research_potential reversed scales
rpR <- c("PRP03", "PRP08", "PRP11", "PRP12", "PRP15", "PRP20", "PRP23", 
         "PRP25", "PRP28", "PRP29", "PRP40", "PRP43", "PRP47", "PRP49")

#big five reversed scales
bfR <- c("PBF01", "PBF21", "PBF26", "PBF07", "PBF17", "PBF27",
         "PBF03", "PBF08", "PBF28", "PBF14", "PBF19", "PBF24", 
         "PBF05", "PBF10", "PBF20","PBF30")

#growth_minset reversed scales
gmR <- c("PGM02", "PGM08", "PGM14", "PGM05", "PGM09", "PGM13", "PGM15")

#grit reversed scales 
gR <- c("PG02", "PG04", "PG06", "PG08", "PG10")

var <- c("self_control", "self_efficacy", "educational",
         "communicative", "emotional", "growth", "creativity",
         "position", "achievment", "external", "self_respect", "motivational", 
         "cognitive", "behavioural", "research_potential", "em_knowledge",
         "em_control", "self_mot", "empaty", "em_identification",
         "O", "C", "E", "A", "N", "gm_intelligence", "gm_personality", 
         "externalism",  "commitment", "consistency", "grit", 
         "life_sat",  "AP02", "PI", "sci_env")

#Creating scales and excluding original questions

scaling <- 
  data %>%
  #self-control
  mutate_at(.vars = scR, .fun = ~ abs(.-6)) %>%
  mutate(self_control = PSC01 + PSC02 + PSC03 + PSC04 + PSC05 + PSC06 +
           PSC07 + PSC08 + PSC09 + PSC10 + PSC11 + PSC12 + PSC13) %>%
  #self_effficacy
  mutate_at(.vars = scR, .fun = ~ abs(.-11)) %>%
  mutate(self_efficacy = PSE01 + PSE02 + PSE03 + PSE04 + PSE05 + PSE06 +
           PSE07 + PSE08 + PSE09 + PSE10 + PSE11 + PSE12 + PSE13 +
           PSE14 + PSE15 + PSE16 + PSE17,
         #motivation
         educational = PM02 + PM09 + PM15,
         communicative = PM03 + PM10 + PM16,
         emotional = PM01 + PM08 + PM21,
         growth = PM06 + PM13 + PM19,
         position = PM07 + PM14 + PM20,
         achievment = PM05 + PM12 + PM18,
         external = PM04 + PM11 + PM17) %>%
  #self_respect
  mutate_at(.vars = srR, .fun = ~ abs(.-5)) %>%
  mutate(self_respect = PSR01 + PSR02 + PSR03 + PSR04 + PSR05 + PSR06 +
           PSR07 + PSR08 + PSR09 + PSR10) %>%
  #research_potential
  mutate_at(.vars = rpR, .fun = ~ abs(.-11)) %>%
  mutate(motivational = PRP09 + PRP13 + PRP17 + PRP22 + PRP27 + PRP29 +
           PRP32 + PRP33 + PRP37 + PRP41 + PRP42,
         cognitive = PRP03 + PRP04 + PRP06 + PRP08 + PRP10 + PRP11 + PRP12 +
           PRP14 + PRP15 + PRP16 + PRP20 + PRP23 + PRP25 + PRP28 + PRP36 +
           PRP40 + PRP43 + PRP47 + PRP49 + PRP50,
         behavioural = PRP01 + PRP02 + PRP05 + PRP07 + PRP18 + PRP19 +
           PRP21 + PRP24 + PRP26 + PRP30 + PRP31 + PRP34 + PRP35 + PRP38 +
           PRP39 + PRP44 + PRP45 + PRP46 + PRP48 + PRP51,
         research_potential = motivational + cognitive + behavioural,
         #creativity
         creativity = PC01 + PC02 + PC03 + PC04 + PC05 + PC06 + 
           PC07 + PC08 + PC09 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
         #emotional intelligence
         em_knowledge = PEI01 + PEI02 + PEI04 + PEI17 + PEI19 + PEI25,
         em_control = PEI03 + PEI07 + PEI08 + PEI10 + PEI18 + PEI30,
         self_mot = PEI05 + PEI06 + PEI13 + PEI14 + PEI16 + PEI22,
         empaty = PEI09 + PEI11 + PEI20 + PEI21 + PEI23 + PEI28,
         em_identification = PEI12 + PEI15 + PEI24 + PEI26 + PEI27 + PEI29) %>%
  #Big5  
  mutate_at(.vars = bfR, .fun = ~ abs(.-6)) %>%
  mutate(O = PBF05 + PBF10 + PBF15 + PBF20 + PBF25 + PBF30,
         C = PBF03 + PBF08 + PBF13 + PBF18 + PBF23 + PBF28,
         E = PBF01 + PBF06 + PBF11 + PBF16 + PBF21 + PBF26,
         A = PBF02 + PBF07 + PBF12 + PBF17 + PBF22 + PBF27,
         N = PBF04 + PBF09 + PBF14 + PBF19 + PBF24 + PBF29) %>%
  #growth_mindset
  mutate_at(.vars = gmR, .fun = ~ abs(.-7)) %>%
  mutate(gm_intelligence = PGM01 + PGM03 + PGM07 + PGM11 +
           PGM05 + PGM09 + PGM13 + PGM15,
         gm_personality = PGM04 + PGM06 + PGM10 + PGM12 + PGM02 + 
           PGM08 + PGM14) %>%
  #locus of control
  mutate(externalism = PLC02a + PLC03a + PLC04a + PLC05a + PLC06a + PLC07a + PLC08a + PLC09a + PLC10a + PLC11a +
           PLC12a + PLC13a + PLC14a + PLC15a + PLC16a + PLC17a + PLC18a + PLC19a + PLC20a + PLC21a +
           PLC22a + PLC23a + PLC24a + PLC25a + PLC26a + PLC27a + PLC28a + PLC29a,
         #AP01 = abs(AP01 - 7)
         ) %>%
  #grit
  mutate_at(.vars = gR, .fun = ~ abs(.-6)) %>%
  mutate(commitment = PG01 + PG03 + PG05 + PG07 + PG09,
         consistency = PG02 + PG04 + PG06 + PG08 + PG10,
         grit = commitment + consistency,
  #parential involvement
         PI = rowMeans(select(., c(PI01, PI02, PI03, PI04, PI05)), na.rm = T)) %>%
  select(ID, sex, grade, life_sat, AP01, AP02,  self_control, self_efficacy, educational,
         communicative, emotional, growth,
         position, achievment, external, self_respect, motivational, 
         cognitive, behavioural, research_potential, em_knowledge,
         em_control, self_mot, empaty, em_identification,
         O, C, E, A, N, gm_intelligence, gm_personality, 
         externalism,  commitment, consistency, grit, dataset, PI,
         creativity,  SC001, SC002, SC003, SC004, SC006, SC007, SC008, SC009, SC010,
         SC011, SC012, SC013, SC014, SC015, SC016, SC017, SC018,
         SC019, SC020, SC021, SC022, SC023, SC024, SC025, SC026, 
         SC027, SC028, SC029, SC030, SC031, SC032, SC033, SC034) %>%
  replace_with_na(.,
                  replace = list(grade = c(12,13,17,49,9283737828))) %>%
  mutate(sci_env = rowMeans(select(.,c(SC007, SC006)),
                            na.rm = T),
         dataset = factor(dataset),
         sex = factor(sex),
         grade = factor(grade),
         school = case_when(
           grepl("^9", ID) ~ "9",
           grepl("^14", ID) ~ "14",
           grepl("^62", ID) ~ "62",
           grepl("^64", ID) ~ "64",
           grepl("^67", ID) ~ "67",
           grepl("^77", ID) ~ "77",
           grepl("^83", ID) ~ "83",
           grepl("^01", ID) ~ "01",
           grepl("^1", ID) ~ "01",
           grepl("^4", ID) ~ "sunz",
           grepl("^3", ID) ~ "3",
           TRUE ~ as.character(NA)),
         sc_type = as.factor(case_when(school %in% c("9", "14", "62") ~ "lyceum",
                                       school %in% c("3", "sunz") ~ "uni",
                                       school %in% c("64", "67","77", "83", "01") ~ "gen",
                                       T ~ as.character(NA)))) %>%
  mutate_at(.vars = var, .fun = ~ scale(., center = T, scale = T)) %>%
  mutate(research_potential = as.numeric(research_potential),
         AP02 = as.numeric(AP02),
         PI = as.numeric(PI),
         sci_env = as.numeric(sci_env),
         AP01 = as.factor(AP01),
         ac_ach = as.factor(case_when(AP01 %in% c(1:3) ~ 1, # ТРОЕЧНИКИ
                                      AP01 %in% c(4:5) ~ 2, # ХОРОШИСТЫ
                                      AP01 == 6 ~ 3)), # ОТЛИЧНИКИ
         gender = as.numeric(sex),
         group_res = ifelse(research_potential > mean(research_potential),1,0)) %>%
  write_dta(file.path(outData,"scales.dta"))

