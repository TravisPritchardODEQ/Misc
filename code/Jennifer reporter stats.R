library(tidyverse)
library(openxlsx)
#read in 2018/2020 parameter assessments

load("C:/Users/tpritch/Documents/2018-2020_IR_Database/data/assessment_display.Rdata")

AU_layer <- read.xlsx('E:/Documents/IR2018/Other tools/statistics generator/AU layer.xlsx') %>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr)

joined_BU_summary <- joined_BU_summary %>%
  left_join(AU_layer)

parameter_assessments_no_temp <- joined_BU_summary %>%
  filter(Char_Name != 'Temperature' )

parameter_assessments_no_temp_impaired <- parameter_assessments_no_temp %>%
  filter(str_detect(IR_category, "[54]", negate = FALSE))

number_of_AUs_impaired_no_temp_sum <- parameter_assessments_no_temp_impaired %>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))


total_quantity <- AU_layer %>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))

total_temperature_length <- joined_BU_summary %>%
  filter(Char_Name == 'Temperature' ) %>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))



parameter_assessments_impaired <- joined_BU_summary %>%
  filter(str_detect(IR_category, "[54]", negate = FALSE))%>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))

parameter_assessments_all <- joined_BU_summary %>%
 
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))


number_of_AUs_impaired_no_temp <- length(unique(parameter_assessments_no_temp_impaired$AU_ID))


parameter_assessments_impaired <- joined_BU_summary %>%
  filter(str_detect(IR_category, "[54]", negate = FALSE))

number_of_AUs_impaired_total <- length(unique(parameter_assessments_impaired$AU_ID))

#Just Aquatic Life

no_temp_toptal_miles <- joined_BU_summary %>%
  filter(Char_Name != 'Temperature' )%>%
  filter(str_detect(Beneficial_uses, "Fish and Aquatic Life", negate = FALSE)) %>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))

parameter_assessments_only_AL <- joined_BU_summary %>%
  filter(str_detect(Beneficial_uses, "Fish and Aquatic Life", negate = FALSE))

parameter_assessments_only_AL_impaired <- parameter_assessments_only_AL %>%
  filter(str_detect(IR_category, "[54]", negate = FALSE))

parameter_assessments_only_AL_impaired_no_temp <- parameter_assessments_only_AL_impaired %>%
  filter(Char_Name != 'Temperature' )

AL_impairement_no_temp_miles <- parameter_assessments_only_AL_impaired_no_temp %>%
  select(AU_ID, AU_LenMiles, AU_AreaAcr) %>%
  distinct() %>%
  mutate(type = case_when(str_detect(AU_ID, "LK", negate = FALSE) ~ "Lentic",
                          str_detect(AU_ID, "EB", negate = FALSE) ~ "Lentic",
                          TRUE  ~ "Lotic")) %>%
  group_by(type) %>%
  summarise(Length_mile = sum(AU_LenMiles, na.rm = TRUE),
            area_acres = sum(AU_AreaAcr, na.rm = TRUE))


ggplot(parameter_assessments_only_AL_impaired_no_temp, aes(x=Char_Name)) + 
  geom_bar()+
  labs(title = "Lake Unit Drinking Water Impairements",
       x = NULL, y = "Number of impaired AUs") +
  theme_bw()

number_of_AUs_impaired_total_AL <- length(unique(parameter_assessments_only_AL_impaired$AU_ID))
number_of_AUs_impaired_notemp_AL <- length(unique(parameter_assessments_only_AL_impaired_no_temp$AU_ID))


#total AUs from 2018/2020 GIS layer

total_AUs <- 6875



# Lakes -----------------------------------------------------------------------------------------------------------


library(sf)

waterbody_layer <- st_read('C:/Users/tpritch/Documents/2018-2020 IR geodatabase/WQ_Assessment_2018_20_FINAL.gdb', 'AssessmentUnits_OR_Waterbodies') %>%
  as_tibble()

lakes <- waterbody_layer %>%
  filter(str_detect(AU_ID, "LK", negate = FALSE))

lake_assessments <- joined_BU_summary %>%
  filter(str_detect(AU_ID, "LK", negate = FALSE))

total_n_lk_units <- length(unique(lake_assessments$AU_ID))

fish_consumption_assessments <- lake_assessments %>%
  filter(str_detect(Beneficial_uses, "Fishing", negate = FALSE))

fish_consumption_impairements <- lake_assessments %>%
  filter(str_detect(Beneficial_uses, "Fishing", negate = FALSE)) %>%
  filter(str_detect(IR_category, "[54]", negate = FALSE)) 

levels(fish_consumption_impairements$Char_Name) <- gsub(" ", "\n", levels(fish_consumption_impairements$Char_Name))

ggplot(fish_consumption_impairements, aes(x=Char_Name)) + 
  geom_bar()+
  labs(title = "Lake Unit Fish Consumption Impairements",
    x = NULL, y = "Number of impaired AUs") +
  theme_bw()


drinking_water_assessments <- lake_assessments %>%
  filter(str_detect(Beneficial_uses, "Domestic", negate = FALSE))


drinking_wate_impairements <- lake_assessments %>%
  filter(str_detect(Beneficial_uses, "Domestic", negate = FALSE)) %>%
  filter(str_detect(IR_category, "[54]", negate = FALSE)) %>%
  filter(Char_Name != 'Methylmercury') %>%
  mutate(case_when(Char_Name == 'Polychlorinated Biphenyls (PCBs)' ~ 'Polychlorinated Biphenyls\n(PCBs)',
                   TRUE ~ Char_Name))

ggplot(drinking_wate_impairements, aes(x=Char_Name)) + 
  geom_bar()+
  labs(title = "Lake Unit Drinking Water Impairements",
       x = NULL, y = "Number of impaired AUs") +
  theme_bw()

area_DQ_impairements <- drinking_wate_impairements %>%
  select(AU_ID, AU_AreaAcr) %>%
  distinct()
