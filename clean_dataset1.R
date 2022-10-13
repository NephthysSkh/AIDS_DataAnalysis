

library(tidyverse)
library(readxl)


#Load data UNICEF for South Asia, Eastern and Southern Africa, Eastern Europe
# and Central Asia, Middle East and North Africa, Latin America and Caribbean,    
# East Asia and Pacific, West and Central Africa, Western Europe.
HIV_Epidemiology_orig1 <- read_excel('HIV_Epidemiology_Children_Adolescents_2021.xlsx',
                                     sheet = 'Data',
                                     col_names = TRUE,
                                     skip = 1,
                                     guess_max = 2000)


#Get the UNICEF data into tidy format
HIV_Epidemiology1 <- HIV_Epidemiology_orig1 %>% 
  pivot_wider(names_from = Indicator,
              values_from = Value) %>%
  rename(region = `Country/Region`,
         UNICEF_region = `UNICEF Region`,
         data_source = `Data source`,
         incidence_rate = `Estimated incidence rate (new HIV infection per 1,000 uninfected population)`,
         transmission_rate = `Estimated mother-to-child transmission rate (%)`,
         nb_death = `Estimated number of annual AIDS-related deaths`,
         nb_infection = `Estimated number of annual new HIV infections`,
         nb_infected = `Estimated number of people living with HIV`,
         death_rate = `Estimated rate of annual AIDS-related deaths (per 100,000 population)`) %>%
  select(ISO3, UNICEF_region, region, data_source, Year, Sex, Age,
         incidence_rate, death_rate, nb_infection, nb_infected, nb_death) %>%
  mutate(Age = gsub('Age ', '', Age),
         nb_death = gsub(',', '', nb_death),
         nb_infection = gsub(',', '', nb_infection),
         nb_infected = gsub(',', '', nb_infected),
         Age = as.factor(Age),
         Sex = as.factor(Sex),
         id = ISO3,
         data_source = as.factor(data_source),
         region = as.factor(region),
         UNICEF_region = as.factor(UNICEF_region),
         incidence_rate = as.numeric(incidence_rate),
         death_rate = as.numeric(death_rate),
         nb_infection = as.numeric(nb_infection),
         nb_infected = as.numeric(nb_infected),
         nb_death = as.numeric(nb_death)) %>%
  filter(Age == "10-19")

#Divide the data set into tables
table_1 <- HIV_Epidemiology1 %>% 
  select(id, region, UNICEF_region, data_source, Year, Sex, Age, incidence_rate) %>%
  drop_na()

table_2 <- HIV_Epidemiology1 %>% 
  select(id, region, UNICEF_region, data_source, Year, Sex, Age, death_rate) %>%
  drop_na()

table_3 <- HIV_Epidemiology1 %>% 
  select(id, region, UNICEF_region, data_source, Year, Sex, Age, nb_infection) %>%
  drop_na()

table_4 <- HIV_Epidemiology1 %>% 
  select(id, region, UNICEF_region, data_source, Year, Sex, Age, nb_death) %>%
  drop_na()

table_5 <- HIV_Epidemiology1 %>% 
  select(id, region, UNICEF_region, data_source, Year, Sex, Age, nb_infected) %>%
  drop_na()

# Join tables using (region, sex, age, year, data_source) as key
HIV_Epidemiology1 <- full_join(table_1, table_2,
                               by = c("id", "region", "UNICEF_region", 
                                      "Age", "Sex", "Year", "data_source")) %>%
  full_join(., table_3,
            by = c("id", "region", "UNICEF_region",
                   "Age", "Sex", "Year", "data_source")) %>%
  full_join(., table_4,
            by = c("id", "region", "UNICEF_region", 
                   "Age", "Sex", "Year", "data_source")) %>%
  full_join(., table_5, 
            by = c("id", "region", "UNICEF_region",
                   "Age", "Sex", "Year", "data_source"))


head(HIV_Epidemiology1)