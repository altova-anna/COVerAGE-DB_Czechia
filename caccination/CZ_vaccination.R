# CZ COVID-19 vaccination
# age specific data, regional (NUT3 level) 
# author: Anna Altova, Charles University, anna.altova at natur.cuni.cz

library(tidyverse)
library(googlesheets4)
library(dplyr)
library(xml2)
library(rvest)
library(lubridate)
library(googledrive)

#############
vaccine_url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/ockovani.csv"

vaccines <- read.csv(vaccine_url, 
                     header = T, 
                     encoding = "UTF-8",
                     col.names = c("Date", 
                                   "vaccine", # Comirnaty, Moderna, AstraZeneca
                                   "NUTS3", 
                                   "Region", # names of the regions
                                   "AgeGroup",
                                   "first_dose", 
                                   "second_dose", 
                                   "n_dose")) %>% 
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

# unfortunatelly only age 80+ is registered, although the 80+ group 
# is the group of first preference in vaccination

cz_vaccines <- vaccines %>% 
  mutate(Region = as.factor(Region), 
         NUTS3 = as.factor(NUTS3)) %>% 
  complete(nesting(NUTS3,Region), Date, AgeGroup, vaccine, fill = list(first_dose = 0, 
                                                               second_dose = 0, 
                                                               n_dose = 0)) %>% 
  arrange(Date, Region, NUTS3, AgeGroup, vaccine) %>%
  group_by(Region, NUTS3, AgeGroup, vaccine) %>% 
  mutate(Vaccination1 = cumsum(first_dose), 
         Vaccination2 = cumsum(second_dose), 
         Vaccinations = cumsum(n_dose)) %>% 
  ungroup() %>% 
  select(Date, Region, NUTS3, AgeGroup, VaccineType = vaccine, 
         Vaccination1, Vaccination2, Vaccinations) %>% 
  pivot_longer(-c(Date, Region, NUTS3, AgeGroup, VaccineType), names_to = "Measure", values_to = "Value") %>% 
  mutate(AgeGroup = na_if(AgeGroup, "neza�azeno"), 
         Country = "Czechia") %>% 
  select(Country, Region, NUTS3, Date, AgeGroup, VaccineType, Measure, Value)

# comments: As of today (15 Feb 2021) only selected population groups 
# are allowed to get the vaccine. These are people over 80 y.o., medical 
# proffessionals (with the priority of those working with the COVID-19+ patients etc), 
# members of the army helping to handle the pandemic, members of the emergency services, 
# other workers in the critical infrastructure for the pandemic.
# Kids <18 should not be vaccinated (although there are some according to the data)
  
