# CZ COVID-19 age-specific cases/deaths data
# regional structure - NUTS3 ("kraje)
# Author: Anna Altová, anna.altova at natur.cuni.cz


library(tidyverse)
library(googlesheets4)
library(dplyr)
library(xml2)
library(rvest)
library(lubridate)
library(googledrive)


###########################################
################ CASES ####################
###########################################

# NUTS 3 codes
NUTS3 <- data.frame(
  code = c("CZ010", "CZ020", "CZ031", "CZ032", 
           "CZ041", "CZ042", "CZ051", "CZ052", 
           "CZ053", "CZ063", "CZ064", "CZ071", 
           "CZ072", "CZ080"), 
  name = c("Prague", "Central Bohemian Region", "South Bohemian Region", "Plzen Region", 
           "Karlovy Vary Region", "Usti nad Labe Region", "Liberec Region", "Hradec Kralove Region", 
           "Pardubice Region", "Vysocina Region", "South Moravian Region", "Olomouc Region", 
           "Zlin Region", "Moravian-Silesian Region")
)

# Getting the data from the Health Ministery website
cases_url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.csv" 

cz_cases<-read.csv(cases_url, header = T, 
                   col.names = c("Date",
                                 "Age", # exact age
                                 "Sex2", # M=male, Z=female
                                 "NUTS3", #region of hygiene station which provides data, according to NUTS 3
                                 "LAU1", # region on LAU1 structure
                                 "Abroad_inf", # 1=claimed to be infected abroad
                                 "Country_inf" # claimed country of infection
                   )) %>% 
  mutate(Sex = ifelse(Sex2 == "M","m","f"), 
         Date = as.Date(Date, "%Y-%m-%d")) %>% 
  select(-Sex2)

cz_cases %>%  pull("Age") %>%  unique()

### DATA ON NUTS3 level
cz_cases_region <- 
  cz_cases %>% 
  select("NUTS3",  "Date", "Sex", "Age") %>% 
  mutate(Region = as.factor(NUTS3),
         Value2 = 1, 
         # grouping age
         AgeGroup = case_when(
           Age >= 0 & Age <= 4 ~ 0, 
           Age >= 5 & Age <= 9 ~ 5, 
           Age >= 10 & Age <= 14 ~ 10,
           Age >= 15 & Age <= 19 ~ 15,
           Age >= 20 & Age <= 24 ~ 20,
           Age >= 25 & Age <= 29 ~ 25,
           Age >= 30 & Age <= 34 ~ 30,
           Age >= 35 & Age <= 39 ~ 35,
           Age >= 40 & Age <= 44 ~ 40,
           Age >= 45 & Age <= 49 ~ 45,
           Age >= 50 & Age <= 54 ~ 50,
           Age >= 55 & Age <= 59 ~ 55,
           Age >= 60 & Age <= 64 ~ 60,
           Age >= 65 & Age <= 69 ~ 65,
           Age >= 70 & Age <= 74 ~ 70,
           Age >= 75 & Age <= 79 ~ 75,
           Age >= 80 & Age <= 84 ~ 80,
           Age >= 85 & Age <= 89 ~ 85,
           Age >= 90 & Age <= 94 ~ 90,
           Age >= 95 & Age <= 99 ~ 95,
           Age >= 100 ~ 100
           )
         ) %>% 
  ### select
  select(Region, Date, Sex, AgeGroup, Value2) %>% 
  group_by(Region, Date, AgeGroup, Sex) %>% 
  summarise(Value = sum(Value2)) %>% 
  ungroup() %>% 
  ### complete = Turns implicit missing values into explicit missing values => chci 
  ### vektor ttech vek skupin explicitne
  complete(Region, Date, AgeGroup = seq(min(0), max(100), by=5), Sex, fill = list(Value = 0)) %>% 
  arrange(Region, Date, AgeGroup, Sex) %>% 
  mutate(AgeInt = 5, # what about the 100+? 
         Metric = "Count", 
         Measure = "Cases") %>% 
  select(Region, Date, Sex, AgeGroup, AgeInt, Metric, Measure, Value)


cz_cases_region_ss <- 
  cz_cases_region %>% 
  mutate(Date2 = Date) %>% 
  mutate(#Date = format(as.Date(Date,"%Y-%m-%d"),"%d.%m.%Y"), 
         Country = "Czechia", 
         Code = paste(Region, sep = ".", format(as.Date(Date, "%Y-%m-%d"), "%d.%m.%Y"))) %>% 
  select(Country, Region, Code, Date, Sex, AgeGroup, AgeInt, Metric, Measure, Value, Date2)



###########################################
################ DEATHS ###################
###########################################

# Getting the data from the Health Ministery website
deaths_url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/umrti.csv"

cz_deaths<-read.csv(deaths_url, header = T, 
                   col.names = c("Date", 
                                 "Age", 
                                 "Sex2", 
                                 "NUTS3", # "kraj" - NUTS 3 administrative unit
                                 "LAU1") # "okres" - LAU 1 administrative unit
                   ) %>% 
  mutate(Sex = ifelse(Sex2 == "M","m","f"),
         Date = as.Date(Date, "%Y-%m-%d")) %>% 
  select(-Sex2)

cz_deaths %>%  pull("Age") %>%  unique()

### DATA ON NUTS3 level
cz_deaths_region <- 
  cz_deaths %>% 
  select("NUTS3",  "Date", "Sex", "Age") %>% 
  mutate(Region = as.factor(NUTS3),
         Value2 = 1, 
         # grouping age
         AgeGroup = case_when(
           Age >= 0 & Age <= 4 ~ 0, 
           Age >= 5 & Age <= 9 ~ 5, 
           Age >= 10 & Age <= 14 ~ 10,
           Age >= 15 & Age <= 19 ~ 15,
           Age >= 20 & Age <= 24 ~ 20,
           Age >= 25 & Age <= 29 ~ 25,
           Age >= 30 & Age <= 34 ~ 30,
           Age >= 35 & Age <= 39 ~ 35,
           Age >= 40 & Age <= 44 ~ 40,
           Age >= 45 & Age <= 49 ~ 45,
           Age >= 50 & Age <= 54 ~ 50,
           Age >= 55 & Age <= 59 ~ 55,
           Age >= 60 & Age <= 64 ~ 60,
           Age >= 65 & Age <= 69 ~ 65,
           Age >= 70 & Age <= 74 ~ 70,
           Age >= 75 & Age <= 79 ~ 75,
           Age >= 80 & Age <= 84 ~ 80,
           Age >= 85 & Age <= 89 ~ 85,
           Age >= 90 & Age <= 94 ~ 90,
           Age >= 95 & Age <= 99 ~ 95,
           Age >= 100 ~ 100
         )
  ) %>% 
  select(Region, Date, Sex, AgeGroup, Value2) %>% 
  group_by(Region, Date, AgeGroup, Sex) %>% 
  summarise(Value = sum(Value2)) %>% 
  ungroup() %>% 
  complete(Region, Date, AgeGroup = seq(min(0), max(100), by = 5), Sex, fill = list(Value = 0)) %>% 
  arrange(Region, Date, AgeGroup, Sex) %>% 
  mutate(AgeInt = 5, # what about the 100+? 
         Metric = "Count", 
         Measure = "Deaths") %>% 
  select(Region, Date, Sex, AgeGroup, AgeInt, Metric, Measure, Value)


cz_deaths_region_ss <- 
  cz_deaths_region %>% 
  mutate(Date2 = Date) %>% 
  mutate(#Date = format(as.Date(Date,"%Y-%m-%d"),"%d.%m.%Y"), 
         Country = "Czechia", 
         Code = paste(Region, sep=".", format(as.Date(Date, "%Y-%m-%d"), "%d.%m.%Y")))
  select(Country, Region, Code, Date, Sex, AgeGroup, AgeInt, Metric, Measure, Value, Date2)



# final spreadsheet 
# WITH THE CUMSUM VALUSE :) 

cz_spreadsheet_region <-
  rbind(cz_cases_region_ss, cz_deaths_region_ss) %>% 
  arrange(Date2,Measure, Sex, AgeGroup) %>% 
  group_by(Sex, Region, Measure, AgeGroup) %>% 
  mutate(Value = cumsum(Value)) %>% # here here !!
  ungroup() %>% 
  select(-Date2) %>%  #### + vyfiltrovat cases >20!!!
  filter(Value >= 20 & Measure == "Cases" | Value > 0 & Measure == "Deaths") %>% 
  left_join(NUTS3, by = c("Region" = "code")) %>% 
  mutate(Date = format(Date, "%d.%m.%Y")) %>% 
  select(Country, Region = name, Code, Date, Sex, AgeGroup, AgeInt, Metric, Measure, Value)


# Now, new spreadsheet for the data will probably be neede
# push into the google drive
# sheet_write(cz_spreadsheet_region, 
#             ss <- "NEW SPREADSHEET LINK",
#             sheet="database")



###########################################
############### save .csv #################
###########################################
# dnes <- toString(Sys.Date())
# setwd("D:/Dokumenty/PhD/covid")
# wd <-"D:/Dokumenty/PhD/covid"
# 
# deaths <- read.csv(deaths_url, header = T)
# fn_deaths <- paste(dnes, "covid_deaths_czeReg.csv", sep="_")
# 
# cases <- read.csv(cases_url, header = T)
# fn_cases <- paste(dnes, "covid_cases_czeReg.csv", sep="_")
# 
# write.csv(deaths, file = fn_deaths, row.names = F)
# write.csv(cases, file = fn_cases, row.names = F)

###################################################
############### update the Gdrive #################
###################################################

# this puts the .csv on the drive
# drive_put(media = paste(wd,fn_deaths, sep = "/"), path = g_drive, name = "covid_deaths_czeReg.csv")
# drive_put(media = paste(wd,fn_cases, sep = "/"), path = g_drive, name = "covid_cases_czeReg.csv")

