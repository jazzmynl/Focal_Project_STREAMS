library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(clipr)

CRAM_LA <- read_excel("CRAMSitesCLEAN.xlsx", 
                      sheet = "Table1") %>% 
  select(ecramid)

CRAM_SB <- read_excel("CRAMSitesCLEAN.xlsx", 
                      sheet = "Table3") %>% 
  select(ecramid)

CRAM_SA <- read_excel("CRAMSitesCLEAN.xlsx", 
                      sheet = "Table5") %>% 
  select(ecramid)

ALL_Cram <- rbind(CRAM_LA, CRAM_SA, CRAM_SB)



riverine <- read_csv("ecoatlas_wetlandcondition_riverine.csv")
metrics <- read_csv("ecoatlas_wetlandcondition_metrics.csv")
plants <- read_csv("ecoatlas_wetlandcondition_plants.csv")


Pivot_test <- metrics %>% 
  select(-attributename) %>% 
  pivot_wider(names_from = metricname, values_from = metricscore)


Metric_sites <- Pivot_test %>% 
  filter(ecramid %in% ALL_Cram$ecramid)
