library(tidyverse)
library(readxl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)

qs_2025_2024 <- read_csv('QS_2025_2024.csv') %>%
  rename(university = 'Institution_Name')


qs_2023 <- read_csv('QS_2023.csv') %>%
  rename(university = 'institution') %>%
  mutate(year = as.integer(2023))


qs_2017_2022 <- read_csv('QS_2017_2022.csv') 








