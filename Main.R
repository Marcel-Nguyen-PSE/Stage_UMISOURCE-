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

qs_2014 <- read_csv('qs_rankings_2014_complete.csv')
qs_2015 <- read_csv('qs_rankings_2015_complete.csv')
qs_2016 <- read_csv('qs_rankings_2016_complete.csv')
qs_2017 <- read_csv('qs_rankings_2017.csv')
qs_2018 <- read_csv('qs_rankings_2018_complete.csv')
qs_2019 <- read_csv('qs_rankings_2019.csv')
qs_2020 <- read_csv('qs_rankings_2020.csv') %>% mutate(year = 2020)
qs_2021 <- read_csv('qs_rankings_2021.csv') %>% mutate(year = 2021)
qs_2022 <- read_csv('qs_rankings_2022.csv') %>% mutate(year = 2022)
qs_2023 <- read_csv('qs_rankings_2023.csv') %>% mutate(year = 2023)
qs_2024 <- read_csv('qs_rankings_2024.csv') %>% mutate(year = 2024)
qs_2025 <- read_csv('qs_rankings_2025.csv') %>% mutate(year = 2025)

qs_list <- list(
  qs_2014,
  qs_2015,
  qs_2016,
  qs_2017,
  qs_2018,
  qs_2019,
  qs_2020,
  qs_2021,
  qs_2022,
  qs_2023,
  qs_2024,
  qs_2025
)

qs_list_clean <- lapply(qs_list, function(df) {
  df %>%
    # convert all columns to character first
    mutate(across(everything(), as.character))
})

qs_panel <- bind_rows(qs_list_clean) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  arrange(university_name, year)

