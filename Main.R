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
library(plm)

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

tw_panel <- read_csv('THE World University Rankings 2016-2026.csv') %>%
  rename(name = 'Name',
        rank = 'Rank',
        scores_overall = 'Overall Score',
        scores_teaching = 'Teaching',
        scores_research = 'Research Quality',
        location = 'Country') 

tw_panel_2011 <- read_csv('2011_2015_rankings.csv') %>%
  select(-rank_order, -...22, -subjects_offered, -closed, -aliases, -unaccredited, -scores_international_outlook_rank, -scores_research_rank, -scores_citations_rank, -scores_overall_rank, -scores_teaching_rank, -scores_industry_income_rank) 

tw_panel_2011_africa <- tw_panel_2011 %>%
  filter(
    location %in% c(
      "South Africa",
      "Egypt",
      "Nigeria",
      "Kenya",
      "Ghana",
      "Morocco",
      "Tunisia",
      "Uganda",
      "Ethiopia",
      "Algeria",
      "Botswana",
      "Zambia",
      "Zimbabwe",
      "Senegal",
      "Cameroon",
      "Sudan",
      "Rwanda",
      "Tanzania",
      "Mauritius",
      'Madagascar'
    )
  )

tw_panel_merge <- bind_rows(
  tw_panel_2011 %>% mutate(across(everything(), as.character)),
  tw_panel %>% mutate(across(everything(), as.character))
) %>%
  mutate(
    year = as.integer(Year)
  ) %>%
  distinct(name, year, .keep_all = TRUE) %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    n_years    = n_distinct(year)
  ) %>%
  ungroup()

tw_panel_africa <- tw_panel_merge %>%
  filter(
    location %in% c(
      "South Africa",
      "Egypt",
      "Nigeria",
      "Kenya",
      "Ghana",
      "Morocco",
      "Tunisia",
      "Uganda",
      "Ethiopia",
      "Algeria",
      "Botswana",
      "Zambia",
      "Zimbabwe",
      "Senegal",
      "Cameroon",
      "Sudan",
      "Rwanda",
      "Tanzania",
      "Mauritius",
      'Madagascar'
    )
  ) %>% 
  select(-Year) 

pdata <- pdata.frame(
  tw_panel_merge,
  index = c("name", "year")
)

model_persistence <- plm(
  scores_overall ~ lag(scores_overall, 1),
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(model_persistence)

