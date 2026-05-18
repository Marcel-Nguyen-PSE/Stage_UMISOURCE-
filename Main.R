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
library(openalexR)

options(
  openalex.mailto = "Marcel.Nguyen@ens.psl.eu"
)

oa_fetch(

  entity = "institutions",

  search = "University of Cape Town",

  per_page = 1

)


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
  arrange(university_name, year) %>%
  mutate(
    across(
      where(is.character),
      ~ case_when(
        . %in% c("null", "NULL", "n/a", "N/A", "NA", "", " ") ~ NA_character_,
        TRUE ~ .
      )
    )
  )

tw_panel <- read_csv('/Users/marcel/Stage_UMISOURCE-/THE World University Rankings 2016-2026.csv') %>%
  rename(name = 'Name',
        rank = 'Rank',
        scores_overall = 'Overall Score',
        scores_teaching = 'Teaching',
        scores_research = 'Research Quality',
        location = 'Country') 

tw_panel_2011 <- read_csv('2011_2015_rankings.csv') %>%
  select(-rank_order, -...22, -subjects_offered, -closed, -aliases, -unaccredited, -scores_international_outlook_rank, -scores_research_rank, -scores_citations_rank, -scores_overall_rank, -scores_teaching_rank, -scores_industry_income_rank) 

qs_panel_citations <- qs_panel %>%
  select(year, `Citations per Faculty`, university_name) %>%
  rename(name = 'university_name')

tw_panel_merge <- tw_panel_merge %>%
  left_join(qs_panel_citations,   by = c('name', 'year')) %>%
  distinct(name, year, .keep_all = TRUE) 

university_names <- tw_panel_merge %>%
  distinct(name) %>%
  pull(name)

inst_ids <- map(university_names, \(u) {
  inst <- tryCatch(oa_fetch(entity = "institutions", search = u, verbose = FALSE), error = function(e) NULL)
  tibble(name = u, inst_id = if (is.null(inst) || nrow(inst) == 0) NA_character_ else inst$id[1])
}) |>
  list_rbind()
