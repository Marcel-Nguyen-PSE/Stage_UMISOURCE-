df_africa <- read_csv('df_africa.csv')
results <- read_csv('results_inter.csv')

df_africa <- df_africa %>%
  dplyr::select(-n_works)


library(dplyr)
library(purrr)
library(tibble)

#------------------------------------------------------------
# Variable summary table
#------------------------------------------------------------

variable_summary <- tibble(
  Variable = names(df_africa)
) %>%
  mutate(
    Type = map_chr(df_africa, ~ class(.x)[1]),
    Missing = map_int(df_africa, ~ sum(is.na(.x))),
    Unique = map_int(df_africa, ~ n_distinct(.x, na.rm = TRUE)),
    `Part missing` = round(
      map_dbl(df_africa, ~ mean(is.na(.x))),
      3
    )
  )

variable_summary



library(dplyr)
library(purrr)
library(tibble)

# Variables to include
vars <- c(
  "n_publication",
  "gdp_cap",
  "gdp_growth",
  "internet",
  "electricity",
  "tertiary_enrol",
  "education_exp",
  "rd_exp",
  "researchers_per_million",
  "n_citations",
  "n_international_any",
  "international_share_any",
  "n_inter_african",
  "inter_african_share",
  "n_extra_african",
  "extra_african_share"
)

variable_summary <- tibble(
  Variable = vars
) %>%
  mutate(
    Type = map_chr(df_africa[vars], ~ class(.x)[1]),
    Missing = map_int(df_africa[vars], ~ sum(is.na(.x))),
    Unique = map_int(df_africa[vars], ~ n_distinct(.x, na.rm = TRUE)),
    `Part missing` = round(
      map_dbl(df_africa[vars], ~ mean(is.na(.x))),
      3
    )
  )

variable_summary

tt_save(tt(variable_summary, rownames = FALSE), 'table_sum.typ')




library(dplyr)
library(purrr)
library(tibble)
library(typsttable)

# Variables to summarize
vars <- c(
  "n_publication",
  "gdp_cap",
  "gdp_growth",
  "internet",
  "electricity",
  "tertiary_enrol",
  "education_exp",
  "rd_exp",
  "researchers_per_million",
  "n_citations",
  "founded_date",
  "n_international_any",
  "international_share_any",
  "n_inter_african",
  "inter_african_share",
  "n_extra_african",
  "extra_african_share"
)

summary_table <- map_dfr(vars, function(v) {

  x <- df_africa[[v]]

  tibble(
    Variable = v,
    Obs = sum(!is.na(x)),
    Mean = round(mean(x, na.rm = TRUE), 3),
    SD = round(sd(x, na.rm = TRUE), 3),
    Min = round(min(x, na.rm = TRUE), 3),
    Median = round(median(x, na.rm = TRUE), 3),
    Max = round(max(x, na.rm = TRUE), 3)
  )
})

summary_table

# Export to Typst

tt_save(tt(summary_table, rownames = FALSE), 'sum_table.typ')
write_csv(df_africa, 'df_africa.csv')
