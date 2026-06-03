library(httr)
library(jsonlite)

# Fetch African universities from OpenAlex
url <- "https://api.openalex.org/institutions"
all_names <- c()
page <- 1

repeat {
  res <- GET(url, query = list(
    filter = "continent:africa,type:education",
    select = "display_name",
    per_page = 200,
    page = page
  ))
  
  data <- fromJSON(rawToChar(res$content))
  results <- data$results$display_name
  
  if (length(results) == 0) break
  
  all_names <- c(all_names, results)
  page <- page + 1
}

df_africa <- tibble(
  name = all_names
) %>%
  crossing(
    year = 2006:2026
  )

########################################################

fetch_inst_id_safe <- function(u) {
  
  message("Fetching: ", u)
  
  inst <- tryCatch(
    oa_fetch(
      entity = "institutions",
      search = u,
      per_page = 1,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  Sys.sleep(0.5)
  
  tibble(
    name = u,
    inst_id = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$id[1],
    country_code = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$country_code[1],
    type = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$type[1]
  )
}

if (file.exists("inst_ids_africa_progress.rds")) {
  
  inst_ids <- readRDS("inst_ids_africa_progress.rds")
  
} else {
  
  inst_ids <- df_africa %>%
    distinct(name) %>%
    mutate(
      inst_id = NA_character_,
      country_code = NA_character_,
      type = NA_character_
    )
}

remaining_names <- inst_ids %>%
  filter(is.na(inst_id)) %>%
  pull(name)

for (u in remaining_names) {
  
  result <- fetch_inst_id_safe(u)
  
  inst_ids <- inst_ids %>%
    filter(name != u) %>%
    bind_rows(result)
  
  saveRDS(inst_ids, "inst_ids_africa_progress.rds")
}

df_africa <- df_africa %>%
  left_join(inst_ids, by = "name")

write_csv(inst_ids, 'instidafrica.csv')
write_csv(df_africa, 'df_africa.csv')
