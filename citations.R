df <- read_csv('df_panel.csv')

fetch_citations <- function(inst_id, year) {
  message("Fetching: ", inst_id, " | ", year)
  works <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 200,
      verbose          = FALSE
    ),
    error = function(e) NULL
  )
  n <- if (is.null(works) || nrow(works) == 0) NA_integer_ else sum(works$cited_by_count, na.rm = TRUE)
  tibble(inst_id = inst_id, year = as.integer(year), n_citations = n)
}

citations <- if (file.exists("citations_progress.rds")) {
  readRDS("citations_progress.rds")
} else {
  tibble(
    inst_id     = character(),
    year        = integer(),
    n_citations = integer()
  )
}


to_fetch <- df %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(citations, by = c("inst_id", "year"))

message(nrow(to_fetch), " requests remaining")

for (i in seq_len(nrow(to_fetch))) {
  result <- fetch_citations(
    to_fetch$inst_id[[i]],
    to_fetch$year[[i]]
  )
  citations <- bind_rows(citations, result)
  saveRDS(citations, "citations_progress.rds")
}

write_csv(citations, "citations.csv")