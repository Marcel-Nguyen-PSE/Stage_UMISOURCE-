fetch_citations <- function(inst_id, year) {
  message("Fetching: ", inst_id, " | ", year)

  Sys.sleep(runif(1, 1, 3))  # random delay between 1 and 3 seconds

  works <- tryCatch(
    oa_fetch(
      entity = "works",
      institutions.id = inst_id,
      publication_year = as.integer(year),
      per_page = 200,
      verbose = TRUE
    ),
    error = function(e) {
      message("ERROR: ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(works)) {
    return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = NA_integer_))
  }

  if (nrow(works) == 0) {
    message("No works returned")
    return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = 0L))
  }

  if (!"cited_by_count" %in% names(works)) {
    message("Column cited_by_count missing")
    return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = NA_integer_))
  }

  tibble(
    inst_id = inst_id,
    year = as.integer(year),
    n_citations = sum(works$cited_by_count, na.rm = TRUE)
  )
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

write_csv(df, 'df_panel.csv')

