df <- read_csv('df_panel.csv')

fetch_citations <- function(inst_id, year, retries = 5) {
  Sys.sleep(runif(1, 0.5, 1.0))  # base jitter before every request
  
  works <- NULL
  for (i in seq_len(retries)) {
    
    result <- tryCatch(
      oa_fetch(
        entity           = "works",
        institutions.id  = inst_id,
        publication_year = as.integer(year),
        per_page         = 200,
        verbose          = FALSE
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("429", msg)) {
          wait <- 2^i + runif(1, 0, 2)  # exponential backoff: 2s, 4s, 8s, 16s...
          message(sprintf("429 rate limit — waiting %.1fs before retry %d/%d", wait, i, retries))
          Sys.sleep(wait)
        } else {
          message(sprintf("ERROR (attempt %d): %s", i, msg))
          Sys.sleep(5)
        }
        return(NULL)
      }
    )
    
    if (!is.null(result)) {
      works <- result
      break
    }
  }

  if (is.null(works) || nrow(works) == 0) {
    return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = NA_integer_))
  }

  tibble(
    inst_id     = inst_id,
    year        = as.integer(year),
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

citations <- citations %>%
  filter(!is.na(n_citations))

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
