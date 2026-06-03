df <- read_csv("df_panel.csv")

library(dplyr)
library(tibble)
library(openalexR)
library(furrr)
library(future)
library(progressr)

plan(multisession, workers = 3)

fetch_citations <- function(inst_id, year, max_tries = 5) {
  
  message("Fetching: ", inst_id, " | ", year)
  
  for (try in seq_len(max_tries)) {
    
    Sys.sleep(runif(1, 1, 3))
    
    works <- tryCatch(
      oa_fetch(
        entity = "works",
        institutions.id = inst_id,
        publication_year = as.integer(year),
        per_page = 200,
        pages = "all",
        verbose = FALSE
      ),
      error = function(e) e
    )
    
    if (!inherits(works, "error")) {
      
      if (is.null(works)) {
        return(tibble(
          inst_id = inst_id,
          year = as.integer(year),
          n_citations = NA_real_
        ))
      }
      
      if (nrow(works) == 0) {
        return(tibble(
          inst_id = inst_id,
          year = as.integer(year),
          n_citations = 0
        ))
      }
      
      if (!"cited_by_count" %in% names(works)) {
        return(tibble(
          inst_id = inst_id,
          year = as.integer(year),
          n_citations = NA_real_
        ))
      }
      
      return(tibble(
        inst_id = inst_id,
        year = as.integer(year),
        n_citations = sum(works$cited_by_count, na.rm = TRUE)
      ))
    }
    
    if (grepl("429", conditionMessage(works))) {
      wait_time <- 30 * try
      message("429 rate limit. Waiting ", wait_time, " seconds...")
      Sys.sleep(wait_time)
    } else {
      message("ERROR: ", conditionMessage(works))
      return(tibble(
        inst_id = inst_id,
        year = as.integer(year),
        n_citations = NA_real_
      ))
    }
  }
  
  tibble(
    inst_id = inst_id,
    year = as.integer(year),
    n_citations = NA_real_
  )
}

citations <- if (file.exists("citations_progress.rds")) {
  readRDS("citations_progress.rds")
} else {
  tibble(
    inst_id = character(),
    year = integer(),
    n_citations = numeric()
  )
}

citations <- citations %>%
  filter(!is.na(n_citations))

to_fetch <- df %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id), !is.na(year)) %>%
  anti_join(citations, by = c("inst_id", "year"))

message(nrow(to_fetch), " requests remaining")

chunk_size <- 50

chunks <- split(
  seq_len(nrow(to_fetch)),
  ceiling(seq_len(nrow(to_fetch)) / chunk_size)
)

handlers(global = TRUE)

with_progress({
  
  p <- progressor(length(chunks))
  
  for (chunk_idx in seq_along(chunks)) {
    
    batch <- to_fetch[chunks[[chunk_idx]], ]
    
    new_citations <- future_map2_dfr(
      batch$inst_id,
      batch$year,
      fetch_citations,
      .options = furrr_options(seed = TRUE)
    )
    
    citations <- bind_rows(citations, new_citations) %>%
      distinct(inst_id, year, .keep_all = TRUE)
    
    saveRDS(citations, "citations_progress.rds")
    
    p(sprintf("Chunk %d/%d done", chunk_idx, length(chunks)))
    
    Sys.sleep(5)
  }
})
