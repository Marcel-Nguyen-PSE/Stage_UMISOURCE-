library(httr)
library(jsonlite)
library(tidyverse)
library(furrr)
library(progressr)

# ── Settings ──────────────────────────────────────────────────────────────────
OPENALEX_API_KEY <- "RNnWtjEHVZgXnaQzeN1KuT"
plan(multisession, workers = 6)

# ── Get total works count ─────────────────────────────────────────────────────
get_works_count <- function(inst_id, year, retries = 5) {
  url <- paste0(
    "https://api.openalex.org/works",
    "?filter=institutions.id:", inst_id,
    ",publication_year:", year,
    "&per_page=1",
    "&api_key=", OPENALEX_API_KEY
  )
  
  for (i in seq_len(retries)) {
    res <- tryCatch(
      GET(url, timeout(120)),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("Timeout|timed out", msg, ignore.case = TRUE)) {
          message(sprintf("  [TIMEOUT] get_works_count retry %d/%d | %s %d", i, retries, inst_id, year))
        } else {
          message(sprintf("  [ERROR] get_works_count: %s | %s %d", msg, inst_id, year))
        }
        Sys.sleep(2^i)
        NULL
      }
    )
    if (!is.null(res) && status_code(res) == 200) {
      parsed <- fromJSON(rawToChar(res$content))
      return(as.integer(parsed$meta$count))
    }
    Sys.sleep(2^i)
  }
  NA_integer_
}

# ── Direct fetch (under 10k works) ───────────────────────────────────────────
fetch_citations_direct <- function(inst_id, year, retries = 5) {
  cursor <- "*"
  total  <- 0L
  
  repeat {
    url <- paste0(
      "https://api.openalex.org/works",
      "?filter=institutions.id:", inst_id,
      ",publication_year:", year,
      "&select=cited_by_count",
      "&per_page=200",
      "&cursor=", cursor,
      "&api_key=", OPENALEX_API_KEY
    )
    
    res <- NULL
    for (i in seq_len(retries)) {
      response <- tryCatch(
        GET(url, timeout(120)),
        error = function(e) {
          msg <- conditionMessage(e)
          if (grepl("Timeout|timed out", msg, ignore.case = TRUE)) {
            wait <- 2^i + runif(1, 0, 2)
            message(sprintf("  [TIMEOUT] waiting %.1fs before retry %d/%d | %s %d",
                            wait, i, retries, inst_id, year))
            Sys.sleep(wait)
          } else {
            message(sprintf("  [ERROR] %s | %s %d", msg, inst_id, year))
            Sys.sleep(5)
          }
          NULL
        }
      )
      
      if (!is.null(response) && status_code(response) == 429) {
        wait <- 2^i + runif(1, 0, 2)
        message(sprintf("  [429] waiting %.1fs", wait))
        Sys.sleep(wait)
        next
      }
      if (!is.null(response) && status_code(response) == 200) {
        res <- response
        break
      }
      Sys.sleep(5)
    }
    
    if (is.null(res)) {
      return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = NA_integer_))
    }
    
    parsed  <- fromJSON(rawToChar(res$content), flatten = TRUE)
    results <- parsed$results
    
    if (is.null(results) || length(results) == 0) break
    
    total  <- total + as.integer(sum(results$cited_by_count, na.rm = TRUE))
    cursor <- parsed$meta$next_cursor
    
    if (is.null(cursor) || is.na(cursor)) break
    Sys.sleep(runif(1, 0.2, 0.5))
  }
  
  tibble(inst_id = inst_id, year = as.integer(year), n_citations = total)
}

# ── Fetch citations within a cited_by_count range ────────────────────────────
fetch_range <- function(inst_id, year, min_cit, max_cit, retries = 5) {
  cursor <- "*"
  total  <- 0L
  
  repeat {
    url <- paste0(
      "https://api.openalex.org/works",
      "?filter=institutions.id:", inst_id,
      ",publication_year:", year,
      ",cited_by_count:", min_cit, "-", max_cit,
      "&select=cited_by_count",
      "&per_page=200",
      "&cursor=", cursor,
      "&api_key=", OPENALEX_API_KEY
    )
    
    res <- NULL
    for (i in seq_len(retries)) {
      response <- tryCatch(
        GET(url, timeout(120)),
        error = function(e) {
          msg <- conditionMessage(e)
          if (grepl("Timeout|timed out", msg, ignore.case = TRUE)) {
            wait <- 2^i + runif(1, 0, 2)
            message(sprintf("  [TIMEOUT] waiting %.1fs before retry %d/%d", wait, i, retries))
            Sys.sleep(wait)
          } else {
            message(sprintf("  [ERROR] %s", msg))
            Sys.sleep(5)
          }
          NULL
        }
      )
      
      if (!is.null(response) && status_code(response) == 429) {
        wait <- 2^i + runif(1, 0, 2)
        message(sprintf("  [429] waiting %.1fs", wait))
        Sys.sleep(wait)
        next
      }
      if (!is.null(response) && status_code(response) == 200) {
        res <- response
        break
      }
      Sys.sleep(5)
    }
    
    if (is.null(res)) return(NA_integer_)
    
    parsed  <- fromJSON(rawToChar(res$content), flatten = TRUE)
    results <- parsed$results
    
    if (is.null(results) || length(results) == 0) break
    
    total  <- total + as.integer(sum(results$cited_by_count, na.rm = TRUE))
    cursor <- parsed$meta$next_cursor
    
    if (is.null(cursor) || is.na(cursor)) break
    Sys.sleep(runif(1, 0.2, 0.5))
  }
  
  total
}

# ── Main fetch function ───────────────────────────────────────────────────────
fetch_citations_chunked <- function(inst_id, year, retries = 5) {
  Sys.sleep(runif(1, 0.5, 1.0))
  
  total_works <- get_works_count(inst_id, year)
  
  if (is.na(total_works)) {
    return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = NA_integer_))
  }
  
  if (total_works == 0) {
    return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = 0L))
  }
  
  if (total_works <= 10000) {
    return(fetch_citations_direct(inst_id, year, retries))
  }
  
  # Over 10k: split by cited_by_count ranges
  message(sprintf("  [CHUNKING] %s %d has %d works — splitting by cited_by_count",
                  inst_id, year, total_works))
  
  ranges <- list(
    c(0,      0),
    c(1,     10),
    c(11,   100),
    c(101,  1000),
    c(1001, 99999)
  )
  
  total_citations <- 0L
  for (r in ranges) {
    citations_in_range <- fetch_range(inst_id, year, r[1], r[2], retries)
    if (is.na(citations_in_range)) {
      return(tibble(inst_id = inst_id, year = as.integer(year), n_citations = NA_integer_))
    }
    total_citations <- total_citations + citations_in_range
  }
  
  tibble(inst_id = inst_id, year = as.integer(year), n_citations = total_citations)
}

# ── Resume from existing data ─────────────────────────────────────────────────

citations_af <- read_csv('citations_final.csv')

citations <- citations_progress

message(sprintf("Loaded %d existing observations", nrow(citations)))

# ── Build fetch queue ─────────────────────────────────────────────────────────
df_africa <- read_csv('df_africa.csv') %>% rename(inst_id = 'openalex_id')

to_fetch <- df_africa %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(citations, by = c("inst_id", "year"))

message(sprintf("%d requests remaining", nrow(to_fetch)))

# ── Chunked parallel loop ─────────────────────────────────────────────────────
chunk_size <- 100
chunks     <- split(seq_len(nrow(to_fetch)), ceiling(seq_len(nrow(to_fetch)) / chunk_size))

message(sprintf("Running %d chunks of ~%d requests each", length(chunks), chunk_size))

with_progress({
  p <- progressor(nrow(to_fetch))
  
  for (chunk_idx in seq_along(chunks)) {
    idx   <- chunks[[chunk_idx]]
    batch <- to_fetch[idx, ]
    
    new_citations <- future_map2_dfr(
      batch$inst_id,
      batch$year,
      function(id, yr) {
        p(sprintf("%s | %d", id, yr))
        fetch_citations_chunked(id, yr)
      },
      .options = furrr_options(seed = TRUE)
    )
    
    citations <- bind_rows(citations, new_citations)
    saveRDS(citations, "citations_progress.rds")
    message(sprintf("Chunk %d/%d done | total fetched: %d", chunk_idx, length(chunks), nrow(citations)))
  }
})

# ── Done ──────────────────────────────────────────────────────────────────────

message("Done! ", nrow(citations), " rows fetched")
message("  Successful : ", sum(!is.na(citations$n_citations)))
message("  Failed     : ", sum(is.na(citations$n_citations)))

saveRDS(citations, "citations_final.rds")
write_csv(citations_progress, "citations_final.csv")
write_csv(citations, 'citations_af_v2.csv')

df_africa <- read_csv('df_africa.csv')


df_africa <- df_africa %>% 
  left_join(citations, by = c('inst_id', 'year'))

write_csv(df_africa, 'df_africa.csv')

nrow(df_africa %>% filter(!is.na(n_citations)))

df_africa <- df_africa %>%
  select(-cum_publications)
names(df_africa)

variable_dictionary <- tribble(

  ~Variable, ~Definition,

  "name", "Name of the institution",

  "year", "Observation year",

  "inst_id", "Unique institution identifier",

  "country_code", "Country code of the institution",

  "type", "Type or category of institution",

  "n_publication", "Number of publications",

  "ace_1", "Academic excellence/collaboration indicator 1",

  "ace_2", "Academic excellence/collaboration indicator 2",

  "deltas_1", "Change (delta) indicator 1",

  "gdp_cap", "GDP per capita",

  "gdp_growth", "Annual GDP growth rate",

  "internet", "Internet penetration rate",

  "electricity", "Access to electricity rate",

  "tertiary_enrol", "Tertiary education enrollment rate",

  "education_exp", "Education expenditure (% of GDP)",

  "rd_exp", "Research and development expenditure (% of GDP)",

  "researchers", "Number of researchers",

  "n_citations", "Number of citations received"

)
library(typstable)
tt_save(tt(variable_dictionary, rownames = FALSE), 'dict.typ')


tt_save(tt(data.frame(

  Variable = names(df_africa %>% select(-ace_1, -ace_2, deltas_1, -name, -year, -type)),

  Type = sapply(df_africa %>% select(-ace_1, -ace_2, deltas_1, -name, -year, -type), class),

  Missing = sapply(df_africa %>% select(-ace_1, -ace_2, deltas_1, -name, -year, -type), function(x) sum(is.na(x))),

  Unique = sapply(df_africa %>% select(-ace_1, -ace_2, deltas_1, -name, -year, -type), n_distinct),
  
  Part_missing =  sapply(df_africa %>% select(-ace_1, -ace_2, deltas_1, -name, -year, -type), function(x) sum(is.na(x))) / nrow(df_africa %>% select(-ace_1, -ace_2, deltas_1, -name, -year, -type))

), rownames = FALSE) %>% mutate(Part_missing = round(Part_missing, 3)), 'desc1.typ')


library(dplyr)
library(tinytable)

vars <- df_africa %>%
  select(-ace_1, -ace_2, -deltas_1, -name, -year, -type)

desc1 <- data.frame(
  Variable = names(vars),
  Type = sapply(vars, function(x) paste(class(x), collapse = ", ")),
  Missing = sapply(vars, function(x) sum(is.na(x))),
  Unique = sapply(vars, n_distinct),
  Part_missing = sapply(vars, function(x) sum(is.na(x))) / nrow(vars),
  row.names = NULL
) %>%
  mutate(Part_missing = round(Part_missing, 3))

tt_save(
  tt(desc1, rownames = FALSE),
  "desc1.typ"
)


library(FactoMineR)
library(factoextra)
library(dplyr)

mca_data <- df_africa %>%
  select(ace_1, ace_2, deltas_1) %>%
  mutate(across(everything(), as.factor))

res.mca <- MCA(mca_data, graph = FALSE)
res.mca

fviz_mca_var(
  res.mca,
  repel = TRUE
)
