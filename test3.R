library(dplyr)
library(purrr)
library(furrr)
library(future)
library(openalexR)
library(readr)

# ── Settings ──────────────────────────────────────────────────────────────────
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY")
options(openalexR.mailto = "Marcel.Nguyen@ens.psl.eu")
plan(multisession, workers = 2)

# ── ISO 3166-1 alpha-2 codes for African countries ───────────────────────────
african_codes <- c(
  "DZ","AO","BJ","BW","BF","BI","CM","CV","CF","TD","KM","CG","CD","CI",
  "DJ","EG","GQ","ER","SZ","ET","GA","GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","YT","MA","MZ","NA","NE","NG","RE","RW",
  "SH","ST","SN","SC","SL","SO","ZA","SS","SD","TZ","TG","TN","UG","EH",
  "ZM","ZW"
)

# ── Helper: get distinct country codes for one work's authorships ────────────
get_country_codes <- function(authorship_df) {
  tryCatch({
    a <- authorship_df
    if (is.null(a) || nrow(a) == 0) return(character(0))

    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        purrr::map(function(x) if (is.null(x) || length(x) == 0) NA_character_ else x) |>
        unlist()
      out <- unique(na.omit(countries))
      if (length(out) > 0) return(out)
    }

    if ("institutions" %in% names(a)) {
      countries <- a$institutions |>
        purrr::map(function(x) {
          if (is.null(x) || nrow(x) == 0) return(NA_character_)
          x$country_code
        }) |>
        unlist()
      return(unique(na.omit(countries)))
    }

    character(0)
  }, error = function(e) character(0))
}

# ── Helper: classify a work's collaboration type from its country codes ─────
classify_collab <- function(codes) {
  n_countries <- length(codes)

  if (n_countries == 0) {
    return(list(
      international_any = NA, inter_african = NA,
      extra_african = NA, inter_african_only = NA
    ))
  }
  if (n_countries < 2) {
    return(list(
      international_any = FALSE, inter_african = FALSE,
      extra_african = FALSE, inter_african_only = FALSE
    ))
  }

  has_non_african  <- any(!(codes %in% african_codes))
  has_african_pair <- sum(codes %in% african_codes) >= 2

  list(
    international_any  = TRUE,
    inter_african       = has_african_pair,
    extra_african        = has_non_african,
    inter_african_only   = has_african_pair && !has_non_african
  )
}

# ── Fetch function ─────────────────────────────────────────────────────────────
fetch_international_share_year <- function(inst_id, year) {
  Sys.sleep(runif(1, 1, 2))

  empty_row <- function(n_works = NA_integer_) {
    dplyr::tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = n_works,
      n_international_any = NA_integer_,    international_share_any    = NA_real_,
      n_inter_african      = NA_integer_,    inter_african_share         = NA_real_,
      n_extra_african       = NA_integer_,    extra_african_share          = NA_real_,
      n_inter_african_only   = NA_integer_,    inter_african_only_share      = NA_real_
    )
  }

  works <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 200,
      pages            = NULL,
      options          = list(api_key = OPENALEX_API_KEY),
      verbose          = FALSE
    ),
    error = function(e) {
      message(sprintf("  [ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
      NULL
    }
  )

  if (is.null(works) || nrow(works) == 0) {
    zero_or_na <- if (is.null(works)) NA_integer_ else 0L
    return(dplyr::tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = zero_or_na,
      n_international_any = zero_or_na,    international_share_any    = NA_real_,
      n_inter_african      = zero_or_na,    inter_african_share         = NA_real_,
      n_extra_african       = zero_or_na,    extra_african_share          = NA_real_,
      n_inter_african_only   = zero_or_na,    inter_african_only_share      = NA_real_
    ))
  }

  if (!"authorships" %in% names(works)) {
    return(empty_row(nrow(works)))
  }

  # FIX: explicitly namespace dplyr verbs (dplyr::mutate, dplyr::summarise,
  # dplyr::select) so they can never be shadowed by another package's
  # same-named function (e.g. MASS::select) loaded later in the session.
  # This was the actual cause of "unused arguments (...)" -- select() was
  # being dispatched to the wrong function.
  result <- tryCatch({
    works |>
      dplyr::mutate(
        country_codes       = purrr::map(authorships, get_country_codes),
        classification      = purrr::map(country_codes, classify_collab),
        international_any   = purrr::map_lgl(classification, ~ isTRUE(.x$international_any)),
        inter_african         = purrr::map_lgl(classification, ~ isTRUE(.x$inter_african)),
        extra_african          = purrr::map_lgl(classification, ~ isTRUE(.x$extra_african)),
        inter_african_only      = purrr::map_lgl(classification, ~ isTRUE(.x$inter_african_only))
      ) |>
      dplyr::summarise(
        n_works                       = dplyr::n(),
        n_international_any           = sum(international_any, na.rm = TRUE),
        international_share_any       = n_international_any / n_works,
        n_inter_african                = sum(inter_african, na.rm = TRUE),
        inter_african_share            = n_inter_african / n_works,
        n_extra_african                 = sum(extra_african, na.rm = TRUE),
        extra_african_share              = n_extra_african / n_works,
        n_inter_african_only              = sum(inter_african_only, na.rm = TRUE),
        inter_african_only_share          = n_inter_african_only / n_works
      ) |>
      dplyr::mutate(inst_id = inst_id, year = as.integer(year)) |>
      dplyr::select(inst_id, year, n_works,
                    n_international_any, international_share_any,
                    n_inter_african, inter_african_share,
                    n_extra_african, extra_african_share,
                    n_inter_african_only, inter_african_only_share)
  }, error = function(e) {
    message(sprintf("  [PARSE ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
    empty_row(nrow(works))
  })

  result
}

# ── Resume from checkpoint ────────────────────────────────────────────────────
to_fetch <- df_africa |>
  dplyr::distinct(inst_id, year) |>
  dplyr::filter(!is.na(inst_id))

results <- if (file.exists("international_share_year_progress.rds")) {
  message("Resuming from checkpoint...")
  readRDS("international_share_year_progress.rds") |>
    dplyr::filter(!is.na(n_works))
} else {
  dplyr::tibble(
    inst_id = character(),
    year = integer(),
    n_works = integer(),
    n_international_any = integer(),
    international_share_any = numeric(),
    n_inter_african = integer(),
    inter_african_share = numeric(),
    n_extra_african = integer(),
    extra_african_share = numeric(),
    n_inter_african_only = integer(),
    inter_african_only_share = numeric()
  )
}

to_fetch_remaining <- to_fetch |>
  dplyr::anti_join(results, by = c("inst_id", "year"))

message(sprintf("%d requests remaining", nrow(to_fetch_remaining)))

# ── Chunked parallel loop ─────────────────────────────────────────────────────
chunk_size <- 100
chunks <- split(
  to_fetch_remaining,
  ceiling(seq_len(nrow(to_fetch_remaining)) / chunk_size)
)

for (k in seq_along(chunks)) {
  message(sprintf("Chunk %d / %d", k, length(chunks)))

  chunk_result <- future_pmap_dfr(
    chunks[[k]],
    function(inst_id, year) {
      fetch_international_share_year(inst_id, year)
    },
    .options = furrr_options(seed = TRUE)
  )

  results <- dplyr::bind_rows(results, chunk_result)
  saveRDS(results, "international_share_year_progress.rds")
  write_csv(results, "international_share_year.csv")

  message(sprintf("  Done | total fetched: %d | successful: %d | failed: %d",
                  nrow(results),
                  sum(!is.na(results$n_works)),
                  sum(is.na(results$n_works))))
}

# ── Done ──────────────────────────────────────────────────────────────────────
df_africa <- df_africa |>
  dplyr::left_join(results, by = c("inst_id", "year"))

plan(sequential)

write_csv(df_africa, 'df_africa.csv')
