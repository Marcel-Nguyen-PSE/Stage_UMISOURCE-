write_csv(df_africa, 'df_africa.csv')

df_africa <- read_csv('df_africa.csv')

df <- read_csv('df_africa.csv') %>%
  filter(year <= 2025)

library(fixest)
library(MatchIt)

# ── Spec 1 : ACE 1, control = ACE 2 universities ─────────────────────────────
df_ace1 <- df |> filter(any_ace == 1)

did_ace1 <- feols(
  n_publication ~ i(year, ace_1, ref = 2013) | openalex_id + year,
  data    = df_ace1,
  cluster = ~openalex_id
)




# ── Spec 1 : ACE 2, control = ACE 1 universities ─────────────────────────────
df_ace2 <- df |> filter(any_ace == 1)

did_ace2 <- feols(
  n_publication ~ i(year, ace_2, ref = 2015) | openalex_id + year,
  data    = df_ace2,
  cluster = ~openalex_id
)

# ── Spec 2 : matched controls ─────────────────────────────────────────────────
df_match_base <- df |>
  filter(year == 2014) |>
  mutate(treated = as.integer(ace_1 == 1 & !is.na(ace_1))) |>
  filter(!is.na(gdp_cap), !is.na(internet), !is.na(electricity),
         !is.na(tertiary_enrol)) |>
  select(openalex_id, treated, gdp_cap, internet,
         electricity, tertiary_enrol)

df_match_base2 <- df |>
  filter(year == 2016) |>
  mutate(treated = as.integer(ace_2 == 1 & !is.na(ace_2))) |>
  filter(!is.na(gdp_cap), !is.na(internet), !is.na(electricity),
         !is.na(tertiary_enrol)) |>
  select(openalex_id, treated, gdp_cap, internet,
         electricity, tertiary_enrol)

table(df_match_base$treated)  # should now have both 0s and 1s

m_out <- matchit(
  treated ~ gdp_cap + internet + electricity + tertiary_enrol,
  data   = df_match_base,
  method = "nearest",
  ratio  = 2
)

m_out2 <- matchit(
  treated ~ gdp_cap + internet + electricity + tertiary_enrol,
  data   = df_match_base2,
  method = "nearest",
  ratio  = 2
)



matched_ids <- match.data(m_out)$openalex_id
df_matched  <- df |> filter(openalex_id %in% matched_ids)

matched_ids2 <- match.data(m_out2)$openalex_id
df_matched2  <- df |> filter(openalex_id %in% matched_ids2)

table(df_matched$ace_1)  # verify: should have both 0s and 1s

did_matched <- feols(
  n_publication ~ i(year, ace_1, ref = 2013) +
    gdp_cap + internet + electricity + tertiary_enrol |
    openalex_id + year,
  data    = df_matched,
  cluster = ~openalex_id
)

did_matched2 <- feols(
  n_publication ~ i(year, ace_2, ref = 2015) +
    gdp_cap + internet + electricity + tertiary_enrol |
    openalex_id + year,
  data    = df_matched2,
  cluster = ~openalex_id
)

# ── Results ───────────────────────────────────────────────────────────────────
iplot(did_ace1,    main = "ACE 1 – control: ACE 2")
iplot(did_ace2,    main = "ACE 2 – control: ACE 1")

did_ace1_match <- ggiplot(did_matched, main = "ACE 1 – matched controls")
did_ace2_match <- ggiplot(did_matched2, main = 'ACE 2 : matched controls')

did_comb <- (did_ace1_match + did_ace2_match) + plot_annotation(title = 'DID Estimates for ACE 1 and ACE 2 using matched controls')
ggsave(
  filename = "parallel_trends_didcomb.jpeg",
  plot     = did_comb,
  width    = 13.33,   # 16:9 slide dimensions in inches
  height   = 7.5,
  dpi      = 300,
  bg       = "white"
)
etable(did_ace1, did_ace2, did_matched)



# ── Spec 2 : matched controls for ACE 2 ──────────────────────────────────────
df_match_base_ace2 <- df |>
  filter(year == 2016) |>
  mutate(treated = as.integer(ace_2 == 1 & !is.na(ace_2))) |>
  filter(!is.na(gdp_cap), !is.na(internet), !is.na(electricity),
         !is.na(tertiary_enrol)) |>
  select(openalex_id, treated, gdp_cap, internet,
         electricity, tertiary_enrol)

table(df_match_base_ace2$treated)  # verify

m_out_ace2 <- matchit(
  treated ~ gdp_cap + internet + electricity + tertiary_enrol,
  data   = df_match_base_ace2,
  method = "nearest",
  ratio  = 2
)

matched_ids_ace2 <- match.data(m_out_ace2)$openalex_id
df_matched_ace2  <- df |> filter(openalex_id %in% matched_ids_ace2)

table(df_matched_ace2$ace_2)  # verify: should have both 0s and 1s

did_matched_ace2 <- feols(
  n_publication ~ i(year, ace_2, ref = 2015) +
    gdp_cap + internet + electricity + tertiary_enrol |
    openalex_id + year,
  data    = df_matched_ace2,
  cluster = ~openalex_id
)

iplot(did_matched_ace2, main = "ACE 2 – matched controls")

library(dplyr)

library(ggplot2)


library(ggplot2)
library(dplyr)

plot_parallel <- function(data, treat_var, treat_year, ref_year, title) {
  data |>
    mutate(group = ifelse({{ treat_var }} == 1, "Treated", "Control")) |>
    filter(!is.na(group)) |>
    group_by(year, group) |>
    summarise(mean_pub = mean(n_publication, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = year, y = mean_pub, color = group, linetype = group)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = treat_year - 0.5, linetype = "dashed", color = "grey40") +
    annotate("text", x = treat_year - 0.6, y = Inf,
             label = "Treatment", hjust = 1, vjust = 1.5,
             size = 3, color = "grey40") +
    scale_color_manual(values = c("Treated" = "#E63946", "Control" = "#457B9D")) +
    labs(title = title, x = NULL, y = "Mean publications",
         color = NULL, linetype = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
}

# ── ACE 1, control = ACE 2 ────────────────────────────────────────────────────
p1 <- plot_parallel(df_ace1, ace_1, 2014, 2013, "ACE 1 – control: ACE 2")

# ── ACE 2, control = ACE 1 ────────────────────────────────────────────────────
p2 <- plot_parallel(df_ace2, ace_2, 2016, 2015, "ACE 2 – control: ACE 1")

# ── ACE 1, matched controls ───────────────────────────────────────────────────
p3 <- plot_parallel(df_matched, ace_1, 2014, 2013, "ACE 1 – matched controls")

# ── ACE 2, matched controls ───────────────────────────────────────────────────
p4 <- plot_parallel(df_matched_ace2, ace_2, 2016, 2015, "ACE 2 – matched controls")

# ── Combined plot ─────────────────────────────────────────────────────────────
library(patchwork)
(p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "Parallel trends – pre-treatment periods",
    theme    = theme(plot.title = element_text(face = "bold"))
  )

library(patchwork)

combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "Parallel trends – pre-treatment periods",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "grey40")
    )
  )

ggsave(
  filename = "parallel_trends.jpeg",
  plot     = combined_plot,
  width    = 13.33,   # 16:9 slide dimensions in inches
  height   = 7.5,
  dpi      = 300,
  bg       = "white"
)

library(patchwork)

# Capture iplot outputs as ggplot objects
g1 <- ggiplot(did_ace1, main = "ACE 1 – control: ACE 2")
g2 <- ggiplot(did_ace2, main = "ACE 2 – control: ACE 1")

combined_iplot <- g1 + g2 +
  plot_annotation(
    title = "DID estimates – ACE 1 and ACE 2",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16)
    )
  )

ggsave(
  filename = "did_estimates.jpeg",
  plot     = combined_iplot,
  width    = 13.33,
  height   = 7.5,
  dpi      = 300,
  bg       = "white"
)
