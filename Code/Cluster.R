df_mca <- df_africa %>%
  filter(year == 2025) %>%
  mutate(
    pub_cat = ntile(n_publication, 3),
    intl_cat = ntile(international_share_any, 3),
    cit_cat = ntile(n_citations, 3),
    inter_af_cat = ntile(inter_african_share, 3),
    extra_af_cat = ntile(extra_african_share, 3)
  ) %>%
  mutate(
    across(
      ends_with("_cat"),
      ~ factor(
        .,
        levels = c(1, 2, 3),
        labels = c("Low", "Medium", "High")
      )
    )
  ) %>%
  dplyr::select(
    name,
    cit_cat,
    country_code,
    pub_cat,
    intl_cat,
    inter_af_cat,
    extra_af_cat
  ) %>%
  tidyr::drop_na()

mca_data <- df_mca %>%
  select(-name, -country_code)

mca_res <- MCA(
  mca_data,
  graph = FALSE
)

p_modalities <- fviz_mca_var(
  mca_res,
  repel = TRUE,
  col.var = "cos2",
  gradient.cols = c("grey70", "#2C7BB6", "#D7191C")
) +
  labs(
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "Output/mca_modalities_2025_cit.jpeg",
  p_modalities,
  width = 9,
  height = 7,
  dpi = 500
)

df_mca_nocit <- mca_data %>%
  dplyr::select(-cit_cat)

mca_res_nocit <- MCA(
  df_mca_nocit,
  graph = FALSE
)

p_modalities_nocit <- fviz_mca_var(
  mca_res_nocit,
  repel = TRUE,
  col.var = "cos2",
  gradient.cols = c("grey70", "#2C7BB6", "#D7191C")
) +
  labs(
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "Output/mca_modalities_2025.jpeg",
  p_modalities_nocit,
  width = 9,
  height = 7,
  dpi = 500
)
