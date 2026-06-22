library(dplyr)
library(tidyr)
library(purrr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------
# 1. BUILD CATEGORICAL "POSITION" VARIABLES (per university-year)
# ---------------------------------------------------------------
df_mca <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    cites_per_pub = n_citations / n_publication,
    pub_growth    = n_publication / lag(n_publication) - 1
  ) %>%
  ungroup() %>%
  mutate(
    pub_tier      = factor(ntile(n_publication, 3),
                            labels = c("low_pub","mid_pub","high_pub")),
    cite_tier     = factor(ntile(cites_per_pub, 3),
                            labels = c("low_impact","mid_impact","high_impact")),
    research_tier = factor(ntile(researchers, 3),
                            labels = c("low_res","mid_res","high_res")),
    rd_tier       = factor(ntile(rd_exp, 3),
                            labels = c("low_rd","mid_rd","high_rd")),
    treated       = factor(if_else(ace_1 == 1 | ace_2 == 1 | deltas_1 == 1,
                                    "treated", "untreated"))
  ) %>%
  filter(!is.na(pub_tier), !is.na(cite_tier), !is.na(research_tier), !is.na(rd_tier))
# ---------------------------------------------------------------
# 2. STATIC MCA (full sample, all years pooled) — modality cloud + individuals
# ---------------------------------------------------------------
mca_vars <- df_mca %>% select(pub_tier, cite_tier, research_tier, rd_tier, treated)

res.mca <- MCA(mca_vars, graph = FALSE)

p_var <- fviz_mca_var(res.mca, repel = TRUE, title = "Nuage des modalités")
p_ind <- fviz_mca_ind(res.mca, habillage = mca_vars$treated, addEllipses = TRUE,
                       geom = "point", alpha.ind = 0.4,
                       title = "Universités : traitées vs non traitées")

p_var | p_ind

# ---------------------------------------------------------------
# 3. DYNAMIC MCA — re-run per year, extract Dim1 coordinates
#    (this gives you the "trajectory" / evolving position over time)
# ---------------------------------------------------------------
mca_by_year <- function(yr) {
  d <- df_mca %>% filter(year == yr) %>%
    select(inst_id, pub_tier, cite_tier, research_tier, rd_tier, treated)
  if (nrow(d) < 30) return(NULL)  # skip years with too few obs

  vars <- d %>% select(-inst_id)
  m <- MCA(vars, graph = FALSE)

  tibble(
    inst_id = d$inst_id,
    year    = yr,
    dim1    = m$ind$coord[, 1],
    dim2    = m$ind$coord[, 2],
    treated = d$treated
  )
}

years <- sort(unique(df_mca$year))
traj <- map_dfr(years, mca_by_year)

# ---------------------------------------------------------------
# 4. POSITION SCORE OVER TIME — group trajectories (core vs periphery)
# ---------------------------------------------------------------
traj_summary <- traj %>%
  group_by(year, treated) %>%
  summarise(mean_dim1 = mean(dim1, na.rm = TRUE),
            se_dim1   = sd(dim1, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

ggplot(traj_summary, aes(year, mean_dim1, color = treated)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_dim1 - 1.96*se_dim1, ymax = mean_dim1 + 1.96*se_dim1,
                  fill = treated), alpha = 0.15, color = NA) +
  labs(title = "Trajectoire d'insertion (Dim 1 de l'AFCM) — traitées vs non traitées",
       y = "Position moyenne (Dim 1)", x = "Année") +
  theme_minimal()

# ---------------------------------------------------------------
# 5. GROUP COMPARISON STATS (Levene + Welch t-test + Cohen's D), like PEI paper
# ---------------------------------------------------------------
library(car)
library(effsize)

for (yr in years) {
  sub <- traj %>% filter(year == yr)
  if (length(unique(sub$treated)) < 2 || nrow(sub) < 10) next

  lev <- leveneTest(dim1 ~ treated, data = sub)
  tt  <- t.test(dim1 ~ treated, data = sub, var.equal = (lev$`Pr(>F)`[1] > 0.05))
  d   <- cohen.d(sub$dim1, sub$treated)

  cat(sprintf("Year %d | Levene p=%.3f | t=%.2f p=%.3f | Cohen's D=%.2f\n",
              yr, lev$`Pr(>F)`[1], tt$statistic, tt$p.value, d$estimate))
}

uni <- df_africa %>%
 count(name, sort = TRUE) %>%
  select(-n)

write_csv(uni, 'uni.csv')
