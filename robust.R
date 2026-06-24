library(dplyr)
library(segmented)
library(lmtest)
library(sandwich)
library(broom)
library(ggplot2)
library(purrr)

# ==================================================
# 0. Clean sample
# ==================================================

seg_data <- df_trans %>%
  filter(
    !is.na(rel_log_cpp_t),
    !is.na(rel_log_cpp_t5),
    is.finite(rel_log_cpp_t),
    is.finite(rel_log_cpp_t5)
  )

# ==================================================
# 1. Baseline segmented regression
# ==================================================

lm1 <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t,
  data = seg_data
)

seg <- segmented(
  lm1,
  seg.Z = ~ rel_log_cpp_t,
  psi = 0.5
)

summary(seg)
confint(seg)
seg$psi

# Slopes before and after threshold
slope(seg)

# ==================================================
# 2. Heteroskedasticity checks
# ==================================================

bptest(lm1)

plot(
  fitted(lm1),
  residuals(lm1),
  pch = 20,
  col = "grey50",
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs fitted values"
)
abline(h = 0, col = "red", lwd = 2)

# Robust SE for the underlying segmented linear terms
coeftest(
  seg,
  vcov = vcovHC(seg, type = "HC3")
)

# ==================================================
# 3. Sensitivity to starting value psi
# ==================================================

psi_grid <- seq(
  quantile(seg_data$rel_log_cpp_t, 0.10, na.rm = TRUE),
  quantile(seg_data$rel_log_cpp_t, 0.90, na.rm = TRUE),
  length.out = 15
)

psi_sensitivity <- map_dfr(psi_grid, function(p0) {
  
  fit <- tryCatch(
    segmented(
      lm1,
      seg.Z = ~ rel_log_cpp_t,
      psi = p0
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(
      tibble(
        start_psi = p0,
        estimated_psi = NA_real_,
        slope_before = NA_real_,
        slope_after = NA_real_,
        converged = FALSE
      )
    )
  }
  
  sl <- slope(fit)$rel_log_cpp_t[, "Est."]
  
  tibble(
    start_psi = p0,
    estimated_psi = fit$psi[1, "Est."],
    slope_before = sl[1],
    slope_after = sl[2],
    converged = TRUE
  )
})

psi_sensitivity

ggplot(
  psi_sensitivity,
  aes(x = start_psi, y = estimated_psi)
) +
  geom_point() +
  geom_hline(
    yintercept = seg$psi[1, "Est."],
    color = "red"
  ) +
  labs(
    title = "Sensitivity of estimated breakpoint to starting value",
    x = "Initial psi",
    y = "Estimated breakpoint"
  ) +
  theme_minimal()

# ==================================================
# 4. Cluster bootstrap by university
# ==================================================

set.seed(123)

B <- 300
ids <- unique(seg_data$name)

boot_seg <- replicate(B, {
  
  boot_ids <- sample(ids, length(ids), replace = TRUE)
  
  boot_df <- bind_rows(
    lapply(
      boot_ids,
      function(id) seg_data[seg_data$name == id, ]
    )
  )
  
  lm_b <- lm(
    rel_log_cpp_t5 ~ rel_log_cpp_t,
    data = boot_df
  )
  
  fit_b <- tryCatch(
    segmented(
      lm_b,
      seg.Z = ~ rel_log_cpp_t,
      psi = seg$psi[1, "Est."]
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit_b)) {
    return(c(psi = NA_real_, slope_before = NA_real_, slope_after = NA_real_))
  }
  
  sl <- tryCatch(
    slope(fit_b)$rel_log_cpp_t[, "Est."],
    error = function(e) c(NA_real_, NA_real_)
  )
  
  c(
    psi = fit_b$psi[1, "Est."],
    slope_before = sl[1],
    slope_after = sl[2]
  )
})

boot_seg <- as.data.frame(t(boot_seg))

boot_summary <- boot_seg %>%
  summarise(
    psi_mean = mean(psi, na.rm = TRUE),
    psi_p025 = quantile(psi, 0.025, na.rm = TRUE),
    psi_p975 = quantile(psi, 0.975, na.rm = TRUE),
    slope_before_mean = mean(slope_before, na.rm = TRUE),
    slope_before_p025 = quantile(slope_before, 0.025, na.rm = TRUE),
    slope_before_p975 = quantile(slope_before, 0.975, na.rm = TRUE),
    slope_after_mean = mean(slope_after, na.rm = TRUE),
    slope_after_p025 = quantile(slope_after, 0.025, na.rm = TRUE),
    slope_after_p975 = quantile(slope_after, 0.975, na.rm = TRUE)
  )

boot_summary

ggplot(boot_seg, aes(x = psi)) +
  geom_histogram(bins = 40) +
  geom_vline(
    xintercept = seg$psi[1, "Est."],
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Cluster-bootstrap distribution of breakpoint",
    x = "Estimated breakpoint",
    y = "Frequency"
  ) +
  theme_minimal()

# ==================================================
# 5. Trim outliers robustness
# ==================================================

trimmed_data <- seg_data %>%
  filter(
    rel_log_cpp_t >= quantile(rel_log_cpp_t, 0.01, na.rm = TRUE),
    rel_log_cpp_t <= quantile(rel_log_cpp_t, 0.99, na.rm = TRUE)
  )

lm_trim <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t,
  data = trimmed_data
)

seg_trim <- segmented(
  lm_trim,
  seg.Z = ~ rel_log_cpp_t,
  psi = seg$psi[1, "Est."]
)

summary(seg_trim)
seg_trim$psi
slope(seg_trim)

# ==================================================
# 6. Alternative thresholds: 3-year and 7-year horizon
# ==================================================
# Only works if df_trans has corresponding variables.
# If not, skip this section.

# ==================================================
# 7. Piecewise model with explicit threshold variable
# ==================================================

tau <- seg$psi[1, "Est."]

piecewise_data <- seg_data %>%
  mutate(
    above_tau = as.integer(rel_log_cpp_t > tau),
    hinge = pmax(0, rel_log_cpp_t - tau)
  )

piecewise_lm <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t + hinge,
  data = piecewise_data
)

summary(piecewise_lm)

coeftest(
  piecewise_lm,
  vcov = vcovHC(piecewise_lm, type = "HC3")
)

# Cluster-robust SE by university
coeftest(
  piecewise_lm,
  vcov = vcovCL(piecewise_lm, cluster = ~ name, type = "HC1")
)

# ==================================================
# 8. Fixed effects robustness
# ==================================================
# This tests whether the segmented shape survives country and year controls.

piecewise_fe <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t + hinge + factor(country_code) + factor(year),
  data = piecewise_data
)

summary(piecewise_fe)

coeftest(
  piecewise_fe,
  vcov = vcovCL(piecewise_fe, cluster = ~ name, type = "HC1")
)

# ==================================================
# 9. Compare linear, quadratic, cubic, segmented
# ==================================================

linear_fit <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t,
  data = seg_data
)

quadratic_fit <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t + I(rel_log_cpp_t^2),
  data = seg_data
)

cubic_fit <- lm(
  rel_log_cpp_t5 ~ rel_log_cpp_t + I(rel_log_cpp_t^2) + I(rel_log_cpp_t^3),
  data = seg_data
)

AIC(linear_fit, quadratic_fit, cubic_fit, seg)
BIC(linear_fit, quadratic_fit, cubic_fit, seg)

# ==================================================
# 10. Plot fitted segmented model
# ==================================================

plot_data <- data.frame(
  rel_log_cpp_t = seq(
    quantile(seg_data$rel_log_cpp_t, 0.01, na.rm = TRUE),
    quantile(seg_data$rel_log_cpp_t, 0.99, na.rm = TRUE),
    length.out = 300
  )
)

plot_data$fit <- predict(
  seg,
  newdata = plot_data
)

ggplot(seg_data, aes(rel_log_cpp_t, rel_log_cpp_t5)) +
  geom_point(alpha = 0.08, size = 0.6) +
  geom_line(
    data = plot_data,
    aes(rel_log_cpp_t, fit),
    linewidth = 1.2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = tau,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Segmented transition function",
    x = "Citation-intensity position at t",
    y = "Citation-intensity position at t + 5"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "segmented_transition_robustness.jpeg",
  width = 8,
  height = 6,
  dpi = 300
)

# ==================================================
# 11. Final robustness table
# ==================================================

robustness_table <- tibble(
  specification = c(
    "Baseline segmented",
    "Trimmed 1-99%",
    "Piecewise fixed threshold",
    "Piecewise + country/year FE"
  ),
  threshold = c(
    seg$psi[1, "Est."],
    seg_trim$psi[1, "Est."],
    tau,
    tau
  ),
  slope_before = c(
    slope(seg)$rel_log_cpp_t[1, "Est."],
    slope(seg_trim)$rel_log_cpp_t[1, "Est."],
    coef(piecewise_lm)["rel_log_cpp_t"],
    coef(piecewise_fe)["rel_log_cpp_t"]
  ),
  slope_after = c(
    slope(seg)$rel_log_cpp_t[2, "Est."],
    slope(seg_trim)$rel_log_cpp_t[2, "Est."],
    coef(piecewise_lm)["rel_log_cpp_t"] + coef(piecewise_lm)["hinge"],
    coef(piecewise_fe)["rel_log_cpp_t"] + coef(piecewise_fe)["hinge"]
  )
)

robustness_table
