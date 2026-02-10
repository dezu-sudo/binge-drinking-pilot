####----plots----

# --- 0) Ensure paths (reuse what you already have) -----------------------------
if (!exists("PROJ_DIR")) {
  PROJ_DIR <- tryCatch(
    normalizePath("C:/Users/Anigma PC/OneDrive - Tulane University/Desktop/Binge Drinking Systematic Review/binge-drinking-pilot",
                  winslash = "/", mustWork = TRUE),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
}
tab_dir <- file.path(PROJ_DIR, "outputs/main/tables")
fig_dir <- file.path(PROJ_DIR, "outputs/main/figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({ library(readr); library(dplyr); library(janitor); library(ggplot2); library(scales) })

# --- 1) Reload prev_all_prop_ageadj from CSV ----------------------------------
csv_prop <- file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_prop.csv") # est/lcl/ucl in proportions (0–1)
csv_pct  <- file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_pct.csv")  # est/lcl/ucl in percent (0–100)

if (file.exists(csv_prop)) {
  dat_raw <- read_csv(csv_prop, show_col_types = FALSE) %>% clean_names()
  prev_all_prop_ageadj <- dat_raw %>%
    transmute(
      year = as.integer(year),
      est  = as.numeric(est),
      lcl  = as.numeric(lcl),
      ucl  = as.numeric(ucl)
    ) %>% arrange(year)
} else if (file.exists(csv_pct)) {
  dat_raw <- read_csv(csv_pct, show_col_types = FALSE) %>% clean_names()
  prev_all_prop_ageadj <- dat_raw %>%
    transmute(
      year = as.integer(year),
      est  = as.numeric(est)/100,
      lcl  = as.numeric(lcl)/100,
      ucl  = as.numeric(ucl)/100
    ) %>% arrange(year)
} else {
  stop("Neither CSV was found in: ", tab_dir)
}

# --- 2) Prep for plotting ------------------------------------------------------
pdat <- prev_all_prop_ageadj %>%
  arrange(year) %>%
  mutate(
    est_pct = est * 100,
    lcl_pct = lcl * 100,
    ucl_pct = ucl * 100,
    roll3   = (lag(est_pct) + est_pct + lead(est_pct))/3,
    delta_pp = est_pct - lag(est_pct)
  )

yr_first <- min(pdat$year, na.rm = TRUE)
yr_last  <- max(pdat$year, na.rm = TRUE)
pp_change <- with(pdat, est_pct[year == yr_last] - est_pct[year == yr_first])

theme_klean <- function(base_size = 12){
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30"),
      plot.caption = element_text(size = rel(0.9), color = "grey40"),
      plot.margin = margin(10, 18, 10, 10),
      legend.position = "none"
    )
}
save_both <- function(p, fname, w = 8, h = 4.75, dpi = 320){
  ggsave(file.path(fig_dir, paste0(fname, ".png")), p, width = w, height = h, units = "in", dpi = dpi, bg = "white")
  ggsave(file.path(fig_dir, paste0(fname, ".svg")), p, width = w, height = h, units = "in", dpi = dpi, bg = "white")
}

# --- 3) MAIN: line + 95% CI ribbon --------------------------------------------
last_row <- pdat[pdat$year == yr_last, , drop = FALSE]
end_lab  <- sprintf("%d: %.1f%% (%.1f–%.1f)", yr_last, last_row$est_pct, last_row$lcl_pct, last_row$ucl_pct)
delta_lab <- sprintf("\u0394 since %d: %+.1f pp", yr_first, pp_change)

p_main <- ggplot(pdat, aes(x = year, y = est_pct)) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.4) +
  geom_ribbon(aes(ymin = lcl_pct, ymax = ucl_pct), alpha = 0.18) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  geom_line(aes(y = roll3), linewidth = 0.8, linetype = "dotted") +
  geom_text(data = last_row, aes(label = end_lab),
            hjust = -0.05, vjust = 0.5, size = 3.8) +
  annotate("text", x = yr_last, y = max(pdat$ucl_pct, na.rm = TRUE),
           label = delta_lab, vjust = -0.8, hjust = 1, size = 3.5, color = "grey30") +
  scale_x_continuous(breaks = pretty_breaks(), expand = expansion(mult = c(0.01, 0.12))) +
  scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Binge drinking prevalence (Any binge, All adults)",
    subtitle = "Age-adjusted to Year-2000 standard (7 bands); BRFSS, 50 states + DC",
    y = "Prevalence (%)",
    caption = "Shaded band = 95% CI. Dotted line = 3-pt rolling average.\nSource: BRFSS LLCP 2011–2024; des_all_std_y7 outputs."
  ) +
  coord_cartesian(clip = "off") +
  theme_klean()
save_both(p_main, "main_prev_any_binge_alladults_AGEADJ2000_line_ribbon")

# --- 4) ALT A: dot + CI --------------------------------------------------------
p_dotci <- ggplot(pdat, aes(x = year, y = est_pct)) +
  geom_linerange(aes(ymin = lcl_pct, ymax = ucl_pct), linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Binge drinking prevalence (Any binge, All adults)",
    subtitle = "Age-adjusted to Year-2000 standard (7 bands); dot = estimate, line = 95% CI",
    y = "Prevalence (%)",
    caption = "BRFSS LLCP 2011–2024; standardized over year×age7 using Year-2000 weights."
  ) +
  theme_klean()
save_both(p_dotci, "main_prev_any_binge_alladults_AGEADJ2000_dot_ci")

# --- 5) ALT B: year-over-year Δ (pp) ------------------------------------------
p_delta <- pdat %>%
  filter(!is.na(delta_pp)) %>%
  ggplot(aes(x = year, y = delta_pp)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey50") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = function(x) sprintf("%+.1f", x),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title = "Year-over-year change in prevalence (percentage points)",
    subtitle = sprintf("Net change %d→%d: %+.1f pp", yr_first, yr_last, pp_change),
    y = "Δ pp vs prior year",
    caption = "Derived from age-adjusted estimates above."
  ) +
  theme_klean()
save_both(p_delta, "main_prev_any_binge_alladults_AGEADJ2000_delta_pp")





suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(readr); library(scales)
})

# Safety: figure dir
if (!exists("fig_dir")) {
  PROJ_DIR <- tryCatch(
    normalizePath("C:/Users/Anigma PC/OneDrive - Tulane University/Desktop/Binge Drinking Systematic Review/binge-drinking-pilot",
                  winslash = "/", mustWork = TRUE),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  fig_dir <- file.path(PROJ_DIR, "outputs/main/figures")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
}
theme_klean <- function(base_size = 12){
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey30"),
          plot.caption = element_text(size = rel(0.9), color = "grey40"),
          legend.position = "bottom")
}
save_both <- function(p, fname, w = 9, h = 5, dpi = 320){
  ggsave(file.path(fig_dir, paste0(fname, ".png")), p, width = w, height = h, units = "in", dpi = dpi, bg = "white")
  ggsave(file.path(fig_dir, paste0(fname, ".svg")), p, width = w, height = h, units = "in", dpi = dpi, bg = "white")
}

# 1) OVERALL (2011–2024 pooled): prevalence by age7 ----------------------------
prev_age7_overall <- survey::svyby(
  ~ any_binge_cdc, ~ age7, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  transmute(
    age7 = factor(age7, levels = levels(des_all$variables$age7)),
    est  = any_binge_cdc, lcl = ci_l, ucl = ci_u,
    est_pct = est*100, lcl_pct = lcl*100, ucl_pct = ucl*100
  ) %>%
  arrange(age7)

# Export (pooled)
readr::write_csv(prev_age7_overall,
                 file.path(PROJ_DIR, "outputs/main/tables/prev_any_binge_by_age7_pooled.csv"))

# Plot (pooled)
p_age7_pooled <- ggplot(prev_age7_overall, aes(x = age7, y = est_pct)) +
  geom_linerange(aes(ymin = lcl_pct, ymax = ucl_pct), linewidth = 1) +
  geom_point(size = 2.4) +
  labs(title = "Any binge drinking by age band (pooled 2011–2024)",
       subtitle = "BRFSS, 50 states + DC; survey-weighted; 95% CI",
       y = "Prevalence (%)", x = "Age band",
       caption = "Note: Not age-standardized since age is the stratifier.") +
  theme_klean()
save_both(p_age7_pooled, "prev_any_binge_by_age7_pooled_dotCI")

# 2) TRENDS: prevalence by year × age7 -----------------------------------------
prev_age7_byyear <- survey::svyby(
  ~ any_binge_cdc, ~ year + age7, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  transmute(
    year = as.integer(year),
    age7 = factor(age7, levels = levels(des_all$variables$age7)),
    est  = any_binge_cdc, lcl = ci_l, ucl = ci_u,
    est_pct = est*100, lcl_pct = lcl*100, ucl_pct = ucl*100
  ) %>%
  arrange(age7, year)

# Export (by year × age7)
readr::write_csv(prev_age7_byyear,
                 file.path(PROJ_DIR, "outputs/main/tables/prev_any_binge_by_year_age7.csv"))

# (A) Faceted small-multiples (cleanest read)
p_age7_facets <- ggplot(prev_age7_byyear, aes(x = year, y = est_pct)) +
  geom_ribbon(aes(ymin = lcl_pct, ymax = ucl_pct), alpha = 0.18) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.9) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
  labs(title = "Any binge drinking by age band over time",
       subtitle = "BRFSS, 50 states + DC; survey-weighted; 95% CI ribbons",
       y = "Prevalence (%)", x = NULL) +
  facet_wrap(~ age7, ncol = 4) +
  theme_klean()
save_both(p_age7_facets, "prev_any_binge_by_year_age7_facets", w = 10.5, h = 7)

# (B) Single-panel multi-line (quick glance; more crowded)
p_age7_multiline <- ggplot(prev_age7_byyear,
                           aes(x = year, y = est_pct, color = age7, group = age7)) +
  geom_line(linewidth = 1) + geom_point(size = 1.6) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
  labs(title = "Any binge drinking trend by age band",
       subtitle = "BRFSS, 50 states + DC; survey-weighted",
       y = "Prevalence (%)", x = NULL, color = "Age band") +
  theme_klean()
save_both(p_age7_multiline, "prev_any_binge_by_year_age7_multiline")

# 3) Heatmap (year × age7) — fast pattern scan --------------------------------
p_age7_heat <- ggplot(prev_age7_byyear, aes(x = year, y = age7, fill = est_pct)) +
  geom_tile() +
  scale_fill_gradient(name = "Prevalence (%)", low = "white", high = "black") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Any binge drinking: heatmap by year and age band",
       subtitle = "Darker = higher prevalence; survey-weighted",
       x = NULL, y = NULL) +
  theme_klean() + theme(legend.position = "right")
save_both(p_age7_heat, "prev_any_binge_by_year_age7_heatmap", w = 9.5, h = 4.8)

# —— Optional variants you may want ————————————————————————————————
# Use 10-bin age groups instead of 7-band
# prev_age10_byyear <- survey::svyby(
#   ~ any_binge_cdc, ~ year + age_group, des_all, survey::svymean,
#   na.rm = TRUE, vartype = c("ci")
# ) %>% tibble::as_tibble()

# Restrict to current drinkers only (is_current defined in your update(des_all))
# prev_age7_byyear_current <- survey::svyby(
#   ~ any_binge_cdc, ~ year + age7, subset(des_all, is_current), survey::svymean,
#   na.rm = TRUE, vartype = c("ci")
# )


# === Prevalence of any binge among CURRENT drinkers, by year × age7 ===
prev_curr_age7 <- survey::svyby(
  ~ any_binge_cdc, ~ year + age7, des_curr, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    year = as.integer(year),
    age7 = as.character(age7),
    est  = any_binge_cdc,
    lcl  = ci_l,
    ucl  = ci_u
  ) %>%
  dplyr::arrange(age7, year)

# Save both proportion and percent versions
readr::write_csv(prev_curr_age7,
                 file.path(tab_dir, "prev_any_binge_CURRENT_by_year_age7_CI_prop.csv"))

readr::write_csv(
  prev_curr_age7 %>% dplyr::mutate(
    est = est*100, lcl = lcl*100, ucl = ucl*100,
    ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
  ),
  file.path(tab_dir, "prev_any_binge_CURRENT_by_year_age7_CI_pct.csv")
)

# === Plot (faceted small multiples keep it readable) ===
library(ggplot2)
p_age_curr <- prev_curr_age7 %>%
  dplyr::mutate(est_pct = est*100, lcl_pct = lcl*100, ucl_pct = ucl*100) %>%
  ggplot(aes(x = year, y = est_pct, group = age7)) +
  geom_ribbon(aes(ymin = lcl_pct, ymax = ucl_pct, fill = age7), alpha = 0.12, color = NA) +
  geom_line(aes(color = age7), linewidth = 1) +
  facet_wrap(~ age7, ncol = 3) +
  scale_y_continuous(name = "Any binge (%)") +
  scale_x_continuous(name = "Year", breaks = scales::pretty_breaks()) +
  guides(color = "none", fill = "none") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "fig_age_trends_CURRENT_drinkers_age7.png"),
       p_age_curr, width = 10, height = 8, dpi = 300)


