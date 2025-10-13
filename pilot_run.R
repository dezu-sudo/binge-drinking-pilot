# ===== 0) Project paths (EDIT ONLY THESE IF YOUR FILENAMES DIFFER) =====
PROJ_DIR <- "C:/Users/Anigma PC/OneDrive - Tulane University/Desktop/Binge Drinking Systematic Review/binge-drinking-pilot"
XPT_2011 <- file.path(PROJ_DIR, "data/raw/2011/LLCP2011.XPT")
XPT_2015 <- file.path(PROJ_DIR, "data/raw/2015/LLCP2015.XPT_")
XPT_2019 <- file.path(PROJ_DIR, "data/raw/2019/LLCP2019.XPT_")

# Create output dirs

dirs <- file.path(PROJ_DIR, c("outputs/tables","outputs/figures","docs"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ===== 1) Packages =====
pkgs <- c("tidyverse","janitor","haven","survey","srvyr","readr","scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
suppressPackageStartupMessages({
  library(tidyverse); library(janitor); library(haven)
  library(survey); library(srvyr); library(readr); library(scales)
})

# Save session info
writeLines(capture.output(sessionInfo()), file.path(PROJ_DIR, "docs/sessionInfo.txt"))

options(survey.lonely.psu = "adjust")  # conservative variance handling

# ===== 2) Helpers =====

# ===== Parallel toggle =====
USE_PARALLEL <- FALSE  # set TRUE when running many groups/years/states

if (USE_PARALLEL) {
  if (!requireNamespace("future", quietly = TRUE)) install.packages(c("future","furrr","progressr"))
  library(future); library(furrr); library(progressr)
  plan(multisession, workers = max(1, future::availableCores() - 1))
  handlers(global = TRUE)  # progress bars
}



# (a) robust resolver & minimal reader
find_var <- function(df, candidates){
  nms <- tolower(names(df))
  for (cand in tolower(candidates)) {
    hit <- which(nms == cand)
    if (length(hit)) return(names(df)[hit[1]])
  }
  # also try without leading underscores
  nms2 <- sub("^_", "", nms)
  for (cand in tolower(candidates)) {
    hit <- which(nms2 == sub("^_", "", cand))
    if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}

read_year_min <- function(yr, path){
  df <- haven::read_xpt(path) |>
    janitor::clean_names()
  
  v <- list(
    wt     = find_var(df, c("_llcpwt","llcpwt","x_llcpwt","cllcpwt")),
    strata = find_var(df, c("_ststr","ststr","x_ststr")),
    psu    = find_var(df, c("_psu","psu","x_psu")),
    alcday = find_var(df, c("alcday5")),
    binge  = find_var(df, c("drnk3ge5","_drnk3ge5")),
    max    = find_var(df, c("maxdrnks","_maxdrnks")),
    aved   = find_var(df, c("avedrnk3","averdrnk3","avedrnk2","_avedrnk")),
    sex    = find_var(df, c("sexvar","sex")),
    age5   = find_var(df, c("_ageg5yr","ageg5yr","x_ageg5yr")),
    educag = find_var(df, c("educag","_educag","x_educag")),
    educa  = find_var(df, c("educa")),
    rfbing = find_var(df, c("_rfbing5","rfbing5"))
  )
  
  must <- c(v$wt, v$strata, v$psu, v$alcday, v$binge, v$sex, v$age5)
  if (any(is.na(must))) {
    message("Missing key variables for survey year ", yr, ". Resolved names:\n",
            paste(sprintf("%10s : %s", names(v), unlist(v)), collapse = "\n"),
            "\n\nAvailable columns:\n", paste(names(df), collapse=", "))
    stop("Update candidate mappings for this file.")
  }
  
  dplyr::transmute(
    df,
    year = as.integer(yr),
    wt   = .data[[v$wt]],
    strata = .data[[v$strata]],
    psu    = .data[[v$psu]],
    alcday5 = .data[[v$alcday]],
    binge_episodes_raw = .data[[v$binge]],
    max_drinks_raw     = if (!is.na(v$max))  .data[[v$max]]  else NA,
    averdrnk_raw       = if (!is.na(v$aved)) .data[[v$aved]] else NA,
    sex_raw            = .data[[v$sex]],
    age5               = .data[[v$age5]],
    educag             = if (!is.na(v$educag)) .data[[v$educag]] else NA,
    educa              = if (!is.na(v$educa))  .data[[v$educa]]  else NA,
    rfbing5            = if (!is.na(v$rfbing)) .data[[v$rfbing]] else NA
  )
}

# (b) recode to analysis-ready
recode_analysis <- function(d){
  na_77_99 <- function(x) ifelse(x %in% c(7,8,9,77,88,99,777,888,999), NA, x)
  
  d %>%
    mutate(
      binge_episodes_clean = case_when(
        !is.na(binge_episodes_raw) & binge_episodes_raw %in% 1:76 ~ as.integer(binge_episodes_raw),
        !is.na(binge_episodes_raw) & binge_episodes_raw == 88     ~ 0L,
        !is.na(binge_episodes_raw) & binge_episodes_raw %in% c(77,99) ~ NA_integer_,
        TRUE ~ NA_integer_
      ),
      binge_episodes = case_when(
        !is.na(binge_episodes_clean) ~ binge_episodes_clean,
        is.na(binge_episodes_clean) & alcday5 == 888 ~ 0L,
        TRUE ~ NA_integer_
      ),
      any_binge_cdc = case_when(
        !is.na(rfbing5) & as.integer(rfbing5) == 2 ~ 1L,
        !is.na(rfbing5) & as.integer(rfbing5) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      any_binge_cdc = if_else(
        is.na(any_binge_cdc),
        case_when(
          is.na(alcday5)      ~ NA_integer_,
          binge_episodes > 0  ~ 1L,
          binge_episodes <= 0 ~ 0L,
          TRUE ~ NA_integer_
        ),
        any_binge_cdc
      ),
      freq5 = case_when(
        is.na(binge_episodes)   ~ NA_character_,
        binge_episodes == 0     ~ "none (0)",
        binge_episodes == 1     ~ "monthly (1)",
        binge_episodes %in% 2:3 ~ "a few times monthly (2–3)",
        binge_episodes %in% 4:7 ~ "weekly (4–7)",
        binge_episodes >= 8     ~ "≥2×/week (≥8)"
      ),
      max_drinks = na_77_99(max_drinks_raw),
      averdrinks = na_77_99(averdrnk_raw),
      sex = case_when(sex_raw %in% c(1,2) ~ as.integer(sex_raw), TRUE ~ NA_integer_),
      age_group = case_when(
        age5 %in% 1 ~ "18–24",
        age5 %in% 2:3 ~ "25–34",
        age5 %in% 4:6 ~ "35–49",
        age5 %in% 7:9 ~ "50–64",
        age5 %in% 10:13 ~ "65+",
        TRUE ~ NA_character_
      ),
      educ4 = case_when(
        !is.na(educag) ~ dplyr::recode(as.character(educag),
                                       `1`="<HS", `2`="HS/GED", `3`="Some college/AA", `4`="College",
                                       .default = NA_character_),
        is.na(educag) ~ case_when(
          educa %in% c(1,2) ~ "<HS",
          educa %in% c(3)   ~ "HS/GED",
          educa %in% c(4,5) ~ "Some college/AA",
          educa %in% c(6)   ~ "College",
          TRUE ~ NA_character_
        )
      )
    ) %>%
    select(
      year, wt, strata, psu, sex, age_group, educ4,
      alcday5, rfbing5,
      binge_episodes, any_binge_cdc, freq5,
      max_drinks, averdrinks
    ) %>%
    mutate(
      psu = as.character(psu), strata = as.character(strata),
      wt  = as.numeric(wt),    year   = as.integer(year),
      any_binge_cdc = as.integer(any_binge_cdc),
      binge_episodes = as.integer(binge_episodes)
    )
}

# (c) small utilities
clamp01 <- function(x) pmax(0, pmin(1, x))

est_ci <- function(des){
  e  <- survey::svymean(~any_binge_cdc, des, na.rm=TRUE)
  est <- as.numeric(e)
  se  <- as.numeric(SE(e))
  tibble::tibble(
    est = est,
    se  = se,
    lcl = est - 1.96*se,
    ucl = est + 1.96*se
  )
}

svy_ci_by <- function(df, group_var) {
  df %>%
    filter(!is.na({{ group_var }})) %>%
    srvyr::as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE) %>%
    group_by({{ group_var }}) %>%
    summarize(prop = srvyr::survey_mean(any_binge_cdc == 1, vartype = "ci", na.rm = TRUE)) %>%
    ungroup()
}

svy_ci_by_curr <- function(df, group_var) {
  df %>%
    filter(alcday5 != 888, !is.na(alcday5), !is.na({{ group_var }})) %>%
    srvyr::as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE) %>%
    group_by({{ group_var }}) %>%
    summarize(prop = srvyr::survey_mean(any_binge_cdc == 1, vartype = "ci", na.rm = TRUE)) %>%
    ungroup()
}

# unique names to avoid collisions
pairwise_diffs_denoms <- function(df, label){
  dd <- df %>% mutate(se = (ucl - lcl) / (2 * 1.96)) %>% select(year, est, se)
  pairs <- tibble::tibble(year1 = c(2011, 2015, 2011), year2 = c(2015, 2019, 2019))
  pairs %>%
    left_join(dd, by = c("year1" = "year")) %>%
    rename(est1 = est, se1 = se) %>%
    left_join(dd, by = c("year2" = "year")) %>%
    rename(est2 = est, se2 = se) %>%
    mutate(
      diff_pp = (est2 - est1) * 100,
      sed     = sqrt(se1^2 + se2^2),
      z       = ( (est2 - est1) / sed ),
      p_two   = 2 * (1 - pnorm(abs(z)))
    ) %>%
    select(year1, year2, diff_pp, z, p_two) %>%
    mutate(denom = label, .before = 1)
}

pairwise_diffs_within <- function(df, group_col, group_value) {
  dd <- df %>%
    filter({{ group_col }} == group_value) %>%
    mutate(se = (ucl - lcl) / (2 * 1.96)) %>%
    select(year, est, se)
  pairs <- tibble::tibble(year1 = c(2011, 2015, 2011), year2 = c(2015, 2019, 2019))
  pairs %>%
    left_join(dd, by = c("year1" = "year")) %>%
    rename(est1 = est, se1 = se) %>%
    left_join(dd, by = c("year2" = "year")) %>%
    rename(est2 = est, se2 = se) %>%
    mutate(
      diff_pp = (est2 - est1) * 100,
      sed     = sqrt(se1^2 + se2^2),
      z       = ( (est2 - est1) / sed ),
      p_two   = 2 * (1 - pnorm(abs(z)))
    ) %>%
    select(year1, year2, diff_pp, z, p_two) %>%
    mutate(group = group_value, .before = 1)
}

# trend test helpers (same behavior)
trend_test_sex <- function(des, sex_value, label) {
  dsub <- subset(des, sex == sex_value)
  dsub$variables$year_c <- (dsub$variables$year - 2011)/4
  fit <- svyglm(any_binge_cdc ~ year_c, design = dsub, family = quasibinomial())
  co  <- summary(fit)$coefficients["year_c", ]
  tibble::tibble(
    group  = label,
    slope_logit = unname(co["Estimate"]),
    se          = unname(co["Std. Error"]),
    z           = unname(co["Estimate"]/co["Std. Error"]),
    p_two       = 2*(1 - pnorm(abs(z)))
  )
}

trend_test_age <- function(des, age_value, label) {
  dsub <- subset(des, age_group == age_value)
  dsub$variables$year_c <- (dsub$variables$year - 2011)/4
  fit <- svyglm(any_binge_cdc ~ year_c, design = dsub, family = quasibinomial())
  co  <- summary(fit)$coefficients["year_c", ]
  tibble::tibble(
    group  = label,
    slope_logit = unname(co["Estimate"]),
    se          = unname(co["Std. Error"]),
    z           = unname(co["Estimate"]/co["Std. Error"]),
    p_two       = 2*(1 - pnorm(abs(z)))
  )
}

# ===== 3) Read, harmonize, filter to valid design rows =====
df11 <- read_year_min(2011, XPT_2011) %>% recode_analysis() %>% filter(!is.na(wt), !is.na(strata), !is.na(psu))
df15 <- read_year_min(2015, XPT_2015) %>% recode_analysis() %>% filter(!is.na(wt), !is.na(strata), !is.na(psu))
df19 <- read_year_min(2019, XPT_2019) %>% recode_analysis() %>% filter(!is.na(wt), !is.na(strata), !is.na(psu))

# Quick QC
qc <- function(d,y){
  tibble::tibble(
    year = y,
    n = nrow(d),
    miss_any_binge_cdc = sum(is.na(d$any_binge_cdc)),
    prev_any_binge_cdc = mean(d$any_binge_cdc==1, na.rm=TRUE)
  )
}
bind_rows(qc(df11,2011), qc(df15,2015), qc(df19,2019)) %>% print(n=Inf)

# ===== 4) Survey designs (per-year + pooled) =====
des11 <- svydesign(id=~psu, strata=~strata, weights=~wt, data=df11, nest=TRUE)
des15 <- svydesign(id=~psu, strata=~strata, weights=~wt, data=df15, nest=TRUE)
des19 <- svydesign(id=~psu, strata=~strata, weights=~wt, data=df19, nest=TRUE)

# Pooled data with factor strata/psu (stable; avoids numeric coercion warnings)
stacked <- dplyr::bind_rows(df11, df15, df19) %>%
  dplyr::mutate(
    strata = interaction(year, strata, drop = TRUE, lex.order = TRUE),  # factor
    psu    = interaction(year, psu,    drop = TRUE, lex.order = TRUE)   # factor
  )

des_all <- survey::svydesign(id = ~psu, strata = ~strata, weights = ~wt,
                             data = stacked, nest = TRUE)

# Collapse lonely strata once, so subgroup models don’t hit degf=0
des_all <- survey::collapse.strata(des_all)

# Precompute modeling covariates ONCE (speeds up downstream fits)
des_all <- update(
  des_all,
  year_c = (year - 2011) / 4,  # 0,1,2
  sex_f  = factor(sex, levels = c(1,2), labels = c("Male","Female")),
  age_f  = factor(ifelse(age_group %in% c("18–24","25–34"), age_group, NA))
)

# ===== 5) Minimal outputs & directories =====
tab_dir <- file.path(PROJ_DIR, "outputs/tables")
fig_dir <- file.path(PROJ_DIR, "outputs/figures")

prev_any <- tibble(
  year = c(2011,2015,2019),
  est  = c(as.numeric(svymean(~any_binge_cdc, des11, na.rm=TRUE)),
           as.numeric(svymean(~any_binge_cdc, des15, na.rm=TRUE)),
           as.numeric(svymean(~any_binge_cdc, des19, na.rm=TRUE))),
  se   = c(SE(svymean(~any_binge_cdc, des11, na.rm=TRUE)),
           SE(svymean(~any_binge_cdc, des15, na.rm=TRUE)),
           SE(svymean(~any_binge_cdc, des19, na.rm=TRUE)))
)
write_csv(prev_any, file.path(tab_dir,"prev_any_binge_cdc_overall.csv"))
print(prev_any)

# ===== 6) Current drinkers (exclude 888) =====
prev_any_current <- tibble(
  year = c(2011,2015,2019),
  est  = c(
    as.numeric(svymean(~any_binge_cdc, subset(des11, alcday5 != 888 & !is.na(alcday5)), na.rm=TRUE)),
    as.numeric(svymean(~any_binge_cdc, subset(des15, alcday5 != 888 & !is.na(alcday5)), na.rm=TRUE)),
    as.numeric(svymean(~any_binge_cdc, subset(des19, alcday5 != 888 & !is.na(alcday5)), na.rm=TRUE))
  )
)
write_csv(prev_any_current, file.path(tab_dir,"prev_any_binge_cdc_current_drinkers.csv"))
print(prev_any_current)

prev_any_current_CI <- bind_rows(
  est_ci(subset(des11, alcday5 != 888 & !is.na(alcday5))) %>% mutate(year=2011),
  est_ci(subset(des15, alcday5 != 888 & !is.na(alcday5))) %>% mutate(year=2015),
  est_ci(subset(des19, alcday5 != 888 & !is.na(alcday5))) %>% mutate(year=2019)
) %>% select(year, est, lcl, ucl) %>%
  mutate(lcl = clamp01(lcl), ucl = clamp01(ucl))
write_csv(prev_any_current_CI, file.path(tab_dir,"prev_any_binge_cdc_currentdrinkers_CI.csv"))
print(prev_any_current_CI)

# ===== 7) All adults: CIs by year =====
prev_any_all_CI <- bind_rows(
  est_ci(des11) %>% mutate(year = 2011),
  est_ci(des15) %>% mutate(year = 2015),
  est_ci(des19) %>% mutate(year = 2019)
) %>% select(year, est, lcl, ucl) %>%
  mutate(lcl = clamp01(lcl), ucl = clamp01(ucl))
write_csv(prev_any_all_CI, file.path(tab_dir, "prev_any_binge_cdc_alladults_CI.csv"))
print(prev_any_all_CI)

# ===== 8) Side-by-side table & plot =====
stopifnot(exists("prev_any_current_CI"))

prev_any_both_CI <- bind_rows(
  prev_any_all_CI %>% mutate(denom = "All adults"),
  prev_any_current_CI %>% mutate(denom = "Current drinkers")
) %>% arrange(denom, year)

write_csv(prev_any_both_CI, file.path(tab_dir, "prev_any_binge_cdc_both_denoms_CI_prop.csv"))

prev_any_both_CI_pct <- prev_any_both_CI %>%
  mutate(across(c(est,lcl,ucl), ~ .x * 100),
         ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)) %>%
  select(denom, year, est, lcl, ucl, ci)
write_csv(prev_any_both_CI_pct, file.path(tab_dir, "prev_any_binge_cdc_both_denoms_CI_pct.csv"))
print(prev_any_both_CI_pct, n = Inf)

plot_df <- prev_any_both_CI_pct
p <- ggplot(plot_df, aes(x = year, y = est, color = denom, fill = denom)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(plot_df$year))) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(x = "Year", y = "Any binge prevalence (%)",
       title = "BRFSS Any-Binge Prevalence (All Adults vs Current Drinkers)",
       subtitle = "Primary denominator: All adults; Sensitivity: Current drinkers",
       color = "Denominator") +
  guides(fill = "none") + theme_minimal(base_size = 12)
print(p)
ggsave(filename = file.path(fig_dir, "prev_any_binge_both_denoms_pilot.png"),
       plot = p, width = 8, height = 5, dpi = 300)

# ===== 9) Year-to-year differences (denoms) =====
diff_all  <- pairwise_diffs_denoms(prev_any_all_CI, "All adults")
diff_curr <- pairwise_diffs_denoms(prev_any_current_CI, "Current drinkers")
diff_both <- bind_rows(diff_all, diff_curr) %>% arrange(denom, year1, year2)
write_csv(diff_both, file.path(tab_dir, "prev_any_binge_pairwise_diffs_pilot.csv"))
print(diff_both, n = Inf)

# ===== 10) All adults: Sex × Year CIs & diffs =====
sex11 <- svy_ci_by(df11, sex) %>% mutate(year = 2011)
sex15 <- svy_ci_by(df15, sex) %>% mutate(year = 2015)
sex19 <- svy_ci_by(df19, sex) %>% mutate(year = 2019)

prev_sex_year_CI <- bind_rows(sex11, sex15, sex19) %>%
  mutate(sex = recode(as.integer(sex), `1`="Male", `2`="Female")) %>%
  transmute(sex, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  arrange(sex, year)

write_csv(prev_sex_year_CI, file.path(tab_dir, "prev_any_binge_alladults_sex_year_CI_prop.csv"))
prev_sex_year_CI_pct <- prev_sex_year_CI %>%
  mutate(across(c(est,lcl,ucl), ~ .x * 100),
         ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
write_csv(prev_sex_year_CI_pct, file.path(tab_dir, "prev_any_binge_alladults_sex_year_CI_pct.csv"))
print(prev_sex_year_CI_pct, n = Inf)

p_sex <- ggplot(prev_sex_year_CI_pct,
                aes(x = year, y = est, color = sex, fill = sex, group = sex)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  labs(x="Year", y="Any binge (%)", title="Any-Binge Prevalence by Sex (All Adults)") +
  guides(fill = "none") + theme_minimal(base_size = 12)
print(p_sex)
ggsave(file.path(fig_dir, "prev_any_binge_alladults_by_sex_pilot.png"),
       p_sex, width = 8, height = 5, dpi = 300)

sex_diffs <- bind_rows(
  pairwise_diffs_within(prev_sex_year_CI, sex, "Male"),
  pairwise_diffs_within(prev_sex_year_CI, sex, "Female")
)
write_csv(sex_diffs, file.path(tab_dir, "prev_any_binge_alladults_sex_year_diffs.csv"))
print(sex_diffs, n = Inf)

# ===== 11) All adults: Young-adult (18–24, 25–34) CIs & plot =====
age_keep <- c("18–24","25–34")
svy_ci_by_age <- function(df) { df %>% filter(age_group %in% age_keep) %>% svy_ci_by(age_group) }

prev_age_year_CI <- bind_rows(
  svy_ci_by_age(df11) %>% mutate(year = 2011),
  svy_ci_by_age(df15) %>% mutate(year = 2015),
  svy_ci_by_age(df19) %>% mutate(year = 2019)
) %>%
  transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  arrange(age_group, year)

write_csv(prev_age_year_CI, file.path(tab_dir, "prev_any_binge_alladults_age18_34_year_CI_prop.csv"))
prev_age_year_CI_pct <- prev_age_year_CI %>%
  mutate(across(c(est,lcl,ucl), ~ .x * 100),
         ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
write_csv(prev_age_year_CI_pct, file.path(tab_dir, "prev_any_binge_alladults_age18_34_year_CI_pct.csv"))
print(prev_age_year_CI_pct, n = Inf)

p_age <- ggplot(prev_age_year_CI_pct,
                aes(x = year, y = est, color = age_group, fill = age_group, group = age_group)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  labs(x="Year", y="Any binge (%)",
       title="Any-Binge Prevalence: Young Adults (All Adults Denominator)") +
  guides(fill = "none") + theme_minimal(base_size = 12)
print(p_age)
ggsave(file.path(fig_dir, "prev_any_binge_alladults_by_age1824_2534_pilot.png"),
       p_age, width = 8, height = 5, dpi = 300)

# ===== 12) Trend tests (all adults) — FAST interaction models =====
# Model 1: Sex-specific trend in one fit
fit_sex <- survey::svyglm(any_binge_cdc ~ year_c * sex_f,
                          design = des_all, family = quasibinomial())

co_s  <- summary(fit_sex)$coefficients
V_s   <- vcov(fit_sex)

# Slopes per sex (Male = reference)
slope_male   <- unname(co_s["year_c","Estimate"])
se_male      <- sqrt(V_s["year_c","year_c"])
slope_female <- unname(co_s["year_c","Estimate"] + co_s["year_c:sex_fFemale","Estimate"])
se_female    <- sqrt(V_s["year_c","year_c"] +
                       V_s["year_c:sex_fFemale","year_c:sex_fFemale"] +
                       2*V_s["year_c","year_c:sex_fFemale"])

z_m <- slope_male / se_male
z_f <- slope_female / se_female
p_m <- 2*(1 - pnorm(abs(z_m)))
p_f <- 2*(1 - pnorm(abs(z_f)))

# Model 2: Young-adult age trend (18–24 vs 25–34) in one fit
des_age <- subset(des_all, !is.na(age_f))
fit_age <- survey::svyglm(any_binge_cdc ~ year_c * age_f,
                          design = des_age, family = quasibinomial())

co_a <- summary(fit_age)$coefficients
V_a  <- vcov(fit_age)

# Slopes per age group (18–24 = reference)
slope_18  <- unname(co_a["year_c","Estimate"])
se_18     <- sqrt(V_a["year_c","year_c"])
slope_25  <- unname(co_a["year_c","Estimate"] + co_a["year_c:age_f25–34","Estimate"])
se_25     <- sqrt(V_a["year_c","year_c"] +
                    V_a["year_c:age_f25–34","year_c:age_f25–34"] +
                    2*V_a["year_c","year_c:age_f25–34"])

z_18 <- slope_18 / se_18
z_25 <- slope_25 / se_25
p_18 <- 2*(1 - pnorm(abs(z_18)))
p_25 <- 2*(1 - pnorm(abs(z_25)))

# Output table (same file name you already use)
trend_tbl <- tibble::tibble(
  group       = c("Sex: Male","Sex: Female","Age: 18–24","Age: 25–34"),
  slope_logit = c(slope_male, slope_female, slope_18, slope_25),
  se          = c(se_male,    se_female,    se_18,     se_25),
  z           = c(z_m,        z_f,          z_18,      z_25),
  p_two       = c(p_m,        p_f,          p_18,      p_25)
)
readr::write_csv(trend_tbl, file.path(tab_dir, "trend_tests_by_group_2011_2019_alladults.csv"))
print(trend_tbl)

# Model-based predicted means by year × sex (nice for plots/tables)
new_sex <- expand.grid(year = c(2011,2015,2019), sex_f = c("Male","Female"))
new_sex$year_c <- (new_sex$year - 2011)/4
pr_s <- predict(fit_sex, newdata = new_sex, type = "link", se.fit = TRUE)
pred_sex <- dplyr::bind_cols(new_sex,
                             tibble::tibble(
                               est = plogis(pr_s$fit),
                               lcl = plogis(pr_s$fit - 1.96*pr_s$se.fit),
                               ucl = plogis(pr_s$fit + 1.96*pr_s$se.fit)
                             )) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl))
readr::write_csv(pred_sex, file.path(tab_dir, "predicted_means_by_year_sex_alladults.csv"))

# Model-based predicted means by year × age (18–24,25–34)
new_age <- expand.grid(year = c(2011,2015,2019), age_f = c("18–24","25–34"))
new_age$year_c <- (new_age$year - 2011)/4
pr_a <- predict(fit_age, newdata = new_age, type = "link", se.fit = TRUE)
pred_age <- dplyr::bind_cols(new_age,
                             tibble::tibble(
                               est = plogis(pr_a$fit),
                               lcl = plogis(pr_a$fit - 1.96*pr_a$se.fit),
                               ucl = plogis(pr_a$fit + 1.96*pr_a$se.fit)
                             )) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl))
readr::write_csv(pred_age, file.path(tab_dir, "predicted_means_by_year_age18_34_alladults.csv"))







