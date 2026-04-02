# ===== 0) Project paths (EDIT ONLY THESE IF YOUR FILENAMES DIFFER) =====
# Portable PROJ_DIR: use your hard-coded path if it exists; otherwise fall back to current project dir

PROJ_DIR <- tryCatch(
  normalizePath("C:/Users/Anigma PC/OneDrive - Tulane University/Desktop/Binge Drinking Systematic Review/binge-drinking-pilot",
                winslash = "/", mustWork = TRUE),
  error = function(e) normalizePath(getwd(), winslash = "/")
)

XPT_2011 <- file.path(PROJ_DIR, "data/raw/2011/LLCP2011.XPT")
XPT_2015 <- file.path(PROJ_DIR, "data/raw/2015/LLCP2015.XPT_")
XPT_2019 <- file.path(PROJ_DIR, "data/raw/2019/LLCP2019.XPT_")

# Create output dirs BEFORE any writes (fix)
dirs <- file.path(PROJ_DIR, c("outputs/tables","outputs/figures","docs"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ===== 1) (Optional) renv activation/init — only once (fixed) =====
if (requireNamespace("renv", quietly = TRUE)) {
  lockpath <- file.path(PROJ_DIR, "renv.lock")
  if (file.exists(lockpath)) {
    renv::activate(project = PROJ_DIR)   # <- removed quiet=TRUE
  } else {
    renv::init(project = PROJ_DIR, bare = TRUE)
  }
}

# ===== 2) Packages =====
pkgs <- c("tidyverse","janitor","haven","survey","srvyr","readr","scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
suppressPackageStartupMessages({
  library(tidyverse); library(janitor); library(haven)
  library(survey); library(srvyr); library(readr); library(scales)
})

options(survey.lonely.psu = "adjust")  # conservative variance handling

# Save session info AFTER dirs exist (fix)
writeLines(capture.output(sessionInfo()), file.path(PROJ_DIR, "docs/sessionInfo.txt"))

# ===== 3) Helpers =====

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
  grp <- deparse(substitute(group_var))
  des <- survey::svydesign(id = ~psu, strata = ~strata, weights = ~wt, data = df, nest = TRUE)
  out <- survey::svyby(
    ~ any_binge_cdc,
    by = as.formula(paste("~", grp)),
    design = des,
    FUN = survey::svymean,
    na.rm = TRUE,
    vartype = c("ci")
  )
  tibble::as_tibble(out) |>
    dplyr::rename(
      prop     = any_binge_cdc,
      prop_low = ci_l,
      prop_upp = ci_u
    )
}

svy_ci_by_curr <- function(df, group_var) {
  grp <- deparse(substitute(group_var))
  df2 <- dplyr::filter(df, alcday5 != 888, !is.na(alcday5))
  des <- survey::svydesign(id = ~psu, strata = ~strata, weights = ~wt, data = df2, nest = TRUE)
  out <- survey::svyby(
    ~ any_binge_cdc,
    by = as.formula(paste("~", grp)),
    design = des,
    FUN = survey::svymean,
    na.rm = TRUE,
    vartype = c("ci")
  )
  tibble::as_tibble(out) |>
    dplyr::rename(
      prop     = any_binge_cdc,
      prop_low = ci_l,
      prop_upp = ci_u
    )
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



predict_prob_ci <- function(fit, newdata) {
  trm   <- stats::terms(fit)
  trm_x <- stats::delete.response(trm)  # drop response so it won't look for any_binge_cdc
  
  # Match factor levels used in the fit
  if (!is.null(fit$xlevels)) {
    for (nm in names(fit$xlevels)) {
      if (nm %in% names(newdata)) {
        newdata[[nm]] <- factor(newdata[[nm]], levels = fit$xlevels[[nm]])
      }
    }
  }
  
  X <- stats::model.matrix(trm_x, newdata)
  b <- stats::coef(fit)
  V <- stats::vcov(fit)
  
  # Align columns with coefficients (add 0-columns if needed; reorder)
  miss <- setdiff(names(b), colnames(X))
  if (length(miss)) {
    X <- cbind(X, matrix(0, nrow(X), length(miss), dimnames = list(NULL, miss)))
  }
  X <- X[, names(b), drop = FALSE]
  
  eta <- as.vector(X %*% b)
  se  <- sqrt(pmax(0, rowSums((X %*% V) * X)))  # diag(X V X^T) efficiently
  
  tibble::tibble(
    est = plogis(eta),
    lcl = plogis(eta - 1.96 * se),
    ucl = plogis(eta + 1.96 * se)
  )
}



# ===== 4) Read, harmonize, filter to valid design rows — with caching =====
proc_dir <- file.path(PROJ_DIR, "data/processed")
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

read_one_cached <- function(yr, path){
  rds <- file.path(proc_dir, sprintf("df_%d.rds", yr))
  if (file.exists(rds)) {
    readRDS(rds)
  } else {
    df <- read_year_min(yr, path) %>%
      recode_analysis() %>%
      dplyr::filter(!is.na(wt), !is.na(strata), !is.na(psu))
    saveRDS(df, rds, compress = "xz")
    df
  }
}

if (USE_PARALLEL) {
  yrs   <- list(2011, 2015, 2019)
  paths <- list(XPT_2011, XPT_2015, XPT_2019)
  with_progress({
    p <- progressr::progressor(along = yrs)
    res <- furrr::future_map2(yrs, paths, ~{ p(); read_one_cached(.x, .y) })
  })
  df11 <- res[[1]]; df15 <- res[[2]]; df19 <- res[[3]]
} else {
  df11 <- read_one_cached(2011, XPT_2011)
  df15 <- read_one_cached(2015, XPT_2015)
  df19 <- read_one_cached(2019, XPT_2019)
}


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

# ===== 5) Survey designs (per-year + pooled) =====
des11 <- svydesign(id=~psu, strata=~strata, weights=~wt, data=df11, nest=TRUE)
des15 <- svydesign(id=~psu, strata=~strata, weights=~wt, data=df15, nest=TRUE)
des19 <- svydesign(id=~psu, strata=~strata, weights=~wt, data=df19, nest=TRUE)

des11 <- stats::update(des11, is_current = (alcday5 != 888 & !is.na(alcday5)))
des15 <- stats::update(des15, is_current = (alcday5 != 888 & !is.na(alcday5)))
des19 <- stats::update(des19, is_current = (alcday5 != 888 & !is.na(alcday5)))

# Pooled data with numeric strata/psu (fast & stable)
stacked <- dplyr::bind_rows(df11, df15, df19) %>%
  dplyr::mutate(
    strata = as.integer(interaction(year, strata, drop = TRUE, lex.order = TRUE)),
    psu    = as.integer(interaction(year, psu,    drop = TRUE, lex.order = TRUE))
  )

des_all <- survey::svydesign(
  id = ~psu, strata = ~strata, weights = ~wt,
  data = stacked, nest = TRUE
)

# Precompute covariates once for fast trend models
des_all <- stats::update(
  des_all,
  year_c = (year - 2011) / 4,
  sex_f  = factor(sex, levels = c(1,2), labels = c("Male","Female")),
  age_f  = factor(ifelse(age_group %in% c("18–24","25–34"), age_group, NA))
)

# Overall prevalence (All adults) — create this first

prev_any <- tibble::tibble(
  year = c(2011, 2015, 2019),
  est  = c(
    as.numeric(svymean(~any_binge_cdc, des11, na.rm = TRUE)),
    as.numeric(svymean(~any_binge_cdc, des15, na.rm = TRUE)),
    as.numeric(svymean(~any_binge_cdc, des19, na.rm = TRUE))
  ),
  se = c(
    SE(svymean(~any_binge_cdc, des11, na.rm = TRUE)),
    SE(svymean(~any_binge_cdc, des15, na.rm = TRUE)),
    SE(svymean(~any_binge_cdc, des19, na.rm = TRUE))
  )
)
write_csv(prev_any, file.path(tab_dir, "prev_any_binge_cdc_overall.csv"))


# ===== 6) Minimal outputs & directories =====
tab_dir <- file.path(PROJ_DIR, "outputs/tables")
fig_dir <- file.path(PROJ_DIR, "outputs/figures")

prev_any_current <- tibble::tibble(
  year = c(2011, 2015, 2019),
  est  = c(
    as.numeric(svymean(~any_binge_cdc, subset(des11, is_current), na.rm = TRUE)),
    as.numeric(svymean(~any_binge_cdc, subset(des15, is_current), na.rm = TRUE)),
    as.numeric(svymean(~any_binge_cdc, subset(des19, is_current), na.rm = TRUE))
  )
)

write_csv(prev_any, file.path(tab_dir,"prev_any_binge_cdc_overall.csv"))
print(prev_any)

tab_dir <- file.path(PROJ_DIR, "outputs/tables")
fig_dir <- file.path(PROJ_DIR, "outputs/figures")

# ===== 7) Current drinkers (exclude 888) =====
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

prev_any_current_CI <- dplyr::bind_rows(
  est_ci(subset(des11, is_current)) %>% dplyr::mutate(year = 2011),
  est_ci(subset(des15, is_current)) %>% dplyr::mutate(year = 2015),
  est_ci(subset(des19, is_current)) %>% dplyr::mutate(year = 2019)
) %>%
  dplyr::select(year, est, lcl, ucl) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl))

write_csv(prev_any_current_CI, file.path(tab_dir,"prev_any_binge_cdc_currentdrinkers_CI.csv"))
print(prev_any_current_CI)

# ===== 8) All adults: CIs by year =====
prev_any_all_CI <- bind_rows(
  est_ci(des11) %>% mutate(year = 2011),
  est_ci(des15) %>% mutate(year = 2015),
  est_ci(des19) %>% mutate(year = 2019)
) %>% select(year, est, lcl, ucl) %>%
  mutate(lcl = clamp01(lcl), ucl = clamp01(ucl))
write_csv(prev_any_all_CI, file.path(tab_dir, "prev_any_binge_cdc_alladults_CI.csv"))
print(prev_any_all_CI)


# ===== QC 1: Compare BRFSS any-binge (all adults) against CDC published benchmarks =====
# Expected CSV (optional): docs/benchmarks_cdc_anybinge.csv with columns: year, cdc_pct
# If the CSV doesn't exist, this creates a placeholder you can edit.

tolerance <- 0.5  # allowable difference in percentage points
bench_path <- file.path(PROJ_DIR, "docs/benchmarks_cdc_anybinge.csv")

if (file.exists(bench_path)) {
  cdc_bm <- readr::read_csv(bench_path, show_col_types = FALSE) %>%
    dplyr::transmute(year = as.integer(year), cdc_pct = as.numeric(cdc_pct))
} else {
  cdc_bm <- tibble::tibble(
    year    = c(2011, 2015, 2019),
    cdc_pct = c(NA_real_, NA_real_, NA_real_)  # <-- fill with CDC % when you have them
  )
  readr::write_csv(cdc_bm, bench_path)  # saves a template you can edit
}

qc_tbl <- prev_any_all_CI %>%
  dplyr::transmute(year, est_pct = est * 100) %>%
  dplyr::left_join(cdc_bm, by = "year") %>%
  dplyr::mutate(
    diff_pp = est_pct - cdc_pct,
    pass = dplyr::case_when(
      is.na(cdc_pct) ~ NA,                     # no benchmark yet
      abs(diff_pp) <= tolerance ~ TRUE, 
      TRUE ~ FALSE
    )
  )

print(qc_tbl, n = Inf)
readr::write_csv(qc_tbl, file.path(tab_dir, "qc_vs_cdc_any_binge_all_adults.csv"))


# ===== 9) Side-by-side table & plot =====
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


# ===== QC-1: Alt binge definitions (CDC flag vs derived) =====
# Build alt variables per year
mk_alt <- function(d){
  d %>%
    dplyr::mutate(
      any_from_rfbing = dplyr::case_when(
        !is.na(rfbing5) & as.integer(rfbing5) == 2 ~ 1L,
        !is.na(rfbing5) & as.integer(rfbing5) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      any_from_derived = dplyr::case_when(
        is.na(alcday5)      ~ NA_integer_,
        binge_episodes > 0  ~ 1L,
        binge_episodes <= 0 ~ 0L,
        TRUE ~ NA_integer_
      )
    )
}
df11_alt <- mk_alt(df11); df15_alt <- mk_alt(df15); df19_alt <- mk_alt(df19)

est_ci_var <- function(df, var){
  des <- survey::svydesign(id=~psu, strata=~strata, weights=~wt, data=df, nest=TRUE)
  e   <- survey::svymean(as.formula(paste0("~", var)), des, na.rm=TRUE)
  est <- as.numeric(e); se <- as.numeric(SE(e))
  tibble::tibble(est=est, lcl=pmax(0, est-1.96*se), ucl=pmin(1, est+1.96*se))
}

alt_tbl <- dplyr::bind_rows(
  est_ci_var(df11_alt,"any_from_rfbing")   %>% dplyr::mutate(year=2011, method="CDC flag"),
  est_ci_var(df11_alt,"any_from_derived")  %>% dplyr::mutate(year=2011, method="Derived"),
  est_ci_var(df15_alt,"any_from_rfbing")   %>% dplyr::mutate(year=2015, method="CDC flag"),
  est_ci_var(df15_alt,"any_from_derived")  %>% dplyr::mutate(year=2015, method="Derived"),
  est_ci_var(df19_alt,"any_from_rfbing")   %>% dplyr::mutate(year=2019, method="CDC flag"),
  est_ci_var(df19_alt,"any_from_derived")  %>% dplyr::mutate(year=2019, method="Derived")
) %>%
  dplyr::arrange(year, method)

# Compare the two methods within each year (pp = percentage points)
alt_wide <- alt_tbl %>%
  dplyr::mutate(method = dplyr::recode(method, "CDC flag" = "CDC_flag")) %>%
  tidyr::pivot_wider(
    id_cols = year,
    names_from = method,
    values_from = c(est, lcl, ucl),
    names_glue = "{.value}_{method}"
  ) %>%
  dplyr::mutate(diff_pp = (est_Derived - est_CDC_flag) * 100)

print(alt_wide, n = Inf)
readr::write_csv(alt_wide, file.path(tab_dir, "qc_alt_binge_methods_wide.csv"))

# ===== QC-2: Lonely PSU sensitivity (adjust vs average) =====
old_lonely <- getOption("survey.lonely.psu")
on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

# Recompute overall CIs under 'average'
options(survey.lonely.psu = "average")
ci_avg <- dplyr::bind_rows(
  est_ci(des11) %>% dplyr::mutate(year=2011),
  est_ci(des15) %>% dplyr::mutate(year=2015),
  est_ci(des19) %>% dplyr::mutate(year=2019)
) %>% dplyr::select(year, est, lcl, ucl) %>% dplyr::mutate(method="average")

# Back to your default 'adjust' to fetch the baseline (already computed above as prev_any_all_CI)
options(survey.lonely.psu = "adjust")
ci_adj <- prev_any_all_CI %>% dplyr::mutate(method="adjust")

lonely_compare <- dplyr::left_join(
  ci_adj %>% dplyr::rename(est_adj=est,lcl_adj=lcl,ucl_adj=ucl) %>% dplyr::select(year, est_adj,lcl_adj,ucl_adj),
  ci_avg %>% dplyr::rename(est_avg=est,lcl_avg=lcl,ucl_avg=ucl) %>% dplyr::select(year, est_avg,lcl_avg,ucl_avg),
  by="year"
) %>%
  dplyr::mutate(
    delta_pp = (est_avg - est_adj)*100,
    width_adj = (ucl_adj - lcl_adj)*100,
    width_avg = (ucl_avg - lcl_avg)*100
  )

readr::write_csv(lonely_compare, file.path(tab_dir, "qc_lonely_psu_adjust_vs_average.csv"))
print(lonely_compare, n=Inf)




# ===== 10) Year-to-year differences (denoms) =====
diff_all  <- pairwise_diffs_denoms(prev_any_all_CI, "All adults")
diff_curr <- pairwise_diffs_denoms(prev_any_current_CI, "Current drinkers")
diff_both <- bind_rows(diff_all, diff_curr) %>% arrange(denom, year1, year2)
write_csv(diff_both, file.path(tab_dir, "prev_any_binge_pairwise_diffs_pilot.csv"))
print(diff_both, n = Inf)

# ===== 11) All adults: Sex × Year CIs & diffs =====
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

# ===== 12) All adults: Young-adult (18–24, 25–34) CIs & plot =====
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


# =====All adults: Age (5-level) × Year — CIs, plot, diffs ==== #
# ===== All adults: Age (5-level) × Year =====
age_levels <- c("18–24","25–34","35–49","50–64","65+")

age11_5 <- df11 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by(age_group) %>% dplyr::mutate(year = 2011)
age15_5 <- df15 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by(age_group) %>% dplyr::mutate(year = 2015)
age19_5 <- df19 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by(age_group) %>% dplyr::mutate(year = 2019)

prev_age5_year_CI <- dplyr::bind_rows(age11_5, age15_5, age19_5) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(prev_age5_year_CI, file.path(tab_dir, "prev_any_binge_alladults_age5_year_CI_prop.csv"))

prev_age5_year_CI_pct <- prev_age5_year_CI %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x * 100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(prev_age5_year_CI_pct, file.path(tab_dir, "prev_any_binge_alladults_age5_year_CI_pct.csv"))

# Plot (All adults, 5 ages)
p_age5 <- ggplot2::ggplot(prev_age5_year_CI_pct,
                          ggplot2::aes(x = year, y = est, color = age_group, fill = age_group, group = age_group)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, 60)) +
  ggplot2::labs(x = "Year", y = "Any binge (%)",
                title = "Any-Binge Prevalence by Age Group (All Adults)") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(fig_dir, "prev_any_binge_alladults_by_age5.png"),
                p_age5, width = 8, height = 5, dpi = 300)

# Pairwise 2011↔2015, 2015↔2019, 2011↔2019 within each age group
age5_diffs <- dplyr::bind_rows(
  pairwise_diffs_within(prev_age5_year_CI, age_group, "18–24"),
  pairwise_diffs_within(prev_age5_year_CI, age_group, "25–34"),
  pairwise_diffs_within(prev_age5_year_CI, age_group, "35–49"),
  pairwise_diffs_within(prev_age5_year_CI, age_group, "50–64"),
  pairwise_diffs_within(prev_age5_year_CI, age_group, "65+")
)
readr::write_csv(age5_diffs, file.path(tab_dir, "prev_any_binge_alladults_age5_year_diffs.csv"))

# === Current drinkers: Age (5-level) × Year — CIs, plot, diffs === + 
# ===== Current drinkers: Age (5-level) × Year =====
age11_c5 <- df11 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2011)
age15_c5 <- df15 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2015)
age19_c5 <- df19 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2019)

prev_age5_year_curr <- dplyr::bind_rows(age11_c5, age15_c5, age19_c5) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(prev_age5_year_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_prop.csv"))

prev_age5_year_curr_pct <- prev_age5_year_curr %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x * 100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(prev_age5_year_curr_pct, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_pct.csv"))

# Plot (Current drinkers, 5 ages)
p_age5_c <- ggplot2::ggplot(prev_age5_year_curr_pct,
                            ggplot2::aes(x = year, y = est, color = age_group, fill = age_group, group = age_group)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, 60)) +
  ggplot2::labs(x = "Year", y = "Any binge (%)",
                title = "Current Drinkers: Any-Binge by Age Group") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(fig_dir, "prev_any_binge_currentdrinkers_by_age5.png"),
                p_age5_c, width = 8, height = 5, dpi = 300)

# Pairwise diffs within each age group (current drinkers)
age5_diffs_curr <- dplyr::bind_rows(
  pairwise_diffs_within(prev_age5_year_curr, age_group, "18–24"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "25–34"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "35–49"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "50–64"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "65+")
)
readr::write_csv(age5_diffs_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_diffs.csv"))



# ===== Current drinkers: Age (5-level) × Year =====
age11_c5 <- df11 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2011)
age15_c5 <- df15 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2015)
age19_c5 <- df19 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2019)

prev_age5_year_curr <- dplyr::bind_rows(age11_c5, age15_c5, age19_c5) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(prev_age5_year_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_prop.csv"))

prev_age5_year_curr_pct <- prev_age5_year_curr %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x * 100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(prev_age5_year_curr_pct, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_pct.csv"))

# Plot (Current drinkers, 5 ages)
p_age5_c <- ggplot2::ggplot(prev_age5_year_curr_pct,
                            ggplot2::aes(x = year, y = est, color = age_group, fill = age_group, group = age_group)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, 60)) +
  ggplot2::labs(x = "Year", y = "Any binge (%)",
                title = "Current Drinkers: Any-Binge by Age Group") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(fig_dir, "prev_any_binge_currentdrinkers_by_age5.png"),
                p_age5_c, width = 8, height = 5, dpi = 300)

# Pairwise diffs within each age group (current drinkers)
age5_diffs_curr <- dplyr::bind_rows(
  pairwise_diffs_within(prev_age5_year_curr, age_group, "18–24"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "25–34"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "35–49"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "50–64"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "65+")
)
readr::write_csv(age5_diffs_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_diffs.csv"))


# ==== Current drinkers: Age (5-level) × Year — CIs, plot, diffs ==== #
# ===== Current drinkers: Age (5-level) × Year =====
age11_c5 <- df11 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2011)
age15_c5 <- df15 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2015)
age19_c5 <- df19 %>% dplyr::filter(age_group %in% age_levels) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2019)

prev_age5_year_curr <- dplyr::bind_rows(age11_c5, age15_c5, age19_c5) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(prev_age5_year_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_prop.csv"))

prev_age5_year_curr_pct <- prev_age5_year_curr %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x * 100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(prev_age5_year_curr_pct, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_pct.csv"))

# Plot (Current drinkers, 5 ages)
p_age5_c <- ggplot2::ggplot(prev_age5_year_curr_pct,
                            ggplot2::aes(x = year, y = est, color = age_group, fill = age_group, group = age_group)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, 60)) +
  ggplot2::labs(x = "Year", y = "Any binge (%)",
                title = "Current Drinkers: Any-Binge by Age Group") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(fig_dir, "prev_any_binge_currentdrinkers_by_age5.png"),
                p_age5_c, width = 8, height = 5, dpi = 300)

# Pairwise diffs within each age group (current drinkers)
age5_diffs_curr <- dplyr::bind_rows(
  pairwise_diffs_within(prev_age5_year_curr, age_group, "18–24"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "25–34"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "35–49"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "50–64"),
  pairwise_diffs_within(prev_age5_year_curr, age_group, "65+")
)
readr::write_csv(age5_diffs_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_diffs.csv"))




# ===== 13) Trend tests (all adults) — FAST interaction models =====
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

# Output table
trend_tbl <- tibble::tibble(
  group       = c("Sex: Male","Sex: Female","Age: 18–24","Age: 25–34"),
  slope_logit = c(slope_male, slope_female, slope_18, slope_25),
  se          = c(se_male,    se_female,    se_18,     se_25),
  z           = c(z_m,        z_f,          z_18,      z_25),
  p_two       = c(p_m,        p_f,          p_18,      p_25)
)
readr::write_csv(trend_tbl, file.path(tab_dir, "trend_tests_by_group_2011_2019_alladults.csv"))
print(trend_tbl)

# Model-based predicted means by year × sex (via X %*% beta; SE from vcov)
new_sex <- expand.grid(year = c(2011,2015,2019), sex_f = c("Male","Female"))
new_sex$year_c <- (new_sex$year - 2011)/4

TT_sex <- stats::delete.response(stats::terms(fit_sex))
X_s    <- stats::model.matrix(TT_sex, new_sex)
beta_s <- stats::coef(fit_sex)
V_s    <- stats::vcov(fit_sex)

eta_s   <- as.numeric(X_s %*% beta_s)
se_eta  <- sqrt(rowSums((X_s %*% V_s) * X_s))   # diag(X V X^T)
pred_sex <- dplyr::bind_cols(
  new_sex,
  tibble::tibble(
    est = plogis(eta_s),
    lcl = plogis(eta_s - 1.96*se_eta),
    ucl = plogis(eta_s + 1.96*se_eta)
  )
) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl))

readr::write_csv(pred_sex, file.path(tab_dir, "predicted_means_by_year_sex_alladults.csv"))


# Model-based predicted means by year × age (18–24,25–34) (via X %*% beta; SE from vcov)
new_age <- expand.grid(year = c(2011,2015,2019), age_f = c("18–24","25–34"))
new_age$year_c <- (new_age$year - 2011)/4

TT_age <- stats::delete.response(stats::terms(fit_age))
X_a    <- stats::model.matrix(TT_age, new_age)
beta_a <- stats::coef(fit_age)
V_a    <- stats::vcov(fit_age)

eta_a  <- as.numeric(X_a %*% beta_a)
se_a   <- sqrt(rowSums((X_a %*% V_a) * X_a))
pred_age <- dplyr::bind_cols(
  new_age,
  tibble::tibble(
    est = plogis(eta_a),
    lcl = plogis(eta_a - 1.96*se_a),
    ucl = plogis(eta_a + 1.96*se_a)
  )
) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl))

readr::write_csv(pred_age, file.path(tab_dir, "predicted_means_by_year_age18_34_alladults.csv"))

# === STEP 2: Export tidy tables ===

# All adults: percent file
prev_any_all_CI_pct <- prev_any_all_CI %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(prev_any_all_CI_pct,
                 file.path(tab_dir, "prev_any_binge_cdc_alladults_CI_pct.csv"))

# Current drinkers: percent file
prev_any_current_CI_pct <- prev_any_current_CI %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(prev_any_current_CI_pct,
                 file.path(tab_dir, "prev_any_binge_cdc_currentdrinkers_CI_pct.csv"))

# Both denominators (prop + percent)
readr::write_csv(prev_any_both_CI,
                 file.path(tab_dir, "prev_any_binge_cdc_both_denoms_CI_prop.csv"))
readr::write_csv(prev_any_both_CI_pct,
                 file.path(tab_dir, "prev_any_binge_cdc_both_denoms_CI_pct.csv"))

# Year-to-year differences (both denoms)
readr::write_csv(diff_both,
                 file.path(tab_dir, "prev_any_binge_pairwise_diffs_pilot.csv"))

# Sex × Year (prop + percent + diffs)
readr::write_csv(prev_sex_year_CI,
                 file.path(tab_dir, "prev_any_binge_alladults_sex_year_CI_prop.csv"))
readr::write_csv(prev_sex_year_CI_pct,
                 file.path(tab_dir, "prev_any_binge_alladults_sex_year_CI_pct.csv"))
readr::write_csv(sex_diffs,
                 file.path(tab_dir, "prev_any_binge_alladults_sex_year_diffs.csv"))

# Young-adult Age × Year (prop + percent)
readr::write_csv(prev_age_year_CI,
                 file.path(tab_dir, "prev_any_binge_alladults_age18_34_year_CI_prop.csv"))
readr::write_csv(prev_age_year_CI_pct,
                 file.path(tab_dir, "prev_any_binge_alladults_age18_34_year_CI_pct.csv"))

# Trend tests and model-based predicted means
readr::write_csv(trend_tbl,
                 file.path(tab_dir, "trend_tests_by_group_2011_2019_alladults.csv"))
readr::write_csv(pred_sex,
                 file.path(tab_dir, "predicted_means_by_year_sex_alladults.csv"))
readr::write_csv(pred_age,
                 file.path(tab_dir, "predicted_means_by_year_age18_34_alladults.csv"))

# ===== STEP 3: Publication-quality figures =====

# Palette (Okabe–Ito subset for CVD-safe colors)
pal <- c(
  "All adults"        = "#0072B2",
  "Current drinkers"  = "#D55E00",
  "Male"              = "#0072B2",
  "Female"            = "#D55E00",
  "18–24"             = "#009E73",
  "25–34"             = "#CC79A7"
)

theme_pub <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30")
    )
}

save_both <- function(plot, name, width = 8, height = 5) {
  ggplot2::ggsave(file.path(fig_dir, paste0(name, ".png")), plot,
                  width = width, height = height, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, paste0(name, ".pdf")), plot,
                  width = width, height = height, device = "pdf")
}

# --- Fig A: All adults vs Current drinkers ---
plot_df <- prev_any_both_CI_pct %>%
  dplyr::mutate(denom = factor(denom, levels = c("All adults","Current drinkers")))

p_both <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = year, y = est, color = denom, fill = denom, group = denom)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl),
                       alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(values = pal[c("All adults","Current drinkers")]) +
  ggplot2::scale_fill_manual(values = pal[c("All adults","Current drinkers")]) +
  ggplot2::scale_x_continuous(breaks = sort(unique(plot_df$year))) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  ggplot2::labs(
    title = "Any-binge prevalence: All adults vs Current drinkers",
    subtitle = "Weighted BRFSS estimates with 95% CIs (2011, 2015, 2019)",
    x = "Year", y = "Prevalence (%)"
  ) + theme_pub()
print(p_both)
save_both(p_both, "fig_any_binge_all_vs_current")

# --- Fig B: By Sex (All adults denominator) ---
sex_df <- prev_sex_year_CI_pct %>%
  dplyr::mutate(sex = factor(sex, levels = c("Male","Female")))

p_sex_pub <- ggplot2::ggplot(sex_df,
                             ggplot2::aes(x = year, y = est, color = sex, fill = sex, group = sex)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl),
                       alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(values = pal[c("Male","Female")]) +
  ggplot2::scale_fill_manual(values = pal[c("Male","Female")]) +
  ggplot2::scale_x_continuous(breaks = c(2011,2015,2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  ggplot2::labs(
    title = "Any-binge prevalence by sex (All adults)",
    subtitle = "Weighted BRFSS estimates with 95% CIs",
    x = "Year", y = "Prevalence (%)"
  ) + theme_pub()
print(p_sex_pub)
save_both(p_sex_pub, "fig_any_binge_alladults_by_sex")

# --- Fig C: Young adults (18–24, 25–34; All adults denominator) ---
age_df <- prev_age_year_CI_pct %>%
  dplyr::mutate(age_group = factor(age_group, levels = c("18–24","25–34")))

p_age_pub <- ggplot2::ggplot(age_df,
                             ggplot2::aes(x = year, y = est, color = age_group, fill = age_group, group = age_group)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl),
                       alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(values = pal[c("18–24","25–34")]) +
  ggplot2::scale_fill_manual(values = pal[c("18–24","25–34")]) +
  ggplot2::scale_x_continuous(breaks = c(2011,2015,2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  ggplot2::labs(
    title = "Any-binge prevalence among young adults (All adults denominator)",
    subtitle = "Weighted BRFSS estimates with 95% CIs",
    x = "Year", y = "Prevalence (%)"
  ) + theme_pub()
print(p_age_pub)
save_both(p_age_pub, "fig_any_binge_alladults_by_age18_34")

# ===== 5) Package outputs — report-ready tables =====
tab_dir <- file.path(PROJ_DIR, "outputs/tables")

# 5.1 Overall (All adults vs Current drinkers), wide (cells = % with 95% CI)
overall_wide <- prev_any_both_CI_pct %>%
  dplyr::select(denom, year, ci) %>%
  tidyr::pivot_wider(names_from = year, values_from = ci) %>%
  dplyr::arrange(match(denom, c("All adults","Current drinkers")))
readr::write_csv(overall_wide, file.path(tab_dir, "table_overall_by_year_wide.csv"))

# 5.2 Sex × Year, wide
sex_wide <- prev_sex_year_CI_pct %>%
  dplyr::select(sex, year, ci) %>%
  tidyr::pivot_wider(names_from = year, values_from = ci) %>%
  dplyr::arrange(match(sex, c("Male","Female")))
readr::write_csv(sex_wide, file.path(tab_dir, "table_sex_by_year_wide.csv"))

# 5.3 Young-adult Age × Year, wide
age_wide <- prev_age_year_CI_pct %>%
  dplyr::select(age_group, year, ci) %>%
  tidyr::pivot_wider(names_from = year, values_from = ci) %>%
  dplyr::arrange(match(age_group, c("18–24","25–34")))
readr::write_csv(age_wide, file.path(tab_dir, "table_age18_34_by_year_wide.csv"))

# 5.4 Year-to-year differences with significance flag
diff_pretty <- diff_both %>%
  dplyr::mutate(sig = dplyr::if_else(p_two < 0.05, "Yes", "No")) %>%
  dplyr::select(denom, year1, year2,
                diff_pp = diff_pp,
                z, p_value = p_two, sig)
readr::write_csv(diff_pretty, file.path(tab_dir, "table_pairwise_diffs.csv"))

# 5.5 Trend tests (logit slope + OR per 4-year step)
trend_pretty <- trend_tbl %>%
  dplyr::mutate(
    OR    = exp(slope_logit),
    OR_l  = exp(slope_logit - 1.96*se),
    OR_u  = exp(slope_logit + 1.96*se),
    OR_ci = sprintf("%.2f (%.2f–%.2f)", OR, OR_l, OR_u)
  ) %>%
  dplyr::select(group,
                slope_logit, se, z,
                p_value = p_two, OR_ci)
readr::write_csv(trend_pretty, file.path(tab_dir, "table_trend_tests.csv"))

# 5.6 (Optional) One Excel workbook if openxlsx is available
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Overall");    openxlsx::writeData(wb, "Overall", overall_wide)
  openxlsx::addWorksheet(wb, "Sex");        openxlsx::writeData(wb, "Sex", sex_wide)
  openxlsx::addWorksheet(wb, "Age 18–34");  openxlsx::writeData(wb, "Age 18–34", age_wide)
  openxlsx::addWorksheet(wb, "Diffs");      openxlsx::writeData(wb, "Diffs", diff_pretty)
  openxlsx::addWorksheet(wb, "Trends");     openxlsx::writeData(wb, "Trends", trend_pretty)
  openxlsx::saveWorkbook(wb, file.path(tab_dir, "pilot_tables.xlsx"), overwrite = TRUE)
}

# ===== QC-1: Alt binge definitions (CDC flag vs derived) =====
# Build alt variables per year
mk_alt <- function(d){
  d %>%
    dplyr::mutate(
      any_from_rfbing = dplyr::case_when(
        !is.na(rfbing5) & as.integer(rfbing5) == 2 ~ 1L,
        !is.na(rfbing5) & as.integer(rfbing5) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      any_from_derived = dplyr::case_when(
        is.na(alcday5)      ~ NA_integer_,
        binge_episodes > 0  ~ 1L,
        binge_episodes <= 0 ~ 0L,
        TRUE ~ NA_integer_
      )
    )
}
df11_alt <- mk_alt(df11); df15_alt <- mk_alt(df15); df19_alt <- mk_alt(df19)

est_ci_var <- function(df, var){
  des <- survey::svydesign(id=~psu, strata=~strata, weights=~wt, data=df, nest=TRUE)
  e   <- survey::svymean(as.formula(paste0("~", var)), des, na.rm=TRUE)
  est <- as.numeric(e); se <- as.numeric(SE(e))
  tibble::tibble(est=est, lcl=pmax(0, est-1.96*se), ucl=pmin(1, est+1.96*se))
}

alt_tbl <- dplyr::bind_rows(
  est_ci_var(df11_alt,"any_from_rfbing")   %>% dplyr::mutate(year=2011, method="CDC flag"),
  est_ci_var(df11_alt,"any_from_derived")  %>% dplyr::mutate(year=2011, method="Derived"),
  est_ci_var(df15_alt,"any_from_rfbing")   %>% dplyr::mutate(year=2015, method="CDC flag"),
  est_ci_var(df15_alt,"any_from_derived")  %>% dplyr::mutate(year=2015, method="Derived"),
  est_ci_var(df19_alt,"any_from_rfbing")   %>% dplyr::mutate(year=2019, method="CDC flag"),
  est_ci_var(df19_alt,"any_from_derived")  %>% dplyr::mutate(year=2019, method="Derived")
) %>%
  dplyr::arrange(year, method)

# Compare the two methods within each year (pp = percentage points)
alt_wide <- alt_tbl %>%
  tidyr::pivot_wider(id_cols=year, names_from=method, values_from=c(est,lcl,ucl)) %>%
  dplyr::mutate(diff_pp = (est_Derived - est_CDC.flag)*100)

readr::write_csv(alt_tbl,  file.path(tab_dir, "qc_alt_binge_methods_prop.csv"))
readr::write_csv(alt_wide, file.path(tab_dir, "qc_alt_binge_methods_wide.csv"))
print(alt_wide, n=Inf)

# ===== QC-2: Lonely PSU sensitivity (adjust vs average) =====
old_lonely <- getOption("survey.lonely.psu")
on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

# Recompute overall CIs under 'average'
options(survey.lonely.psu = "average")
ci_avg <- dplyr::bind_rows(
  est_ci(des11) %>% dplyr::mutate(year=2011),
  est_ci(des15) %>% dplyr::mutate(year=2015),
  est_ci(des19) %>% dplyr::mutate(year=2019)
) %>% dplyr::select(year, est, lcl, ucl) %>% dplyr::mutate(method="average")

# Back to your default 'adjust' to fetch the baseline (already computed above as prev_any_all_CI)
options(survey.lonely.psu = "adjust")
ci_adj <- prev_any_all_CI %>% dplyr::mutate(method="adjust")

lonely_compare <- dplyr::left_join(
  ci_adj %>% dplyr::rename(est_adj=est,lcl_adj=lcl,ucl_adj=ucl) %>% dplyr::select(year, est_adj,lcl_adj,ucl_adj),
  ci_avg %>% dplyr::rename(est_avg=est,lcl_avg=lcl,ucl_avg=ucl) %>% dplyr::select(year, est_avg,lcl_avg,ucl_avg),
  by="year"
) %>%
  dplyr::mutate(
    delta_pp = (est_avg - est_adj)*100,
    width_adj = (ucl_adj - lcl_adj)*100,
    width_avg = (ucl_avg - lcl_avg)*100
  )

readr::write_csv(lonely_compare, file.path(tab_dir, "qc_lonely_psu_adjust_vs_average.csv"))
print(lonely_compare, n=Inf)

# ===== Current drinkers: fast trend models (sex & young ages) =====
# Subset the pooled design to current drinkers (ALCDAY5 != 888)
des_curr <- subset(des_all, alcday5 != 888 & !is.na(alcday5))

## --- Sex-specific trend in a single model ---
fit_sex_curr <- survey::svyglm(any_binge_cdc ~ year_c * sex_f,
                               design = des_curr, family = quasibinomial())

co_sc <- summary(fit_sex_curr)$coefficients
V_sc  <- vcov(fit_sex_curr)

# Slopes per sex (Male = reference)
slope_m_curr <- unname(co_sc["year_c","Estimate"])
se_m_curr    <- sqrt(V_sc["year_c","year_c"])
slope_f_curr <- unname(co_sc["year_c","Estimate"] + co_sc["year_c:sex_fFemale","Estimate"])
se_f_curr    <- sqrt(V_sc["year_c","year_c"] +
                       V_sc["year_c:sex_fFemale","year_c:sex_fFemale"] +
                       2*V_sc["year_c","year_c:sex_fFemale"])

z_m_curr <- slope_m_curr / se_m_curr
z_f_curr <- slope_f_curr / se_f_curr
p_m_curr <- 2*(1 - pnorm(abs(z_m_curr)))
p_f_curr <- 2*(1 - pnorm(abs(z_f_curr)))

## --- Young-adult age trend (18–24 vs 25–34) in a single model ---
des_age_curr <- subset(des_curr, !is.na(age_f))
fit_age_curr <- survey::svyglm(any_binge_cdc ~ year_c * age_f,
                               design = des_age_curr, family = quasibinomial())

co_ac <- summary(fit_age_curr)$coefficients
V_ac  <- vcov(fit_age_curr)

# Slopes per age (18–24 = reference)
slope_18_curr <- unname(co_ac["year_c","Estimate"])
se_18_curr    <- sqrt(V_ac["year_c","year_c"])
slope_25_curr <- unname(co_ac["year_c","Estimate"] + co_ac["year_c:age_f25–34","Estimate"])
se_25_curr    <- sqrt(V_ac["year_c","year_c"] +
                        V_ac["year_c:age_f25–34","year_c:age_f25–34"] +
                        2*V_ac["year_c","year_c:age_f25–34"])

z_18_curr <- slope_18_curr / se_18_curr
z_25_curr <- slope_25_curr / se_25_curr
p_18_curr <- 2*(1 - pnorm(abs(z_18_curr)))
p_25_curr <- 2*(1 - pnorm(abs(z_25_curr)))

# Output: trend tests for current drinkers
trend_curr_tbl <- tibble::tibble(
  group       = c("Sex: Male (current)","Sex: Female (current)",
                  "Age: 18–24 (current)","Age: 25–34 (current)"),
  slope_logit = c(slope_m_curr, slope_f_curr, slope_18_curr, slope_25_curr),
  se          = c(se_m_curr,    se_f_curr,    se_18_curr,     se_25_curr),
  z           = c(z_m_curr,     z_f_curr,     z_18_curr,      z_25_curr),
  p_two       = c(p_m_curr,     p_f_curr,     p_18_curr,      p_25_curr)
)
readr::write_csv(trend_curr_tbl,
                 file.path(tab_dir, "trend_tests_by_group_2011_2019_currentdrinkers.csv"))
print(trend_curr_tbl)

# Predicted means (current drinkers) by year × sex
new_sex_c <- expand.grid(year = c(2011, 2015, 2019), sex_f = c("Male","Female"))
new_sex_c$sex_f  <- factor(new_sex_c$sex_f, levels = c("Male","Female"))
new_sex_c$year_c <- (new_sex_c$year - 2011)/4

pred_sex_curr <- dplyr::bind_cols(
  new_sex_c,
  predict_prob_ci(fit_sex_curr, new_sex_c)
) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x * 100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))

readr::write_csv(pred_sex_curr, file.path(tab_dir, "predicted_means_by_year_sex_current.csv"))


# Predicted means (current drinkers) by year × age
new_age_c <- expand.grid(year = c(2011, 2015, 2019), age_f = c("18–24","25–34"))
new_age_c$age_f  <- factor(new_age_c$age_f, levels = c("18–24","25–34"))
new_age_c$year_c <- (new_age_c$year - 2011)/4

pred_age_curr <- dplyr::bind_cols(
  new_age_c,
  predict_prob_ci(fit_age_curr, new_age_c)
) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x * 100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))

readr::write_csv(pred_age_curr, file.path(tab_dir, "predicted_means_by_year_age_current.csv"))


# ===== Save predictions (current drinkers) =====
readr::write_csv(pred_sex_curr,
                 file.path(tab_dir, "predicted_means_by_year_sex_currentdrinkers.csv"))
readr::write_csv(pred_age_curr,
                 file.path(tab_dir, "predicted_means_by_year_age18_34_currentdrinkers.csv"))

# ===== NEW: Full Age (5-level) + Education (4-level) analyses =====
# (Place after your current predicted means code; does not overwrite earlier objects)

# --- Ensure helper for predictions exists (no-ops if already defined) ---
if (!exists("predict_prob_ci")) {
  predict_prob_ci <- function(fit, newdata) {
    pr <- stats::predict(fit, newdata = newdata, type = "link", se.fit = TRUE)
    fit <- if (is.list(pr) && !is.null(pr$fit)) pr$fit else if (is.list(pr) && !is.null(pr$pred)) pr$pred else pr
    se  <- if (is.list(pr) && !is.null(pr$se.fit)) pr$se.fit else if (is.list(pr) && !is.null(pr$se)) pr$se else NA_real_
    tibble::tibble(
      est = plogis(fit),
      lcl = plogis(fit - 1.96 * se),
      ucl = plogis(fit + 1.96 * se)
    )
  }
}

# --- Factor setup on pooled designs (adds new vars; doesn't touch earlier ones) ---
age5_levels <- c("18–24","25–34","35–49","50–64","65+")
des_all <- stats::update(
  des_all,
  age5_f  = factor(age_group, levels = age5_levels),
  educ4_f = factor(educ4, levels = c("<HS","HS/GED","Some college/AA","College"))
)

# If you don't already have a pooled design for current drinkers, create it once
if (!exists("des_all_curr")) {
  stacked_curr <- subset(des_all, !is.na(alcday5) & alcday5 != 888)
  des_all_curr <- stacked_curr
} else {
  # Make sure factors exist on the current-drinker design too
  des_all_curr <- stats::update(
    des_all_curr,
    age5_f  = factor(age_group, levels = age5_levels),
    educ4_f = factor(educ4, levels = c("<HS","HS/GED","Some college/AA","College"))
  )
}

# --- Design-based CIs by Year × Age (5-level), both denominators ---
age5_yr_all <- dplyr::bind_rows(
  svy_ci_by(dplyr::filter(df11, age_group %in% age5_levels), age_group) %>% dplyr::mutate(year=2011),
  svy_ci_by(dplyr::filter(df15, age_group %in% age5_levels), age_group) %>% dplyr::mutate(year=2015),
  svy_ci_by(dplyr::filter(df19, age_group %in% age5_levels), age_group) %>% dplyr::mutate(year=2019)
) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(age5_yr_all, file.path(tab_dir, "prev_any_binge_alladults_age5_year_CI_prop.csv"))

age5_yr_all_pct <- age5_yr_all %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100))
readr::write_csv(age5_yr_all_pct, file.path(tab_dir, "prev_any_binge_alladults_age5_year_CI_pct.csv"))

age5_yr_curr <- dplyr::bind_rows(
  svy_ci_by_curr(dplyr::filter(df11, age_group %in% age5_levels), age_group) %>% dplyr::mutate(year=2011),
  svy_ci_by_curr(dplyr::filter(df15, age_group %in% age5_levels), age_group) %>% dplyr::mutate(year=2015),
  svy_ci_by_curr(dplyr::filter(df19, age_group %in% age5_levels), age_group) %>% dplyr::mutate(year=2019)
) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(age5_yr_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_prop.csv"))

age5_yr_curr_pct <- age5_yr_curr %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100))
readr::write_csv(age5_yr_curr_pct, file.path(tab_dir, "prev_any_binge_currentdrinkers_age5_year_CI_pct.csv"))

# --- Design-based CIs by Year × Education (4-level), both denominators ---
educ_yr_all <- dplyr::bind_rows(
  svy_ci_by(dplyr::filter(df11, !is.na(educ4)), educ4) %>% dplyr::mutate(year=2011),
  svy_ci_by(dplyr::filter(df15, !is.na(educ4)), educ4) %>% dplyr::mutate(year=2015),
  svy_ci_by(dplyr::filter(df19, !is.na(educ4)), educ4) %>% dplyr::mutate(year=2019)
) %>%
  dplyr::transmute(educ4, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::arrange(educ4, year)

readr::write_csv(educ_yr_all, file.path(tab_dir, "prev_any_binge_alladults_educ4_year_CI_prop.csv"))

educ_yr_all_pct <- educ_yr_all %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100))
readr::write_csv(educ_yr_all_pct, file.path(tab_dir, "prev_any_binge_alladults_educ4_year_CI_pct.csv"))

educ_yr_curr <- dplyr::bind_rows(
  svy_ci_by_curr(dplyr::filter(df11, !is.na(educ4)), educ4) %>% dplyr::mutate(year=2011),
  svy_ci_by_curr(dplyr::filter(df15, !is.na(educ4)), educ4) %>% dplyr::mutate(year=2015),
  svy_ci_by_curr(dplyr::filter(df19, !is.na(educ4)), educ4) %>% dplyr::mutate(year=2019)
) %>%
  dplyr::transmute(educ4, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::arrange(educ4, year)

readr::write_csv(educ_yr_curr, file.path(tab_dir, "prev_any_binge_currentdrinkers_educ4_year_CI_prop.csv"))

educ_yr_curr_pct <- educ_yr_curr %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100))
readr::write_csv(educ_yr_curr_pct, file.path(tab_dir, "prev_any_binge_currentdrinkers_educ4_year_CI_pct.csv"))

# --- Trend tests via single interaction models, with per-level slopes ---
extract_slopes <- function(fit, base_name, inter_prefix, levels, ref) {
  cf <- summary(fit)$coefficients
  V  <- vcov(fit)
  out <- lapply(levels, function(lev){
    if (lev == ref) {
      est <- cf[base_name,"Estimate"]
      var <- V[base_name, base_name]
    } else {
      iname <- paste0(inter_prefix, lev)
      est <- cf[base_name,"Estimate"] + cf[iname,"Estimate"]
      var <- V[base_name,base_name] + V[iname,iname] + 2*V[base_name,iname]
    }
    se <- sqrt(var); z <- est/se; p <- 2*(1 - pnorm(abs(z)))
    tibble::tibble(level = lev, slope_logit = unname(est), se = unname(se), z = unname(z), p_two = unname(p))
  })
  dplyr::bind_rows(out)
}

# Age (5-level) trends — All adults
fit_age5_all <- survey::svyglm(any_binge_cdc ~ year_c * age5_f, design = des_all, family = quasibinomial())
age5_trend_all <- extract_slopes(fit_age5_all, "year_c", "year_c:age5_f", age5_levels, ref = "18–24") %>%
  dplyr::mutate(group = paste0("Age: ", level)) %>%
  dplyr::select(group, slope_logit, se, z, p_two)
readr::write_csv(age5_trend_all, file.path(tab_dir, "trend_tests_age5_alladults.csv"))

# Age (5-level) trends — Current drinkers
fit_age5_curr <- survey::svyglm(any_binge_cdc ~ year_c * age5_f, design = des_all_curr, family = quasibinomial())
age5_trend_curr <- extract_slopes(fit_age5_curr, "year_c", "year_c:age5_f", age5_levels, ref = "18–24") %>%
  dplyr::mutate(group = paste0("Age (current): ", level)) %>%
  dplyr::select(group, slope_logit, se, z, p_two)
readr::write_csv(age5_trend_curr, file.path(tab_dir, "trend_tests_age5_currentdrinkers.csv"))

# Education trends — All adults
educ_levels <- c("<HS","HS/GED","Some college/AA","College")
fit_educ_all <- survey::svyglm(any_binge_cdc ~ year_c * educ4_f, design = des_all, family = quasibinomial())
educ_trend_all <- extract_slopes(fit_educ_all, "year_c", "year_c:educ4_f", educ_levels, ref = "<HS") %>%
  dplyr::mutate(group = paste0("Educ: ", level)) %>%
  dplyr::select(group, slope_logit, se, z, p_two)
readr::write_csv(educ_trend_all, file.path(tab_dir, "trend_tests_educ4_alladults.csv"))

# Education trends — Current drinkers
fit_educ_curr <- survey::svyglm(any_binge_cdc ~ year_c * educ4_f, design = des_all_curr, family = quasibinomial())
educ_trend_curr <- extract_slopes(fit_educ_curr, "year_c", "year_c:educ4_f", educ_levels, ref = "<HS") %>%
  dplyr::mutate(group = paste0("Educ (current): ", level)) %>%
  dplyr::select(group, slope_logit, se, z, p_two)
readr::write_csv(educ_trend_curr, file.path(tab_dir, "trend_tests_educ4_currentdrinkers.csv"))

# --- Predicted margins by Year × Age5 / Education (All adults and Current drinkers) ---
# Age5 predicted means (All adults)
new_age5 <- expand.grid(year = c(2011,2015,2019), age5_f = age5_levels)
new_age5$year_c <- (new_age5$year - 2011)/4
pred_age5_all <- dplyr::bind_cols(new_age5, predict_prob_ci(fit_age5_all, new_age5)) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(pred_age5_all, file.path(tab_dir, "predicted_means_by_year_age5_alladults.csv"))

# Age5 predicted means (Current drinkers)
pred_age5_curr <- dplyr::bind_cols(new_age5, predict_prob_ci(fit_age5_curr, new_age5)) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(pred_age5_curr, file.path(tab_dir, "predicted_means_by_year_age5_currentdrinkers.csv"))

# Education predicted means (All adults)
new_educ <- expand.grid(year = c(2011,2015,2019), educ4_f = educ_levels)
new_educ$year_c <- (new_educ$year - 2011)/4
pred_educ_all <- dplyr::bind_cols(new_educ, predict_prob_ci(fit_educ_all, new_educ)) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(pred_educ_all, file.path(tab_dir, "predicted_means_by_year_educ4_alladults.csv"))

# Education predicted means (Current drinkers)
pred_educ_curr <- dplyr::bind_cols(new_educ, predict_prob_ci(fit_educ_curr, new_educ)) %>%
  dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl))
readr::write_csv(pred_educ_curr, file.path(tab_dir, "predicted_means_by_year_educ4_currentdrinkers.csv"))

# --- Pairwise diffs within age5 & education (does not overwrite earlier 'age_diffs' etc.) ---
age5_diffs_all <- dplyr::bind_rows(lapply(age5_levels, function(g)
  pairwise_diffs_within(age5_yr_all, age_group, g)))
readr::write_csv(age5_diffs_all, file.path(tab_dir, "pairwise_diffs_age5_alladults.csv"))

age5_diffs_curr <- dplyr::bind_rows(lapply(age5_levels, function(g)
  pairwise_diffs_within(age5_yr_curr, age_group, g)))
readr::write_csv(age5_diffs_curr, file.path(tab_dir, "pairwise_diffs_age5_currentdrinkers.csv"))

educ_diffs_all <- dplyr::bind_rows(lapply(educ_levels, function(g)
  pairwise_diffs_within(educ_yr_all, educ4, g)))
readr::write_csv(educ_diffs_all, file.path(tab_dir, "pairwise_diffs_educ4_alladults.csv"))

educ_diffs_curr <- dplyr::bind_rows(lapply(educ_levels, function(g)
  pairwise_diffs_within(educ_yr_curr, educ4, g)))
readr::write_csv(educ_diffs_curr, file.path(tab_dir, "pairwise_diffs_educ4_currentdrinkers.csv"))


# ===== Plots: model-based predictions (current drinkers) =====
p_sex_curr_pred <- ggplot2::ggplot(
  pred_sex_curr,
  ggplot2::aes(x = year, y = est, color = sex_f, fill = sex_f, group = sex_f)
) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::labs(
    x = "Year", y = "Any binge (%)",
    title = "Current Drinkers: Model-based Any-Binge by Sex",
    color = "Sex"
  ) +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(fig_dir, "pred_any_binge_currentdrinkers_by_sex_model.png"),
  plot = p_sex_curr_pred, width = 8, height = 5, dpi = 300
)

p_age_curr_pred <- ggplot2::ggplot(
  pred_age_curr,
  ggplot2::aes(x = year, y = est, color = age_f, fill = age_f, group = age_f)
) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.15, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::labs(
    x = "Year", y = "Any binge (%)",
    title = "Current Drinkers: Model-based Any-Binge (18–24 vs 25–34)",
    color = "Age group"
  ) +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(fig_dir, "pred_any_binge_currentdrinkers_by_age_model.png"),
  plot = p_age_curr_pred, width = 8, height = 5, dpi = 300
)

# ===== Step 2 — Consolidated pilot results tables =====
# 2A) Prevalence (design CIs + model-based preds) in one long CSV

# -- Ensure current-drinker overall CIs in percent
prev_any_current_CI_pct <- prev_any_current_CI %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100)

# -- Design-based CIs (percent)
prev_design_long <- dplyr::bind_rows(
  # Overall
  prev_any_all_CI   %>% dplyr::mutate(denom="All adults",       subgroup="Overall"),
  prev_any_current_CI %>% dplyr::mutate(denom="Current drinkers", subgroup="Overall")
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100,
                source = "Design CI") %>%
  dplyr::select(denom, subgroup, year, est, lcl, ucl, source)

# --- (RE)BUILD current-drinkers CIs needed for consolidation ---

# Sex × Year (current drinkers)
sex11_c <- svy_ci_by_curr(df11, sex) %>% dplyr::mutate(year = 2011)
sex15_c <- svy_ci_by_curr(df15, sex) %>% dplyr::mutate(year = 2015)
sex19_c <- svy_ci_by_curr(df19, sex) %>% dplyr::mutate(year = 2019)

prev_sex_year_curr <- dplyr::bind_rows(sex11_c, sex15_c, sex19_c) %>%
  dplyr::mutate(sex = dplyr::recode(as.integer(sex), `1`="Male", `2`="Female")) %>%
  dplyr::transmute(sex, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  dplyr::arrange(sex, year)

# Current drinkers — Sex pairwise diffs
if (!exists("prev_sex_year_curr")) {
  sex11_c <- svy_ci_by_curr(df11, sex) %>% dplyr::mutate(year = 2011)
  sex15_c <- svy_ci_by_curr(df15, sex) %>% dplyr::mutate(year = 2015)
  sex19_c <- svy_ci_by_curr(df19, sex) %>% dplyr::mutate(year = 2019)
  
  prev_sex_year_curr <- dplyr::bind_rows(sex11_c, sex15_c, sex19_c) %>%
    dplyr::mutate(sex = dplyr::recode(as.integer(sex), `1`="Male", `2`="Female")) %>%
    dplyr::transmute(sex, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
    dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
    dplyr::arrange(sex, year)
}

sex_diffs_curr <- dplyr::bind_rows(
  pairwise_diffs_within(prev_sex_year_curr, sex, "Male"),
  pairwise_diffs_within(prev_sex_year_curr, sex, "Female")
)


# Young-adult Age × Year (current drinkers)
age_keep <- c("18–24","25–34")
age11_c <- df11 %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2011)
age15_c <- df15 %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2015)
age19_c <- df19 %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2019)

prev_age_year_curr <- dplyr::bind_rows(age11_c, age15_c, age19_c) %>%
  dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
  dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
  dplyr::arrange(age_group, year)


# Sex (design)
prev_design_sex <- dplyr::bind_rows(
  prev_sex_year_CI       %>% dplyr::mutate(denom="All adults",       subgroup = paste0("Sex: ", sex)),
  prev_sex_year_curr     %>% dplyr::mutate(denom="Current drinkers", subgroup = paste0("Sex: ", sex))
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100,
                source = "Design CI") %>%
  dplyr::select(denom, subgroup, year, est, lcl, ucl, source)

# Age 18–34 (design)
prev_design_age <- dplyr::bind_rows(
  prev_age_year_CI       %>% dplyr::mutate(denom="All adults",       subgroup = paste0("Age: ", age_group)),
  prev_age_year_curr     %>% dplyr::mutate(denom="Current drinkers", subgroup = paste0("Age: ", age_group))
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100,
                source = "Design CI") %>%
  dplyr::select(denom, subgroup, year, est, lcl, ucl, source)

# -- Model-based predictions (already in percent)
pred_model_long <- dplyr::bind_rows(
  pred_sex       %>% dplyr::mutate(denom="All adults",       subgroup = paste0("Sex: ", sex_f)) %>%
    dplyr::select(denom, subgroup, year, est, lcl, ucl) %>% dplyr::mutate(source="Model Pred"),
  pred_age       %>% dplyr::mutate(denom="All adults",       subgroup = paste0("Age: ", age_f)) %>%
    dplyr::select(denom, subgroup, year, est, lcl, ucl) %>% dplyr::mutate(source="Model Pred"),
  pred_sex_curr  %>% dplyr::mutate(denom="Current drinkers", subgroup = paste0("Sex: ", sex_f)) %>%
    dplyr::select(denom, subgroup, year, est, lcl, ucl) %>% dplyr::mutate(source="Model Pred"),
  pred_age_curr  %>% dplyr::mutate(denom="Current drinkers", subgroup = paste0("Age: ", age_f)) %>%
    dplyr::select(denom, subgroup, year, est, lcl, ucl) %>% dplyr::mutate(source="Model Pred")
)

pilot_prev <- dplyr::bind_rows(
  prev_design_long, prev_design_sex, prev_design_age, pred_model_long
) %>%
  dplyr::arrange(denom, subgroup, year)

readr::write_csv(pilot_prev, file.path(tab_dir, "pilot_prevalence_consolidated.csv"))

# 2B) Trend tests (all adults + current drinkers)
pilot_trends <- dplyr::bind_rows(
  trend_tbl      %>% dplyr::mutate(denom = "All adults"),
  trend_curr_tbl %>% dplyr::mutate(denom = "Current drinkers")
) %>%
  dplyr::select(denom, group, slope_logit, se, z, p_two)

readr::write_csv(pilot_trends, file.path(tab_dir, "pilot_trends_consolidated.csv"))

# All adults — Age 18–34 pairwise diffs
age_diffs <- dplyr::bind_rows(
  pairwise_diffs_within(prev_age_year_CI, age_group, "18–24"),
  pairwise_diffs_within(prev_age_year_CI, age_group, "25–34")
)

# --- Ensure age diffs exist for All adults and Current drinkers ---

# Age groups used
if (!exists("age_keep")) age_keep <- c("18–24","25–34")

# All adults: build prev_age_year_CI if missing, then diffs
if (!exists("prev_age_year_CI")) {
  svy_ci_by_age <- function(df) { df %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by(age_group) }
  prev_age_year_CI <- dplyr::bind_rows(
    svy_ci_by_age(df11) %>% dplyr::mutate(year = 2011),
    svy_ci_by_age(df15) %>% dplyr::mutate(year = 2015),
    svy_ci_by_age(df19) %>% dplyr::mutate(year = 2019)
  ) %>%
    dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
    dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
    dplyr::arrange(age_group, year)
}

if (!exists("age_diffs")) {
  age_diffs <- dplyr::bind_rows(
    pairwise_diffs_within(prev_age_year_CI, age_group, "18–24"),
    pairwise_diffs_within(prev_age_year_CI, age_group, "25–34")
  )
}

# Current drinkers: build prev_age_year_curr if missing, then diffs
if (!exists("prev_age_year_curr")) {
  age11_c <- df11 %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2011)
  age15_c <- df15 %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2015)
  age19_c <- df19 %>% dplyr::filter(age_group %in% age_keep) %>% svy_ci_by_curr(age_group) %>% dplyr::mutate(year = 2019)
  
  prev_age_year_curr <- dplyr::bind_rows(age11_c, age15_c, age19_c) %>%
    dplyr::transmute(age_group, year, est = prop, lcl = prop_low, ucl = prop_upp) %>%
    dplyr::mutate(lcl = clamp01(lcl), ucl = clamp01(ucl)) %>%
    dplyr::arrange(age_group, year)
}

age_diffs_curr <- dplyr::bind_rows(
  pairwise_diffs_within(prev_age_year_curr, age_group, "18–24"),
  pairwise_diffs_within(prev_age_year_curr, age_group, "25–34")
)



# 2C) Pairwise diffs consolidated (overall, sex, age; both denoms)
pilot_diffs <- dplyr::bind_rows(
  diff_both           %>% dplyr::mutate(scope = "Denominator overall"),
  sex_diffs           %>% dplyr::mutate(scope = "All adults — Sex"),
  age_diffs           %>% dplyr::mutate(scope = "All adults — Age 18–34"),
  sex_diffs_curr      %>% dplyr::mutate(scope = "Current drinkers — Sex"),
  age_diffs_curr      %>% dplyr::mutate(scope = "Current drinkers — Age 18–34")
)


readr::write_csv(pilot_diffs, file.path(tab_dir, "pilot_pairwise_diffs_consolidated.csv"))

pilot_diffs <- dplyr::bind_rows(
  diff_both      %>% dplyr::mutate(scope = "Denominator overall"),
  sex_diffs      %>% dplyr::mutate(scope = "All adults — Sex"),
  age_diffs      %>% dplyr::mutate(scope = "All adults — Age 18–34"),
  sex_diffs_curr %>% dplyr::mutate(scope = "Current drinkers — Sex"),
  age_diffs_curr %>% dplyr::mutate(scope = "Current drinkers — Age 18–34")
)

readr::write_csv(pilot_diffs, file.path(tab_dir, "pilot_pairwise_diffs_all.csv"))
print(pilot_diffs, n = Inf)

# --- 1) Build "design CI" tables (both denoms) ---

# Sex
prev_design_sex <- dplyr::bind_rows(
  prev_sex_year_CI   %>% dplyr::mutate(denom = "All adults",       subgroup = paste0("Sex: ", sex)),
  prev_sex_year_curr %>% dplyr::mutate(denom = "Current drinkers", subgroup = paste0("Sex: ", sex))
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100,
                source = "Design CI", panel = "Sex") %>%
  dplyr::select(denom, panel, subgroup, year, est, lcl, ucl, source)

# Age 18–34
prev_design_age <- dplyr::bind_rows(
  prev_age_year_CI   %>% dplyr::mutate(denom = "All adults",       subgroup = paste0("Age: ", age_group)),
  prev_age_year_curr %>% dplyr::mutate(denom = "Current drinkers", subgroup = paste0("Age: ", age_group))
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100,
                source = "Design CI", panel = "Age 18–34") %>%
  dplyr::select(denom, panel, subgroup, year, est, lcl, ucl, source)

# --- 2) Ensure model predictions exist; if not, compute quickly ---

# Helper already defined earlier: predict_prob_ci()
# All-adults model preds
if (!exists("pred_sex")) {
  new_sex <- expand.grid(year = c(2011,2015,2019), sex_f = c("Male","Female"))
  new_sex$sex_f  <- factor(new_sex$sex_f, levels = c("Male","Female"))
  new_sex$year_c <- (new_sex$year - 2011)/4
  pred_sex <- dplyr::bind_cols(new_sex, predict_prob_ci(fit_sex, new_sex))
}
if (!exists("pred_age")) {
  new_age <- expand.grid(year = c(2011,2015,2019), age_f = c("18–24","25–34"))
  new_age$age_f  <- factor(new_age$age_f, levels = c("18–24","25–34"))
  new_age$year_c <- (new_age$year - 2011)/4
  pred_age <- dplyr::bind_cols(new_age, predict_prob_ci(fit_age, new_age))
}

# Current-drinkers model preds
if (!exists("pred_sex_curr")) {
  new_sex_c <- expand.grid(year = c(2011,2015,2019), sex_f = c("Male","Female"))
  new_sex_c$sex_f  <- factor(new_sex_c$sex_f, levels = c("Male","Female"))
  new_sex_c$year_c <- (new_sex_c$year - 2011)/4
  pred_sex_curr <- dplyr::bind_cols(new_sex_c, predict_prob_ci(fit_sex_curr, new_sex_c))
}
if (!exists("pred_age_curr")) {
  new_age_c <- expand.grid(year = c(2011,2015,2019), age_f = c("18–24","25–34"))
  new_age_c$age_f  <- factor(new_age_c$age_f, levels = c("18–24","25–34"))
  new_age_c$year_c <- (new_age_c$year - 2011)/4
  pred_age_curr <- dplyr::bind_cols(new_age_c, predict_prob_ci(fit_age_curr, new_age_c))
}

# --- 3) Build "model pred" tables (both denoms) ---

# Sex
pred_model_sex <- dplyr::bind_rows(
  pred_sex       %>% dplyr::transmute(denom="All adults",       panel="Sex",
                                      subgroup=paste0("Sex: ", as.character(sex_f)),
                                      year, est=est*100, lcl=lcl*100, ucl=ucl*100, source="Model pred"),
  pred_sex_curr  %>% dplyr::transmute(denom="Current drinkers", panel="Sex",
                                      subgroup=paste0("Sex: ", as.character(sex_f)),
                                      year, est=est*100, lcl=lcl*100, ucl=ucl*100, source="Model pred")
)

# Age 18–34
pred_model_age <- dplyr::bind_rows(
  pred_age       %>% dplyr::transmute(denom="All adults",       panel="Age 18–34",
                                      subgroup=paste0("Age: ", as.character(age_f)),
                                      year, est=est*100, lcl=lcl*100, ucl=ucl*100, source="Model pred"),
  pred_age_curr  %>% dplyr::transmute(denom="Current drinkers", panel="Age 18–34",
                                      subgroup=paste0("Age: ", as.character(age_f)),
                                      year, est=est*100, lcl=lcl*100, ucl=ucl*100, source="Model pred")
)

# --- 4) Combine & export tables ---

pilot_prev_design <- dplyr::bind_rows(prev_design_sex, prev_design_age)
pilot_prev_model  <- dplyr::bind_rows(pred_model_sex, pred_model_age)
pilot_prev_all    <- dplyr::bind_rows(pilot_prev_design, pilot_prev_model)

readr::write_csv(pilot_prev_design, file.path(tab_dir, "pilot_prev_design_ci.csv"))
readr::write_csv(pilot_prev_model,  file.path(tab_dir, "pilot_prev_model_preds.csv"))
readr::write_csv(pilot_prev_all,    file.path(tab_dir, "pilot_prev_design_vs_model_all.csv"))

# Also save the consolidated diffs you built earlier
readr::write_csv(pilot_diffs, file.path(tab_dir, "pilot_pairwise_diffs_all.csv"))

# --- 5) Quick comparison plot (2×2 grid: Sex vs Age, rows = denom) ---

p_grid <- ggplot2::ggplot(pilot_prev_all,
                          ggplot2::aes(x = year, y = est/100, color = subgroup, linetype = source)) +
  ggplot2::geom_ribbon(
    data = dplyr::filter(pilot_prev_all, source == "Design CI"),
    ggplot2::aes(ymin = lcl/100, ymax = ucl/100, fill = subgroup),
    alpha = 0.12, linewidth = 0, show.legend = FALSE
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_grid(panel ~ denom) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, 60)) +
  ggplot2::labs(
    x = "Year", y = "Any binge (%)",
    title = "Design CIs vs Model Trends: Sex and Young Adults",
    linetype = "Estimate", color = "Subgroup"
  ) +
  ggplot2::theme_minimal(base_size = 12)



print(p_grid)
ggplot2::ggsave(file.path(fig_dir, "pilot_design_vs_model_grid.png"),
                p_grid, width = 10, height = 6.5, dpi = 300)

# ===== 3) Overlay: Design CI vs Model predictions (Sex & Age, both denominators) =====

# --- Sex: design tables (already computed earlier) ---
prev_design_sex <- dplyr::bind_rows(
  prev_sex_year_CI       %>% dplyr::mutate(denom = "All adults",       subgroup = paste0("Sex: ", sex)),
  prev_sex_year_curr     %>% dplyr::mutate(denom = "Current drinkers", subgroup = paste0("Sex: ", sex))
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100, source = "Design CI") %>%
  dplyr::select(denom, subgroup, year, est, lcl, ucl, source)

# --- Sex: model predictions tables (from svyglm) ---
pred_model_sex <- dplyr::bind_rows(
  pred_sex       %>% dplyr::transmute(denom = "All adults",       subgroup = paste0("Sex: ", sex_f), year, est, lcl, ucl, source = "Model (svyglm)"),
  pred_sex_curr  %>% dplyr::transmute(denom = "Current drinkers", subgroup = paste0("Sex: ", sex_f), year, est, lcl, ucl, source = "Model (svyglm)")
)

sex_overlay <- dplyr::bind_rows(prev_design_sex, pred_model_sex) %>%
  dplyr::arrange(denom, subgroup, year)
readr::write_csv(sex_overlay, file.path(tab_dir, "overlay_design_vs_model_sex.csv"))

# --- Plot: Sex overlays ---
p_overlay_sex <- ggplot2::ggplot(sex_overlay, ggplot2::aes(x = year, y = est, color = source, fill = source, group = source)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.12, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_grid(denom ~ subgroup) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::labs(x = "Year", y = "Any binge (%)", title = "Design CIs vs Model Predictions — Sex") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(fig_dir, "overlay_design_vs_model_sex.png"), p_overlay_sex, width = 10, height = 6, dpi = 300)

# --- Age (18–24, 25–34): design tables ---
prev_design_age <- dplyr::bind_rows(
  prev_age_year_CI       %>% dplyr::mutate(denom = "All adults",       subgroup = paste0("Age: ", age_group)),
  prev_age_year_curr     %>% dplyr::mutate(denom = "Current drinkers", subgroup = paste0("Age: ", age_group))
) %>%
  dplyr::mutate(est = est*100, lcl = lcl*100, ucl = ucl*100, source = "Design CI") %>%
  dplyr::select(denom, subgroup, year, est, lcl, ucl, source)

# --- Age: model predictions tables (from svyglm) ---
pred_model_age <- dplyr::bind_rows(
  pred_age       %>% dplyr::transmute(denom = "All adults",       subgroup = paste0("Age: ", age_f), year, est, lcl, ucl, source = "Model (svyglm)"),
  pred_age_curr  %>% dplyr::transmute(denom = "Current drinkers", subgroup = paste0("Age: ", age_f), year, est, lcl, ucl, source = "Model (svyglm)")
)

age_overlay <- dplyr::bind_rows(prev_design_age, pred_model_age) %>%
  dplyr::arrange(denom, subgroup, year)
readr::write_csv(age_overlay, file.path(tab_dir, "overlay_design_vs_model_age18_34.csv"))

# --- Plot: Age overlays ---
p_overlay_age <- ggplot2::ggplot(age_overlay, ggplot2::aes(x = year, y = est, color = source, fill = source, group = source)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.12, linewidth = 0) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_grid(denom ~ subgroup) +
  ggplot2::scale_x_continuous(breaks = c(2011, 2015, 2019)) +
  ggplot2::labs(x = "Year", y = "Any binge (%)", title = "Design CIs vs Model Predictions — Young Adults") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(fig_dir, "overlay_design_vs_model_age18_34.png"), p_overlay_age, width = 10, height = 6, dpi = 300)



