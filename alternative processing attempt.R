  # (1) All adults, by year — FAST direct standardization (no svystandardize object)
  # Requires:
  #   - des_all : pooled svydesign (NOT standardized)
  #   - age7    : 7-band age factor in des_all$variables
  #   - w_2000_age7 : named numeric weights that sum to 1, names match levels(age7)
  
  # Safety: align weights to the data’s factor levels
  w7 <- w_2000_age7[levels(des_all$variables$age7)]
  stopifnot(all(!is.na(w7)), abs(sum(w7) - 1) < 1e-8)
  
  # 1) Age-specific prevalence by year (this is the only svyby call)
  age_y <- survey::svyby(
    ~ any_binge_cdc,
    ~ year + age7,
    subset(des_all, !is.na(age7)),
    survey::svymean,
    na.rm = TRUE,
    vartype = c("se")
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      year = as.integer(year),
      age7 = as.character(age7),
      w = w7[age7]
    )
  
  # 2) Direct standardization: sum(w * p_age) within year
  prev_all_prop_ageadj <- age_y %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      est = sum(w * any_binge_cdc),
      # Approx SE: assumes age strata independent (standard DS approximation)
      se  = sqrt(sum((w^2) * (se^2))),
      lcl = pmax(0, est - stats::qnorm(0.975) * se),
      ucl = pmin(1, est + stats::qnorm(0.975) * se),
      .groups = "drop"
    ) %>%
    dplyr::arrange(year) %>%
    dplyr::select(year, est, lcl, ucl)
  
  # --- write outputs (same as your originals) ---
  readr::write_csv(
    prev_all_prop_ageadj,
    file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_prop.csv")
  )
  
  readr::write_csv(
    prev_all_prop_ageadj %>%
      dplyr::mutate(
        dplyr::across(c(est, lcl, ucl), ~ .x * 100),
        ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
      ),
    file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_pct.csv")
  )

  stopifnot(all(age_y$age7 %in% names(w7)))

  
  # (B) Age-adjusted overall by year, by age-band 
  prev_by_age7 <- age_y %>%
    dplyr::mutate(
      year = as.integer(year),
      age7 = as.character(age7),
      lcl  = pmax(0, any_binge_cdc - stats::qnorm(0.975) * se),
      ucl  = pmin(1, any_binge_cdc + stats::qnorm(0.975) * se)
    ) %>%
    dplyr::select(year, age7, est = any_binge_cdc, lcl, ucl, se, w) %>%
    dplyr::arrange(age7, year)
  
  readr::write_csv(prev_by_age7,
                   file.path(tab_dir, "prevalence_by_age_band_prop.csv"))

  readr::write_csv(prev_by_age7,
                   file.path(tab_dir, "prevalence_by_age_band_pct.csv"))  

  # (B) By race/ethnicity (all adults) — FAST direct standardization (age7)
  # Requires:
  #   des_all : pooled svydesign (NOT standardized)
  #   age7    : 7-band factor in des_all$variables
  #   race6_nat in des_all$variables
  #   w_2000_age7 : named numeric weights that sum to 1 (names match levels(age7))
  
  # 0) Align weights to age7 levels
  w7 <- w_2000_age7[levels(des_all$variables$age7)]
  stopifnot(all(!is.na(w7)), abs(sum(w7) - 1) < 1e-8)
  
  # 1) Age-specific prevalence by year x race (only svyby call)
  age_yr <- survey::svyby(
    ~ any_binge_cdc,
    ~ year + race6_nat + age7,
    subset(des_all, !is.na(age7) & !is.na(race6_nat)),
    survey::svymean,
    na.rm = TRUE,
    vartype = "se"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      year      = as.integer(year),
      race6_nat = as.character(race6_nat),
      age7      = as.character(age7),
      est_age   = any_binge_cdc,   # this IS the age-specific prevalence
      se_age    = se,
      w         = w7[age7]
    )
  
  # 2) Direct standardization within each year x race
  prev_race_all_ageadj <- age_yr %>%
    dplyr::group_by(year, race6_nat) %>%
    dplyr::summarise(
      est = sum(w * est_age),
      se  = sqrt(sum((w^2) * (se_age^2))),
      lcl = pmax(0, est - stats::qnorm(0.975) * se),
      ucl = pmin(1, est + stats::qnorm(0.975) * se),
      .groups = "drop"
    ) %>%
    dplyr::arrange(race6_nat, year)
  
  # 3) Write outputs (prop + percent w/ CI string)
  readr::write_csv(
    prev_race_all_ageadj,
    file.path(tab_dir, "main_prev_any_binge_alladults_BYRACE_AGEADJ2000_CI_prop.csv")
  )
  
  readr::write_csv(
    prev_race_all_ageadj %>%
      dplyr::mutate(
        dplyr::across(c(est, lcl, ucl), ~ .x * 100),
        ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
      ),
    file.path(tab_dir, "main_prev_any_binge_alladults_BYRACE_AGEADJ2000_CI_pct.csv")
  )
  
    