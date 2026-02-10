# ===== 0) Project paths (portable) =====

PROJ_DIR <- tryCatch(
  normalizePath("C:/Users/Anigma PC/OneDrive - Tulane University/Desktop/Binge Drinking Systematic Review/binge-drinking-pilot",
                winslash = "/", mustWork = TRUE),
  error = function(e) normalizePath(getwd(), winslash = "/")
)


YEAR_PATHS <- c(
  `2011` = file.path(PROJ_DIR, "data/raw/2011/LLCP2011.XPT"),
  `2012` = file.path(PROJ_DIR, "data/raw/2012/LLCP2012.XPT"),
  `2013` = file.path(PROJ_DIR, "data/raw/2013/LLCP2013.XPT"),
  `2014` = file.path(PROJ_DIR, "data/raw/2014/LLCP2014.XPT_"),
  `2015` = file.path(PROJ_DIR, "data/raw/2015/LLCP2015.XPT_"),
  `2016` = file.path(PROJ_DIR, "data/raw/2016/LLCP2016.XPT_"),
  `2017` = file.path(PROJ_DIR, "data/raw/2017/LLCP2017.XPT_"),
  `2018` = file.path(PROJ_DIR, "data/raw/2018/LLCP2018.XPT_"),
  `2019` = file.path(PROJ_DIR, "data/raw/2019/LLCP2019.XPT_"),
  `2020` = file.path(PROJ_DIR, "data/raw/2020/LLCP2020.XPT_"),
  `2021` = file.path(PROJ_DIR, "data/raw/2021/LLCP2021.XPT_"),
  `2022` = file.path(PROJ_DIR, "data/raw/2022/LLCP2022.XPT_"),
  `2023` = file.path(PROJ_DIR, "data/raw/2023/LLCP2023.XPT_"),
  `2024` = file.path(PROJ_DIR, "data/raw/2024/LLCP2024.XPT_"))
  
# ===== Dirs =====
dirs <- file.path(PROJ_DIR, c("outputs/main/tables","outputs/main/figures","docs","data/cache"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
tab_dir <- file.path(PROJ_DIR, "outputs/main/tables")
fig_dir <- file.path(PROJ_DIR, "outputs/main/figures")

# ===== Packages / parallel =====


  library(tidyverse); library(janitor); library(haven)
  library(survey);    library(srvyr);   library(readr);  library(scales); library(purrr)

options(survey.lonely.psu = "adjust")
writeLines(capture.output(sessionInfo()), file.path(PROJ_DIR, "docs/sessionInfo_main_run.txt"))

# ===== Helpers (REPLACES older versions) =====

# robust resolver
find_var <- function(df, candidates){
  nms <- tolower(names(df))
  for (cand in tolower(candidates)) {
    hit <- which(nms == cand)
    if (length(hit)) return(names(df)[hit[1]])
  }
  nms2 <- sub("^_", "", nms)
  for (cand in tolower(candidates)) {
    hit <- which(nms2 == sub("^_", "", cand))
    if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}


# Convert ALCDAY5/4 coded values to ~30-day drinking days
alcday5_to_days30 <- function(x){
  xi <- suppressWarnings(as.integer(as.character(x)))
  out <- ifelse(is.na(xi), NA_real_,
                ifelse(xi == 888L, 0,
                       ifelse(xi >= 101L & xi <= 107L, (xi - 100) * 4.3,       # days/week * ~4.3
                              ifelse(xi >= 201L & xi <= 231L, (xi - 200), NA_real_)))) # days/month
  out
}



# ---- Step 12: Formatting helpers ----
fmt_pct1 <- function(x) sprintf("%.1f", x*100)
fmt_ci1  <- function(p, l, u) sprintf("%.1f (%.1f–%.1f)", p*100, l*100, u*100)
add_universe <- function(df, label) dplyr::mutate(df, universe = label, .before = 1)


# recoder (keep as-is from pilot)
recode_analysis <- function(d){
  na_77_88_99 <- function(x) ifelse(x %in% c(77, 88, 99, 777, 888, 999), NA, x)
  as_int   <- function(x) suppressWarnings(as.integer(as.character(x)))
  
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
      any_binge_cdc = dplyr::case_when(
        !is.na(rfbing5) & as.integer(rfbing5) == 2 ~ 1L,
        !is.na(rfbing5) & as.integer(rfbing5) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      any_binge_cdc = dplyr::if_else(
        is.na(rfbing5),  # <-- ONLY fallback if the CDC flag isn't present in that year/file
        dplyr::case_when(
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
      max_drinks = na_77_88_99(max_drinks_raw),
      averdrinks = na_77_88_99(averdrnk_raw),
      sex = case_when(sex_raw %in% c(1,2) ~ as.integer(sex_raw), TRUE ~ NA_integer_),
      
      # ---------- AGE (10-bin primary, 5-bin fallback) ----------
      # _AGEG5YR codes: 1=18–24, 2=25–29, 3=30–34, 4=35–39, 5=40–44,
      #                 6=45–49, 7=50–54, 8=55–59, 9=60–64, 10=65–69, 11=70–74, 12=75–79, 13=80+
      age5 = as_int(age5),
      
      age_group = dplyr::case_when(        # <-- redefine age_group as the 10-bin variable
        age5 == 1 ~ "18–24",
        age5 == 2 ~ "25–29",
        age5 == 3 ~ "30–34",
        age5 == 4 ~ "35–39",
        age5 == 5 ~ "40–44",
        age5 == 6 ~ "45–49",
        age5 == 7 ~ "50–54",
        age5 == 8 ~ "55–59",
        age5 == 9 ~ "60–64",
        age5 %in% 10:13 ~ "65+",
        TRUE ~ NA_character_
      ),
      # 5-bin fallback for thin cells in models/plots
      age_fallback = dplyr::case_when(
        age5 == 1 ~ "18–24",
        age5 %in% 2:3 ~ "25–34",
        age5 %in% 4:6 ~ "35–49",
        age5 %in% 7:9 ~ "50–64",
        age5 %in% 10:13 ~ "65+",
        TRUE ~ NA_character_
      ),
      
      # ---------- Education (4 levels: <HS, HS/GED, Some college/AA, College) ----------
      educag_i = as_int(educag),
      educa_i  = as_int(educa),
      
      educ4 = dplyr::case_when(
        # Preferred: EDUCAG (CDC 4-level)
        !is.na(educag_i) ~ dplyr::recode(educag_i,
                                         `1` = "<HS",
                                         `2` = "HS/GED",
                                         `3` = "Some college/AA",
                                         `4` = "College",
                                         .default = NA_character_
        ),
        # Fallback: collapse EDUCA (6-level) to the same 4 groups
        is.na(educag_i) & !is.na(educa_i) ~ dplyr::case_when(
          educa_i %in% c(1L,2L,3L) ~ "<HS",             # ≤ grade 11
          educa_i == 4L            ~ "HS/GED",          # HS graduate/GED
          educa_i == 5L            ~ "Some college/AA", # some college/tech
          educa_i == 6L            ~ "College",
          TRUE ~ NA_character_
        ),
        TRUE ~ NA_character_
      ),
      
      # ---------- Income (3 groups) from INCOME2 (8-cat) or _INCOMG (6-cat) ----------
      income3_i = as_int(income3_raw),
      income2_i = as_int(income2_raw),
      incomg_i  = as_int(incomg_raw),
      # --- 3-group: "<$25k", "$25–<50k", "≥$50k" (Unknown for DK/Ref/Miss) ---
      income3 = dplyr::case_when(
        # Prefer INCOME3 (2013+ where present; certainly by 2023)
        !is.na(income3_i) & income3_i %in% c(77,99) ~ "Unknown",
        !is.na(income3_i) & income3_i %in% 1:11 ~ dplyr::case_when(
          income3_i %in% 1:4 ~ "<$25k",
          income3_i %in% 5:6 ~ "$25–<50k",
          income3_i %in% 7:11 ~ "≥$50k"
        ),
        # Fallback to INCOME2 (1..8; 77/99 DK/Ref)
        is.na(income3_i) & !is.na(income2_i) & income2_i %in% c(77,99) ~ "Unknown",
        is.na(income3_i) & !is.na(income2_i) & income2_i %in% 1:8 ~ dplyr::case_when(
          income2_i %in% 1:4 ~ "<$25k",
          income2_i %in% 5:6 ~ "$25–<50k",
          income2_i %in% 7:8 ~ "≥$50k"
        ),
        # Fallback to _INCOMG (1..6; 77/99 handled upstream as Unknown)
        is.na(income3_i) & is.na(income2_i) & !is.na(incomg_i) & incomg_i %in% 1:6 ~ dplyr::case_when(
          incomg_i %in% 1:2 ~ "<$25k",
          incomg_i %in% 3:4 ~ "$25–<50k",
          incomg_i %in% 5:6 ~ "≥$50k"
        ),
        TRUE ~ "Unknown"
      ),
      
      
      # --- 6-level: "<$15k","$15–<25k","$25–<35k","$35–<50k","$50–<75k","≥$75k","Unknown" ---
      income6 = dplyr::case_when(
        !is.na(income3_i) & income3_i %in% c(77,99) ~ "Unknown",
        !is.na(income3_i) & income3_i %in% 1:11 ~ dplyr::case_when(
          income3_i %in% 1:2 ~ "<$15k",
          income3_i %in% 3:4 ~ "$15–<25k",
          income3_i == 5L    ~ "$25–<35k",
          income3_i == 6L    ~ "$35–<50k",
          income3_i == 7L    ~ "$50–<75k",
          income3_i %in% 8:11 ~ "≥$75k"
        ),
        is.na(income3_i) & !is.na(income2_i) & income2_i %in% c(77,99) ~ "Unknown",
        is.na(income3_i) & !is.na(income2_i) & income2_i %in% 1:8 ~ dplyr::case_when(
          income2_i %in% 1:2 ~ "<$15k",
          income2_i %in% 3:4 ~ "$15–<25k",
          income2_i == 5L    ~ "$25–<35k",
          income2_i == 6L    ~ "$35–<50k",
          income2_i == 7L    ~ "$50–<75k",
          income2_i == 8L    ~ "≥$75k"
        ),
        is.na(income3_i) & is.na(income2_i) & !is.na(incomg_i) & incomg_i %in% 1:6 ~ dplyr::case_when(
          incomg_i == 1L ~ "<$15k",
          incomg_i == 2L ~ "$15–<25k",
          incomg_i == 3L ~ "$25–<35k",
          incomg_i == 4L ~ "$35–<50k",
          incomg_i == 5L ~ "$50–<75k",
          incomg_i == 6L ~ "≥$75k"
        ),
        TRUE ~ "Unknown"
      ),
      
      # --- High-income gradient (only available where INCOME3 exists) ---
      income_hi4 = dplyr::case_when(
        !is.na(income3_i) & income3_i %in% c(77,99) ~ "Unknown",
        !is.na(income3_i) & income3_i %in% 1:11 ~ dplyr::case_when(
          income3_i %in% 1:8  ~ "<$100k",
          income3_i == 9L     ~ "$100–<150k",
          income3_i == 10L    ~ "$150–<200k",
          income3_i == 11L    ~ "≥$200k"
        ),
        # Pre-INCOME3 years: we can’t see >$100k brackets → set as Unknown
        TRUE ~ "Unknown"
      ),
      
      
      
      # ---------- Employment (3 groups) from EMPLOY1 ----------
      employ_i = as_int(employ_raw),
      employ3 = dplyr::case_when(
        is.na(employ_i) ~ "Unknown",
        employ_i %in% c(9,77,99) ~ "Unknown",
        employ_i %in% c(1,2) ~ "Employed",
        employ_i %in% c(3,4) ~ "Unemployed",
        employ_i %in% c(5,6,7,8) ~ "NILF", # not in labor force
        TRUE ~ "Unknown"
      ),
      
      # ---------- Marital (2 groups) from MARITAL ----------
      marital_i = as_int(marital_raw),
      marital2 = dplyr::case_when(
        is.na(marital_i) ~ "Unknown",
        marital_i %in% c(9,77,99) ~ "Unknown",
        marital_i %in% c(1,6) ~ "Married/Partnered",
        marital_i %in% c(2,3,4,5) ~ "Not married",
        TRUE ~ "Unknown"
      ),
      
      # --- 3-level marital status ---
      marital3 = dplyr::case_when(
        is.na(marital_i) | marital_i %in% c(9,77,99) ~ "Unknown",
        marital_i %in% c(1,6) ~ "Married/Partnered",
        marital_i %in% c(2,3,4) ~ "Previously married",
        marital_i == 5 ~ "Never married",
        TRUE ~ "Unknown"
      ),
      
      
      # ---------- Insurance from HLTHPLN1 ----------
      ins_i = as_int(ins_raw),
      insured = dplyr::case_when(
        is.na(ins_i) ~ "Unknown",
        ins_i %in% c(7,9,77,99) ~ "Unknown",
        ins_i == 1L ~ "Yes",
        ins_i == 2L ~ "No",
        TRUE ~ "Unknown"
      ),
      
      
      # ---------- NEW: Harmonized race/ethnicity (4 groups) ----------
      imprace_i = as_int(imprace_raw),
      race1_i   = as_int(race1_raw),
      mrace2_i  = as_int(mrace2_raw),
      hisp_i    = as_int(hisp_raw),
      race4 = dplyr::case_when(
        # Prefer IMPRACE when present (2017+ widespread)
        !is.na(imprace_i) & imprace_i == 8L ~ "Hispanic",
        !is.na(imprace_i) & imprace_i == 1L ~ "NH White",
        !is.na(imprace_i) & imprace_i == 2L ~ "NH Black",
        !is.na(imprace_i) & imprace_i %in% c(3L,4L,5L,6L,7L) ~ "NH Other",
        # fallback to RACE1
        is.na(imprace_i) & !is.na(race1_i) & race1_i == 8L ~ "Hispanic",
        is.na(imprace_i) & !is.na(race1_i) & race1_i == 1L ~ "NH White",
        is.na(imprace_i) & !is.na(race1_i) & race1_i == 2L ~ "NH Black",
        is.na(imprace_i) & !is.na(race1_i) & race1_i %in% c(3L,4L,5L,6L,7L) ~ "NH Other",
        # derive from HISPANC + MRACE2 if needed
        is.na(imprace_i) & is.na(race1_i) & !is.na(hisp_i) & hisp_i == 1L ~ "Hispanic",
        is.na(imprace_i) & is.na(race1_i) & (is.na(hisp_i) | hisp_i == 2L) & !is.na(mrace2_i) & mrace2_i == 10L ~ "NH White",
        is.na(imprace_i) & is.na(race1_i) & (is.na(hisp_i) | hisp_i == 2L) & !is.na(mrace2_i) & mrace2_i == 20L ~ "NH Black",
        is.na(imprace_i) & is.na(race1_i) & (is.na(hisp_i) | hisp_i == 2L) & !is.na(mrace2_i) & mrace2_i %in% c(30L,40L,50L,51L,52L,53L,54L,60L,70L) ~ "NH Other",
        TRUE ~ NA_character_
      ),
      
      # ---------- NEW: Detailed race for national reporting (6 levels) ----------
      race6_nat = dplyr::case_when(
        !is.na(imprace_i) & imprace_i == 8L ~ "Hispanic",
        !is.na(imprace_i) & imprace_i == 1L ~ "NH White",
        !is.na(imprace_i) & imprace_i == 2L ~ "NH Black",
        !is.na(imprace_i) & imprace_i == 3L ~ "NH AI/AN",
        !is.na(imprace_i) & imprace_i %in% c(4L,5L) ~ "NH Asian/NHPI",
        !is.na(imprace_i) & imprace_i %in% c(6L,7L) ~ "NH Other/Multiracial",
        # fallbacks using race1/mrace2/hisp
        is.na(imprace_i) & !is.na(hisp_i) & hisp_i == 1L ~ "Hispanic",
        is.na(imprace_i) & (is.na(hisp_i) | hisp_i == 2L) & !is.na(race1_i) & race1_i == 1L ~ "NH White",
        is.na(imprace_i) & (is.na(hisp_i) | hisp_i == 2L) & !is.na(race1_i) & race1_i == 2L ~ "NH Black",
        is.na(imprace_i) & (is.na(hisp_i) | hisp_i == 2L) & !is.na(mrace2_i) & mrace2_i == 30L ~ "NH AI/AN",
        is.na(imprace_i) & (is.na(hisp_i) | hisp_i == 2L) &
          !is.na(mrace2_i) & mrace2_i %in% c(40L,50L,51L,52L,53L,54L) ~ "NH Asian/NHPI",
        is.na(imprace_i) & (is.na(hisp_i) | hisp_i == 2L) &
          !is.na(mrace2_i) & mrace2_i %in% c(60L,70L) ~ "NH Other/Multiracial",
        TRUE ~ NA_character_
      )
      
    
    ) %>%
    select(
      year, wt, strata, psu, state, sex,
      age5, age_group, age_fallback,
      educ4, race4, race6_nat,
      income3, income6, income_hi4,
      employ3, marital2, marital3, insured,   # <- add marital3 here
      alcday5, rfbing5, binge_episodes, any_binge_cdc, freq5,
      max_drinks, averdrinks
    ) %>%
    
    mutate(
      psu = as.character(psu), strata = as.character(strata),
      wt  = as.numeric(wt),    year   = as.integer(year),
      any_binge_cdc  = as.integer(any_binge_cdc),
      binge_episodes = as.integer(binge_episodes),
      age_group   = factor(age_group,
                           levels = c("18–24","25–29","30–34","35–39","40–44",
                                      "45–49","50–54","55–59","60–64","65+"),
                           ordered = TRUE),
      age_fallback = factor(age_fallback,
                            levels = c("18–24","25–34","35–49","50–64","65+"),
                            ordered = TRUE),
      race4 = factor(race4, levels = c("NH White","NH Black","Hispanic","NH Other")),
      race6_nat = factor(race6_nat,
                         levels = c("NH White","NH Black","Hispanic","NH Asian/NHPI",
                                    "NH AI/AN","NH Other/Multiracial")),
      educ4 = factor(educ4, levels = c("<HS","HS/GED","Some college/AA","College"), ordered = TRUE),
      income3 = factor(income3, levels = c("<$25k","$25–<50k","≥$50k","Unknown"), ordered = TRUE),
      income6    = factor(income6,
                          levels = c("<$15k","$15–<25k","$25–<35k","$35–<50k","$50–<75k","≥$75k","Unknown"),
                          ordered = TRUE),
      income_hi4 = factor(income_hi4,
                          levels = c("<$100k","$100–<150k","$150–<200k","≥$200k","Unknown"),
                          ordered = TRUE),
      
      employ3 = factor(employ3, levels = c("Employed","Unemployed","NILF","Unknown")),
      marital2 = factor(marital2, levels = c("Married/Partnered","Not married","Unknown")),
      marital3 = factor(marital3,
                        levels = c("Married/Partnered","Never married","Previously married","Unknown")),
      insured  = factor(insured,  levels = c("Yes","No","Unknown"))
    )
}

# ---- minimal reader with cross-year aliases (2011–2024) + race sources ----
read_year_min <- function(yr, path){
  df <- haven::read_xpt(path) |> janitor::clean_names()

  # ---- resolved source names (strings) ----
  nm_wt     <- find_var(df, c("_llcpwt","llcpwt","x_llcpwt","cllcpwt","llcpwt2","llcpwt22"))
  nm_strata <- find_var(df, c("_ststr","ststr","x_ststr","ststr2"))
  nm_psu    <- find_var(df, c("_psu","psu","x_psu"))

  # alcohol frequency (ALCDAY5 <=2019, ALCDAY4 >=2020)
  nm_alcday <- find_var(df, c("alcday5","alcday_5","alcday","alcday4"))

  # binge episode and CDC flag
  nm_binge  <- find_var(df, c("drnk3ge5","_drnk3ge5"))
  nm_rfbing <- find_var(df, c("_rfbing6","rfbing6","_rfbing5","rfbing5","_rfbing4","rfbing4"))
  
  nm_maxd   <- find_var(df, c("maxdrnks","_maxdrnks"))
  nm_aved   <- find_var(df, c("avedrnk3","averdrnk3","avedrnk2","_avedrnk"))

  # demographics
  nm_sex    <- find_var(df, c("sex","sex1","sexvar","rcsgendr"))
  nm_age5   <- find_var(df, c("_ageg5yr","ageg5yr","x_ageg5yr"))
  nm_educag <- find_var(df, c("educag","_educag","x_educag"))
  nm_educa  <- find_var(df, c("educa"))
  nm_state  <- find_var(df, c("state", "x_state"))

  # race/ethnicity sources
  nm_impr   <- find_var(df, c("_imprace","imprace"))
  nm_race1  <- find_var(df, c("_race1","race1","_racegr3","racegr3","_racegr2","racegr2"))
  nm_mrace2 <- find_var(df, c("_mrace2","mrace2"))
  nm_hisp   <- find_var(df, c("hispanc","hispanic","hispan2","hispan1","_hispanc"))
  
  nm_income2 <- find_var(df, c("income2"))
  nm_income3 <- find_var(df, c("income3"))
  nm_incomg  <- find_var(df, c("_incomg","incomg"))
  nm_employ1 <- find_var(df, c("employ1","_employ1","employ"))
  nm_marital <- find_var(df, c("marital","_marital"))
  nm_hlthpln <- find_var(df, c("hlthpln1","_hlthpln1","hlthpln2"))
  

  # required inputs to proceed
  must <- c(nm_wt, nm_strata, nm_psu, nm_alcday, nm_binge, nm_sex, nm_age5)
  if (any(is.na(must))) {
    message("Missing key variables for year ", yr, ". Resolved:\n",
            sprintf(paste(
              "  wt=%s strata=%s psu=%s alcday=%s binge=%s sex=%s age5=%s",
              "rfbing=%s max=%s aved=%s state=%s imprace=%s race1=%s mrace2=%s hispanc=%s",
              sep = "\n"),
              nm_wt,nm_strata,nm_psu,nm_alcday,nm_binge,nm_sex,nm_age5,
              nm_rfbing,nm_maxd,nm_aved,nm_state,nm_impr,nm_race1,nm_mrace2,nm_hisp),
            "\nAvailable:\n", paste(names(df), collapse=", "))
    stop("Update candidate mappings for this file.")
  }

  # ---- standardized minimal dataset ----
  dplyr::transmute(
    df,
    year = as.integer(yr),
    wt   = .data[[nm_wt]],
    strata = .data[[nm_strata]],
    psu    = .data[[nm_psu]],

    # keep unified name 'alcday5' for downstream code, regardless of ALCDAY4/5 source
    alcday5 = .data[[nm_alcday]],

    binge_episodes_raw = .data[[nm_binge]],
    rfbing5            = if (!is.na(nm_rfbing)) .data[[nm_rfbing]] else NA,
    max_drinks_raw     = if (!is.na(nm_maxd))   .data[[nm_maxd]]   else NA,
    averdrnk_raw       = if (!is.na(nm_aved))   .data[[nm_aved]]   else NA,

    sex_raw            = .data[[nm_sex]],
    age5               = .data[[nm_age5]],
    educag             = if (!is.na(nm_educag)) .data[[nm_educag]] else NA,
    educa              = if (!is.na(nm_educa))  .data[[nm_educa]]  else NA,
    state              = if (!is.na(nm_state))  .data[[nm_state]]  else NA,

    # carry raw race sources for recode_analysis()
    imprace_raw        = if (!is.na(nm_impr))   .data[[nm_impr]]   else NA,
    race1_raw          = if (!is.na(nm_race1))  .data[[nm_race1]]  else NA,
    mrace2_raw         = if (!is.na(nm_mrace2)) .data[[nm_mrace2]] else NA,
    hisp_raw           = if (!is.na(nm_hisp))   .data[[nm_hisp]]   else NA, 
    
    income2_raw  = if (!is.na(nm_income2)) .data[[nm_income2]] else NA,
    income3_raw  = if (!is.na(nm_income3)) .data[[nm_income3]] else NA,
    incomg_raw   = if (!is.na(nm_incomg))  .data[[nm_incomg]]  else NA,
    employ_raw   = if (!is.na(nm_employ1)) .data[[nm_employ1]] else NA,
    marital_raw  = if (!is.na(nm_marital)) .data[[nm_marital]] else NA,
    ins_raw      = if (!is.na(nm_hlthpln)) .data[[nm_hlthpln]] else NA
  )
}

# ===== CACHE VERSIONING =====
# Put this near the top (after PROJ_DIR). Bump CACHE_VER anytime recode logic changes.

CACHE_VER <- "v2026-01-16_popTrend"  # <- change this string to force rebuild

read_year_cached <- function(yr, xpt_path){
  cache_dir <- file.path(PROJ_DIR, "data/cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  cache_rds <- file.path(cache_dir, paste0("BRFSS_", yr, "_", CACHE_VER, ".rds"))
  
  # If cache exists, load and validate
  if (file.exists(cache_rds)) {
    obj <- tryCatch(readRDS(cache_rds), error = function(e) NULL)
    
    needed <- c("year","wt","strata","psu","state","sex","age5","age_group",
                "any_binge_cdc","binge_episodes","alcday5",
                "educ4","income3","income6","race4","race6_nat",
                "employ3","marital2","marital3","insured","max_drinks","averdrinks")
    
    ok_educ_levels <- c("<HS","HS/GED","Some college/AA","College")
    
    if (!is.null(obj) &&
        inherits(obj, "data.frame") &&
        nrow(obj) > 0 &&
        all(needed %in% names(obj)) &&
        !is.null(levels(obj$educ4)) &&
        identical(levels(obj$educ4), ok_educ_levels)) {
      return(obj)
    }
  }
  
  # Build fresh
  d <- read_year_min(yr, xpt_path) |>
    recode_analysis() |>
    dplyr::filter(!is.na(wt), !is.na(strata), !is.na(psu))
  
  saveRDS(d, cache_rds)
  d
}
# ===== END CACHE  =====





# ===== Read & combine all years =====
yrs   <- as.integer(names(YEAR_PATHS))
paths <- unname(YEAR_PATHS)

# only read years with existing files
exists_vec <- file.exists(paths)
if (!all(exists_vec)) {
  message("Skipping missing files for years: ",
          paste(yrs[!exists_vec], collapse = ", "))
  yrs   <- yrs[exists_vec]
  paths <- paths[exists_vec]
}

# ===== Resolver audit: which raw variables were selected per year =====
audit_find <- function(df){
  list(
    wt      = find_var(df, c("_llcpwt","llcpwt","x_llcpwt","cllcpwt","llcpwt2","llcpwt22")),
    strata  = find_var(df, c("_ststr","ststr","x_ststr","ststr2")),
    psu     = find_var(df, c("_psu","psu","x_psu")),
    alcday  = find_var(df, c("alcday5","alcday_5","alcday","alcday4")),
    binge   = find_var(df, c("drnk3ge5","_drnk3ge5")),
    rfbing  = find_var(df, c("_rfbing6","rfbing6","_rfbing5","rfbing5","_rfbing4","rfbing4")),
    maxd    = find_var(df, c("maxdrnks","_maxdrnks")),
    aved    = find_var(df, c("avedrnk3","averdrnk3","avedrnk2","_avedrnk")),
    sex     = find_var(df, c("sex","sex1","sexvar","rcsgendr")),
    age5    = find_var(df, c("_ageg5yr","ageg5yr","x_ageg5yr")),
    educag  = find_var(df, c("educag","_educag","x_educag")),
    educa   = find_var(df, c("educa")),
    state   = find_var(df, c("state", "x_state")),
    imprace = find_var(df, c("_imprace","imprace")),
    race1   = find_var(df, c("_race1","race1","_racegr3","racegr3","_racegr2","racegr2")),
    mrace2  = find_var(df, c("_mrace2","mrace2")),
    hisp    = find_var(df, c("hispanc","hispanic","hispan2","hispan1","_hispanc")),
    income2 = find_var(df, c("income2")),
    income3 = find_var(df, c("income3")),
    incomg  = find_var(df, c("_incomg","incomg")),
    employ1 = find_var(df, c("employ1","_employ1","employ")),
    marital = find_var(df, c("marital","_marital")),
    
    hlthpln = find_var(df, c("hlthpln1","_hlthpln1","hlthpln2"))
  )
}
aud <- purrr::map2_dfr(
  yrs, paths,
  ~{
    df_head <- haven::read_xpt(.y) %>% janitor::clean_names()
    tibble::tibble(year = .x, !!!audit_find(df_head))
  }
)
readr::write_csv(aud, file.path(PROJ_DIR,"docs/source_map_per_year.csv"))



# === QC ===
bad <- aud %>%
  dplyr::filter(is.na(wt) | is.na(strata) | is.na(psu) | is.na(alcday) | is.na(binge) | is.na(sex) | is.na(age5))

if (nrow(bad) > 0) {
  print(bad)
  stop("Resolver failed for one or more core variables in the years shown above.")
}
# === END QC ===

vars_needed <- c("wt","psu","strata","year","state","age7","any_binge_cdc",
                 "sex_f","race6_nat","educ4","income6","employ3","marital3","insured")


USE_PARALLEL <- FALSE

dlist <- purrr::map2(
  yrs, paths,
  ~ tryCatch(
    read_year_cached(.x, .y),
    error = function(e) {
      message("Year ", .x, " skipped: ", conditionMessage(e))
      tibble::tibble()
    }
  )
)

df_all <- dplyr::bind_rows(dlist)
rm(dlist); gc()



# ---- Exclude territories (keep 50 states + DC) ----
to_int <- function(x) suppressWarnings(as.integer(haven::zap_labels(x)))
territories <- c(60L, 66L, 69L, 72L, 78L)  # AS, GU, MP, PR, VI

# ---- state crosswalk (FIPS -> abbr/name; includes territories used by BRFSS) ----
state_xwalk <- tibble::tribble(
  ~fips, ~state_abbr, ~state_name,
  1,  "AL", "Alabama",
  2,  "AK", "Alaska",
  4,  "AZ", "Arizona",
  5,  "AR", "Arkansas",
  6,  "CA", "California",
  8,  "CO", "Colorado",
  9,  "CT", "Connecticut",
  10,  "DE", "Delaware",
  11,  "DC", "District of Columbia",
  12,  "FL", "Florida",
  13,  "GA", "Georgia",
  15,  "HI", "Hawaii",
  16,  "ID", "Idaho",
  17,  "IL", "Illinois",
  18,  "IN", "Indiana",
  19,  "IA", "Iowa",
  20,  "KS", "Kansas",
  21,  "KY", "Kentucky",
  22,  "LA", "Louisiana",
  23,  "ME", "Maine",
  24,  "MD", "Maryland",
  25,  "MA", "Massachusetts",
  26,  "MI", "Michigan",
  27,  "MN", "Minnesota",
  28,  "MS", "Mississippi",
  29,  "MO", "Missouri",
  30,  "MT", "Montana",
  31,  "NE", "Nebraska",
  32,  "NV", "Nevada",
  33,  "NH", "New Hampshire",
  34,  "NJ", "New Jersey",
  35,  "NM", "New Mexico",
  36,  "NY", "New York",
  37,  "NC", "North Carolina",
  38,  "ND", "North Dakota",
  39,  "OH", "Ohio",
  40,  "OK", "Oklahoma",
  41,  "OR", "Oregon",
  42,  "PA", "Pennsylvania",
  44,  "RI", "Rhode Island",
  45,  "SC", "South Carolina",
  46,  "SD", "South Dakota",
  47,  "TN", "Tennessee",
  48,  "TX", "Texas",
  49,  "UT", "Utah",
  50,  "VT", "Vermont",
  51,  "VA", "Virginia",
  53,  "WA", "Washington",
  54,  "WV", "West Virginia",
  55,  "WI", "Wisconsin",
  56,  "WY", "Wyoming",
  60,  "AS", "American Samoa",
  66,  "GU", "Guam",
  69,  "MP", "Northern Mariana Islands",
  72,  "PR", "Puerto Rico",
  78,  "VI", "U.S. Virgin Islands"
)


df_all <- df_all %>%
  dplyr::mutate(state_code = to_int(state)) %>%
  dplyr::filter(!is.na(state_code), !(state_code %in% territories)) %>%
  dplyr::select(-state_code)

sort(unique(to_int(df_all$state)))  # should NOT include 60,66,69,72,78

# -----------------------------------------------------------


# quick sanity check
cat("Combined rows:", nrow(df_all), "  Years in data:", paste(sort(unique(df_all$year)), collapse=", "), "\n")
print(df_all %>% count(year) %>% arrange(year), n=Inf)

# OPTIONAL: write a slim QC to CSV
readr::write_csv(df_all %>% count(year) %>% arrange(year),
                 file.path(tab_dir, "qc_rows_by_year.csv"))

saveRDS(df_all, file.path(PROJ_DIR, "data/cache/df_all_after_race_state.rds"))

# # ===== Missingness table (unweighted N, weighted %) BEFORE core exclusions =====
# df_pre <- readRDS(file.path(PROJ_DIR, "data/cache/df_all_after_race_state.rds"))
# 
# vars_check <- c(
#   "any_binge_cdc","binge_episodes","alcday5","sex","age_group",
#   "race4","educ4","income3","employ3","marital2","insured",
#   "max_drinks","averdrinks","state"
# )
# 
# # --- Unweighted missing counts (fast, no pivoting across types) ---
# miss_unw <- tibble::tibble(
#   variable = vars_check,
#   n_unw_missing = vapply(df_pre[vars_check], function(x) sum(is.na(x)), integer(1))
# )
# 
# # --- Slim survey design (same IDs/strata/weights; fewer columns for speed) ---
# df_pre_small <- df_pre %>% dplyr::select(wt, psu, strata, year, dplyr::all_of(vars_check))
# des_pre <- survey::svydesign(
#   id = ~interaction(year, psu),
#   strata = ~interaction(year, strata),
#   weights = ~wt,
#   data = df_pre_small,
#   nest = TRUE
# )
# 
# # --- Vectorized weighted missingness (one svymean call) ---
# miss_terms <- paste0("I(is.na(", vars_check, "))")
# f_miss <- as.formula(paste("~", paste(miss_terms, collapse = " + ")))
# 
# p_miss <- survey::svymean(f_miss, design = des_pre, na.rm = TRUE)
# ci_miss <- suppressWarnings(confint(p_miss))
# 
# miss_wt <- tibble::tibble(
#   variable = vars_check,
#   pct_wt_missing = as.numeric(p_miss) * 100,
#   lcl = ci_miss[, 1] * 100,
#   ucl = ci_miss[, 2] * 100
# )
# 
# # --- Final table ---
# miss_tab <- dplyr::left_join(miss_unw, miss_wt, by = "variable") %>%
#   dplyr::arrange(dplyr::desc(n_unw_missing))
# 
# readr::write_csv(miss_tab, file.path(tab_dir, "qc_missingness_before_core_exclusions.csv"))
# 
# # ===== (Optional) Equivalence check: full vs slim design for missingness =====
# des_pre_full <- survey::svydesign(
#   id = ~interaction(year, psu),
#   strata = ~interaction(year, strata),
#   weights = ~wt,
#   data = df_pre,
#   nest = TRUE
# )
# 
# all.equal(
#   as.numeric(survey::svymean(f_miss, des_pre_full, na.rm = TRUE)),
#   as.numeric(p_miss),
#   tolerance = 1e-12
# )





# ---- Drop missing core covariates (per Methods) ----
df_all <- df_all %>%
  dplyr::filter(!is.na(sex), !is.na(age_group))
# -------------------------------------------------------------



# ===== A) Build pooled survey design (2011–2024) =====
# (Put this right after your qc_rows_by_year.csv write)

# Ensure year-specific strata/PSU so pooling is valid
df_all <- df_all %>%
  dplyr::mutate(
    strata = interaction(year, strata, drop = TRUE, lex.order = TRUE),
    psu    = interaction(year, psu,    drop = TRUE, lex.order = TRUE)
  )

# Normalize dash variants to ASCII hyphen and enforce canonical levels
# Canonical labels we want
canon10 <- c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65+")

# Normalizer: strip suffixes like ".15-19", normalize dashes/spaces
fix_age <- function(x){
  x <- as.character(x)
  x <- sub("\\..*$", "", x)                  # remove anything after first dot
  x <- gsub("\u2013|\u2014|\u2212", "-", x)  # en/em/minus → hyphen
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# 1) Clean age_group in the DATA (not the design)
df_all <- df_all %>%
  dplyr::mutate(age_group = factor(fix_age(age_group),
                                   levels = canon10, ordered = TRUE))

# (Optional sanity)
df_all %>%
  summarise(
    n = n(),
    n_age_na = sum(is.na(age_group)),
    pct_age_na = mean(is.na(age_group))*100
  )   


# Create 7-band age factor from _AGEG5YR (age5: 1..13)
df_all <- df_all %>%
  dplyr::mutate(
    age7 = dplyr::case_when(
      age5 %in% 1:2  ~ "18-29",  # 18-24, 25-29
      age5 %in% 3:4  ~ "30-39",
      age5 %in% 5:6  ~ "40-49",
      age5 %in% 7:8  ~ "50-59",
      age5 %in% 9:10 ~ "60-69",  # 60-64 + 65-69
      age5 %in% 11:12~ "70-79",
      age5 == 13     ~ "80+",
      TRUE ~ NA_character_
    ) |> factor(levels = c("18-29","30-39","40-49","50-59","60-69","70-79","80+"))
  )



# 2) Rebuild the survey design objects that depend on age_group
des_all <- survey::svydesign(id = ~psu, strata = ~strata, weights = ~wt, data = df_all, nest = TRUE)

des_all <- stats::update(
  des_all,
  any_binge_f = factor(any_binge_cdc, levels = c(0,1), labels = c("No","Yes"))
)


des_all <- stats::update(
  des_all,
  alcday5 = suppressWarnings(as.integer(haven::zap_labels(alcday5)))
)

des_all <- stats::update(
  des_all,
  
  is_current = dplyr::case_when(
    is.na(alcday5) ~ NA,
    alcday5 %in% c(777, 999) ~ NA,
    alcday5 == 888 ~ FALSE,
    TRUE ~ TRUE),
  
  year_lin   = as.numeric(year) - 2011,
  drink_days = alcday5_to_days30(alcday5),
  sex_f      = factor(as.integer(sex), levels = c(2,1), labels = c("Female","Male")),
  race4      = factor(race4, levels = c("NH White","NH Black","Hispanic","NH Other")),
  race6_nat  = factor(race6_nat, levels = c("NH White","NH Black","Hispanic","NH Asian/NHPI","NH AI/AN","NH Other/Multiracial")),
  educ4      = factor(educ4, levels = c("<HS","HS/GED","Some college/AA","College"), ordered = TRUE),
  income3    = factor(income3, levels = c("<$25k","$25–<50k","≥$50k","Unknown"), ordered = TRUE),
  income6    = factor(income6, levels = c("<$15k","$15–<25k","$25–<35k","$35–<50k","$50–<75k","≥$75k","Unknown"), ordered = TRUE),
  employ3    = factor(employ3, levels = c("Employed","Unemployed","NILF","Unknown")),
  marital2   = factor(marital2, levels = c("Married/Partnered","Not married","Unknown")),
  marital3   = factor(marital3, levels = c("Married/Partnered","Never married","Previously married","Unknown")),
  insured    = factor(insured, levels = c("Yes","No","Unknown"))
)

des_all <- stats::update(
  des_all,
  any_binge_f = factor(any_binge_cdc, levels = c(0,1), labels = c("No","Yes"))
)



des_curr <- subset(des_all, is_current)




# ==== 7-band Age Standardization (Year 2000, adults 18+) ====

# 2000 standard population counts in 5-year bins (millions scaled to counts) — from NCHS Stat Note 20
# If your PDF uses en dashes, keep the exact labels below or normalize consistently.
std2000_5yr <- c(
  "15-19"=72169, "20-24"=66478, "25-29"=64529, "30-34"=71044, "35-39"=80762,
  "40-44"=81851, "45-49"=72118, "50-54"=62716, "55-59"=48454, "60-64"=38793,
  "65-69"=34264, "70-74"=31773, "75-79"=26999, "80-84"=17842, "85+"=15508
)

# Helper to access with ASCII hyphens (your dataset already normalized dashes earlier)
g <- function(k) std2000_5yr[[k]]

# Build 10-bin adult 18+ counts (to keep your 10-bin outputs working too)
std_10bin_counts <- c(
  "18-24" = 0.4*g("15-19") + g("20-24"),    # 18–19 is 2/5 of 15–19
  "25-29" = g("25-29"),
  "30-34" = g("30-34"),
  "35-39" = g("35-39"),
  "40-44" = g("40-44"),
  "45-49" = g("45-49"),
  "50-54" = g("50-54"),
  "55-59" = g("55-59"),
  "60-64" = g("60-64"),
  "65+"   = g("65-69") + g("70-74") + g("75-79") + g("80-84") + g("85+")
)

std2000_adult10 <- std_10bin_counts / sum(std_10bin_counts)
names(std2000_adult10) <- c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65+")


# Derive 7-band Year-2000 adult weights from the same 5-year standard
w_2000_age7_counts <- setNames(
  unname(c(
    std_10bin_counts["18-24"] + std_10bin_counts["25-29"],
    std_10bin_counts["30-34"] + std_10bin_counts["35-39"],
    std_10bin_counts["40-44"] + std_10bin_counts["45-49"],
    std_10bin_counts["50-54"] + std_10bin_counts["55-59"],
    std_10bin_counts["60-64"] + g("65-69"),
    g("70-74") + g("75-79"),
    g("80-84") + g("85+")
  )),
  c("18-29","30-39","40-49","50-59","60-69","70-79","80+")
)
w_2000_age7 <- w_2000_age7_counts / sum(w_2000_age7_counts)

# sanity
stopifnot(setequal(names(w_2000_age7), levels(des_all$variables$age7)))



# --- Standardized designs using 7 bands (by year; by year×race; by year×sex; by year×income; by year×state)

des_all_std_y7 <- survey::svystandardize(
  subset(des_all, !is.na(age7)),
  by = ~age7, over = ~year,
  population = w_2000_age7_counts
)


des_all_std_yrace7  <- survey::svystandardize(
  subset(des_all, !is.na(age7) & !is.na(race6_nat)),
  by=~age7, over=~year + race6_nat, population=w_2000_age7_counts)

des_all_std_ysex7   <- survey::svystandardize(subset(des_all, !is.na(age7) & !is.na(sex_f)),
                                              by=~age7, over=~year + sex_f,  population=w_2000_age7_counts)

des_all_std_yinc7 <- survey::svystandardize(
  subset(des_all, !is.na(age7) & !is.na(income6)),
  by=~age7, over=~year + income6, population=w_2000_age7_counts)


des_all_std_ystate7 <- survey::svystandardize(subset(des_all, !is.na(age7) & !is.na(state)),
                                              by=~age7, over=~year + state, population=w_2000_age7_counts)


des_curr_y7 <- subset(des_all, is_current & !is.na(age7))
des_curr_std_y7 <- survey::svystandardize(
  des_curr_y7,
  by = ~age7, over = ~year,
  population = w_2000_age7_counts  # (see #4 below)
)

des_curr_std_yrace7 <- survey::svystandardize(
  subset(des_all, is_current & !is.na(age7) & !is.na(race6_nat)),
  by = ~age7, over = ~year + race6_nat,
  population = w_2000_age7_counts
)

des_curr_std_ystate7 <- survey::svystandardize(
  subset(des_all, is_current & !is.na(age7) & !is.na(state)),
  by = ~age7, over = ~year + state,
  population = w_2000_age7_counts
)





# Align to the data’s factor level order
std2000_adult10 <- std2000_adult10[levels(df_all$age_group)]



# Optional safety check
stopifnot(identical(names(std2000_adult10), levels(df_all$age_group)),
          all(!is.na(std2000_adult10)))



# --- Build standardized designs (10-bin) ---
des_all_std_y <- survey::svystandardize(
  subset(des_all, !is.na(age_group)),
  by = ~age_group, over = ~year,
  population = std_10bin_counts
)

des_all_std_yrace <- survey::svystandardize(
  subset(des_all, !is.na(age_group) & !is.na(race4)),
  by = ~age_group, over = ~year + race4,
  population = std_10bin_counts
)

des_all_std_ystate <- survey::svystandardize(
  subset(des_all, !is.na(age_group) & !is.na(state)),
  by = ~age_group, over = ~year + state,
  population = std_10bin_counts
)

# Current drinkers versions (subset AFTER standardization)
des_curr_std_y      <- subset(des_all_std_y,      is_current)
des_curr_std_yrace  <- subset(des_all_std_yrace,  is_current)
des_curr_std_ystate <- subset(des_all_std_ystate, is_current)

stopifnot(setequal(names(w_2000_age7), levels(des_all$variables$age7)))


# ===== Age-adjusted prevalence (Year 2000 standard) =====

# (1) All adults, by year
prev_all_prop_ageadj <- survey::svyby(
  ~ any_binge_cdc, ~ year, des_all_std_y7, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
)%>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    year = as.integer(year),
    est  = any_binge_cdc,
    lcl  = ci_l,
    ucl  = ci_u
  )
readr::write_csv(prev_all_prop_ageadj,
                 file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_prop.csv"))
readr::write_csv(prev_all_prop_ageadj %>% dplyr::mutate(
  est = est*100, lcl = lcl*100, ucl = ucl*100,
  ci  = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
  file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_pct.csv"))


# (2) Current drinkers, by year
prev_curr_prop_ageadj <- survey::svyby(
  ~ any_binge_cdc, ~ year, des_curr_std_y7, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    year = as.integer(year),
    est  = any_binge_cdc,
    lcl  = ci_l,
    ucl  = ci_u
  )
readr::write_csv(prev_curr_prop_ageadj,
                 file.path(tab_dir, "main_prev_any_binge_current_AGEADJ2000_CI_prop.csv"))
readr::write_csv(prev_curr_prop_ageadj %>% dplyr::mutate(
  est = est*100, lcl = lcl*100, ucl = ucl*100,
  ci  = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
  file.path(tab_dir, "main_prev_any_binge_current_AGEADJ2000_CI_pct.csv"))
  
# (3) By race/ethnicity (all adults)
prev_race_all_ageadj <- survey::svyby(
  ~ any_binge_cdc, ~ year + race6_nat, des_all_std_yrace7, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
)  %>%
  tibble::as_tibble() %>%
  dplyr::transmute(race6_nat = as.character(race6_nat), year = as.integer(year),
                   est = any_binge_cdc, lcl = ci_l, ucl = ci_u) %>%
  dplyr::arrange(race6_nat, year)
readr::write_csv(prev_race_all_ageadj,
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_AGEADJ2000_CI_prop.csv"))
readr::write_csv(prev_race_all_ageadj %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100)),
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_AGEADJ2000_CI_pct.csv"))

# (4) By race/ethnicity among current drinkers
prev_race_curr_ageadj <- survey::svyby(
  ~ any_binge_cdc, ~ year + race6_nat, des_curr_std_yrace7, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
)  %>%
  tibble::as_tibble() %>%
  dplyr::transmute(race6_nat = as.character(race6_nat), year = as.integer(year),
                   est = any_binge_cdc, lcl = ci_l, ucl = ci_u)%>%
  dplyr::arrange(race6_nat, year)
readr::write_csv(prev_race_curr_ageadj,
                 file.path(tab_dir, "main_prev_any_binge_current_by_race6_AGEADJ2000_CI_prop.csv"))
readr::write_csv(prev_race_curr_ageadj %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100)),
                 file.path(tab_dir, "main_prev_any_binge_current_by_race6_AGEADJ2000_CI_pct.csv"))

# (5) States — All adults (AGE-ADJUSTED, 7-band)
state_prev_all_prop_ageadj <- survey::svyby(
  ~ any_binge_cdc, ~ year + state, des_all_std_ystate7, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    state_code = to_int(state),
    year = as.integer(year)
  ) %>%
  dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::transmute(
    year, state_code, state_abbr, state_name,
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(state_code, year)

readr::write_csv(
  state_prev_all_prop_ageadj,
  file.path(tab_dir, "main_state_prev_any_binge_alladults_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  state_prev_all_prop_ageadj %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                                               ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
  file.path(tab_dir, "main_state_prev_any_binge_alladults_AGEADJ2000_CI_pct.csv")
)

# (6) States — Current drinkers
state_prev_curr_prop_ageadj <- survey::svyby(
  ~ any_binge_cdc, ~ year + state, des_curr_std_ystate7, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    state_code = to_int(state),
    year = as.integer(year)
  ) %>%
  dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::transmute(
    year, state_code, state_abbr, state_name,
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(state_code, year)

readr::write_csv(
  state_prev_curr_prop_ageadj,
  file.path(tab_dir, "main_state_prev_any_binge_current_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  state_prev_curr_prop_ageadj %>%
    dplyr::mutate(
      dplyr::across(c(est, lcl, ucl), ~ .x * 100),
      ci = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_state_prev_any_binge_current_AGEADJ2000_CI_pct.csv")
)


# ===========================================================

# ===== Step 13) One-time CDC binge flag coding check =====
check_year <- min(df_all$year, na.rm=TRUE)
d_chk <- dplyr::filter(df_all, year == check_year)

tab_chk <- d_chk %>%
  dplyr::mutate(rf = suppressWarnings(as.integer(rfbing5))) %>%
  dplyr::filter(!is.na(rf) & !is.na(any_binge_cdc)) %>%
  dplyr::count(rf, any_binge_cdc) %>%
  dplyr::arrange(rf, any_binge_cdc)

print(tab_chk)
mis <- tab_chk %>%
  dplyr::filter(!(rf==1 & any_binge_cdc==0) & !(rf==2 & any_binge_cdc==1)) %>%
  dplyr::summarise(mis = sum(n)) %>% dplyr::pull(mis)
message("CDC binge flag mismatches in year ", check_year, ": ", ifelse(length(mis)==0, 0L, mis))

chk <- purrr::map_dfr(sort(unique(df_all$year)), function(y){
  d <- dplyr::filter(df_all, year == y)
  tab <- d %>%
    dplyr::mutate(rf = suppressWarnings(as.integer(rfbing5))) %>%
    dplyr::filter(!is.na(rf) & !is.na(any_binge_cdc)) %>%
    dplyr::count(rf, any_binge_cdc)
  mis <- tab %>% dplyr::filter(!(rf==1 & any_binge_cdc==0) & !(rf==2 & any_binge_cdc==1)) %>%
    dplyr::summarise(mis = sum(n), .groups="drop") %>% dplyr::pull(mis)
  tibble::tibble(year=y, mismatches = ifelse(length(mis)==0, 0L, mis))
})
readr::write_csv(chk, file.path(tab_dir,"qc_cdc_binge_flag_mismatches_by_year.csv"))


# ---- Census region helper + add to design (Step 16a) ----
state_to_region <- function(fips){
  f <- suppressWarnings(as.integer(as.character(fips)))
  dplyr::case_when(
    f %in% c(9,23,25,33,44,50,34,36,42) ~ "Northeast",
    f %in% c(17,18,26,39,55,19,20,27,29,31,38,46) ~ "Midwest",
    f %in% c(10,11,12,13,24,37,45,51,54,1,21,28,47,5,22,40,48) ~ "South",
    f %in% c(4,8,16,30,32,35,49,56,2,6,15,41,53) ~ "West",
    TRUE ~ NA_character_
  )
}
des_all <- stats::update(
  des_all,
  region = factor(state_to_region(state),
                  levels = c("Northeast","Midwest","South","West"))
)
des_curr <- subset(des_all, is_current)


# ---- Primary adjusted PR model (current drinkers) ----
fit_pr_curr_lin <- survey::svyglm(
  any_binge_cdc ~ year_lin + sex_f + age7 + race6_nat + educ4 + income6 + employ3 + marital3 + insured + factor(state),
  design = des_curr,
  family = quasipoisson(link = "log")
)

# (Optional secondary) All adults version
fit_pr_all_lin <- survey::svyglm(
  any_binge_cdc ~ year_lin + sex_f + age7 + race6_nat + educ4 + income6 + employ3 + marital3 + insured + factor(state),
  design = des_all,
  family = quasipoisson(link = "log")
)

# Export PR tables
mk_pr_table <- function(fit){
  co <- stats::coef(fit); ci <- stats::confint(fit); sm <- summary(fit)$coefficients
  tibble::tibble(
    term = names(co),
    PR   = exp(co),
    LCL  = exp(ci[,1]),
    UCL  = exp(ci[,2]),
    p    = sm[,4]
  )
}

pr_curr <- mk_pr_table(fit_pr_curr_lin)
pr_all  <- mk_pr_table(fit_pr_all_lin)

readr::write_csv(pr_curr, file.path(tab_dir, "adj_PR_currentdrinkers_linetime.csv"))
readr::write_csv(pr_all,  file.path(tab_dir, "adj_PR_alladults_linetime.csv"))
# --------------------------------------------------------------




# ---- Optional trend sensitivity (not run by default) ----


RUN_TREND_SENS <- FALSE  # set TRUE when you want to run/export the alt time forms

if (RUN_TREND_SENS) {
  fit_pr_curr_time_cat <- survey::svyglm(
    any_binge_cdc ~ factor(year) + sex_f + age_group + race4 + educ4 + factor(state),
    design = des_curr, family = quasipoisson(link = "log")
  )
  fit_pr_curr_time_ns3 <- survey::svyglm(
    any_binge_cdc ~ splines::ns(year_lin, df = 3) + sex_f + age_group + race4 + educ4 + factor(state),
    design = des_curr, family = quasipoisson(link = "log")
  )
}
  # Uncomment if you want files:
  # readr::write_csv(mk_pr_table(fit_pr_curr_time_cat),
  #   file.path(tab_dir, "adj_PR_currentdrinkers_timeCategorical.csv"))
  # readr::write_csv(mk_pr_table(fit_pr_curr_time_ns3),
  #   file.path(tab_dir, "adj_PR_currentdrinkers_timeSpline_df3.csv"))

  # ===== Export time-spec sensitivity models =====
  #readr::write_csv(mk_pr_table(fit_pr_curr_time_cat),
    #                 file.path(tab_dir, "adj_PR_currentdrinkers_timeCategorical.csv"))
  #readr::write_csv(mk_pr_table(fit_pr_curr_time_ns3),
  #                 file.path(tab_dir, "adj_PR_currentdrinkers_timeSpline_df3.csv"))


# ---- Sensitivity: Region FE instead of State FE (Step 16b) ----
fit_pr_curr_region <- survey::svyglm(
  any_binge_cdc ~ year_lin + sex_f + age7 + race6_nat + educ4 + income6 + employ3 + marital3 + insured + factor(region),
  design = des_curr,
  family = quasipoisson(link = "log")
)
pr_curr_region <- mk_pr_table(fit_pr_curr_region)
readr::write_csv(pr_curr_region, file.path(tab_dir, "adj_PR_currentdrinkers_linetime_REGION_FE.csv"))


# ---- Secondary outcome: High-intensity binge (PRs), current drinkers ----
fit_hi_curr <- survey::svyglm(
  hi_binge ~ year_lin + sex_f + age7  + race6_nat  + educ4 +
    income6  + employ3 + marital3  + insured + factor(state),
  design = des_curr,
  family = quasipoisson(link = "log")
)

hi_pr <- mk_pr_table(fit_hi_curr)
readr::write_csv(hi_pr, file.path(tab_dir, "adj_PR_currentdrinkers_highIntensity.csv"))

# ---- Secondary outcome: Binge episode frequency (RR per drinking day) ----
des_curr_freq <- subset(des_curr, !is.na(binge_episodes) & !is.na(drink_days) & drink_days > 0)

fit_freq <- survey::svyglm(
  binge_episodes ~ year_lin + sex_f + age7  + race6_nat  + educ4 + income6  + employ3 + marital3  + insured + factor(state),
  design = des_curr_freq,
  family = quasipoisson(link = "log"),
  offset = log(drink_days)
)

freq_rr <- mk_pr_table(fit_freq)  # exp(coef) are rate ratios
readr::write_csv(freq_rr, file.path(tab_dir, "adj_RR_currentdrinkers_bingeFrequency_perDrinkDay.csv"))



# ---- Effect modification (Year × Sex), current drinkers ----
fit_pr_curr_int_sex <- survey::svyglm(any_binge_cdc ~ year_lin * sex_f + age7 + race6_nat + educ4 + income6 + employ3 + marital3 + insured + factor(state),
  design = des_curr,
  family = quasipoisson(link = "log")
)

# Joint Wald test for interaction
wt_int_sex <- survey::regTermTest(fit_pr_curr_int_sex, ~ year_lin:sex_f)
int_sex_test <- tibble::tibble(
  interaction = "Year×Sex",
  chisq = unname(wt_int_sex$test),
  df    = unname(wt_int_sex$df),
  p     = unname(wt_int_sex$p)
)
readr::write_csv(int_sex_test, file.path(tab_dir, "adj_PR_currentdrinkers_interaction_YearBySex_jointWald.csv"))

# Stratum-specific *annual* PRs from the interaction model
co  <- stats::coef(fit_pr_curr_int_sex)
vc  <- stats::vcov(fit_pr_curr_int_sex)

# Female (reference): PR per +1 year = exp(beta_year)
b_f <- unname(co["year_lin"])
se_f <- sqrt(vc["year_lin","year_lin"])
pr_f <- tibble::tibble(
  stratum = "Female",
  PR_per_year = exp(b_f),
  LCL = exp(b_f - 1.96*se_f),
  UCL = exp(b_f + 1.96*se_f)
)

# Male: PR per +1 year = exp(beta_year + beta_year:sex_fMale)
b_m <- unname(co["year_lin"]) + unname(co["year_lin:sex_fMale"])
se_m <- sqrt(vc["year_lin","year_lin"] + vc["year_lin:sex_fMale","year_lin:sex_fMale"] +
               2*vc["year_lin","year_lin:sex_fMale"])
pr_m <- tibble::tibble(
  stratum = "Male",
  PR_per_year = exp(b_m),
  LCL = exp(b_m - 1.96*se_m),
  UCL = exp(b_m + 1.96*se_m)
)

readr::write_csv(dplyr::bind_rows(pr_f, pr_m),
                 file.path(tab_dir, "adj_PR_currentdrinkers_TrendBySex_annualPR.csv"))


# ===== Model diagnostics: dispersion (phi) and DF =====
diag_quasi <- function(fit, label){
  rp <- residuals(fit, type="pearson")
  df <- fit$df.residual
  phi <- sum(rp^2, na.rm=TRUE)/df
  tibble::tibble(model = label,
                 df_resid = df,
                 phi_pearson = phi,
                 family = paste0(class(fit$family)[1], " (", fit$family$family, ", link=", fit$family$link, ")"))
}
diag_tab <- dplyr::bind_rows(
  diag_quasi(fit_pr_curr_lin,       "PR current, linear year"),
  diag_quasi(fit_pr_all_lin,        "PR all, linear year"),
  diag_quasi(fit_hi_curr,           "High-intensity PR"),
  diag_quasi(fit_freq,              "Binge frequency RR (offset)"),
  diag_quasi(fit_pr_curr_int_sex,   "PR current, Year×Sex")
)
readr::write_csv(diag_tab, file.path(tab_dir, "model_diagnostics_phi.csv"))




# ===== B) All-adult prevalence by year (percent, with 95% CI) =====
prev_all_prop <- survey::svyby(
  ~ any_binge_cdc, ~ year, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    year = as.integer(year),
    est  = any_binge_cdc,
    lcl  = ci_l,
    ucl  = ci_u
  )

prev_all_pct <- prev_all_prop %>%
  dplyr::mutate(
    est = est*100, lcl = lcl*100, ucl = ucl*100,
    ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
  )

readr::write_csv(prev_all_prop, file.path(tab_dir, "main_prev_any_binge_alladults_CI_prop.csv"))
readr::write_csv(prev_all_pct,  file.path(tab_dir, "main_prev_any_binge_alladults_CI_pct.csv"))

# ===== C) Current drinkers prevalence by year (percent, with 95% CI) =====
des_curr <- subset(des_all, is_current)

prev_curr_prop <- survey::svyby(
  ~ any_binge_cdc, ~ year, des_curr, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    year = as.integer(year),
    est  = any_binge_cdc,
    lcl  = ci_l,
    ucl  = ci_u
  )

prev_curr_pct <- prev_curr_prop %>%
  dplyr::mutate(
    est = est*100, lcl = lcl*100, ucl = ucl*100,
    ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
  )

readr::write_csv(prev_curr_prop, file.path(tab_dir, "main_prev_any_binge_current_CI_prop.csv"))
readr::write_csv(prev_curr_pct,  file.path(tab_dir, "main_prev_any_binge_current_CI_pct.csv"))

# ===== QC: quick table for a few anchor years (manual cross-check) =====
qc_years <- c(2011, 2015, 2019, 2021, max(prev_all_prop$year, na.rm=TRUE))
qc_tab <- prev_all_prop %>%
  dplyr::filter(year %in% qc_years) %>%
  dplyr::mutate(
    est_pct = est*100, lcl_pct = lcl*100, ucl_pct = ucl*100,
    ci = sprintf("%.1f (%.1f–%.1f)", est_pct, lcl_pct, ucl_pct)
  ) %>%
  dplyr::select(year, est_pct, ci)

readr::write_csv(qc_tab, file.path(tab_dir, "qc_prev_alladults_anchor_years.csv"))



# ===== D) By sex (all adults) =====
prev_sex_all <- survey::svyby(
  ~ any_binge_cdc, ~ year + sex_f, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    sex = as.character(sex_f),
    year = as.integer(year),
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  )


readr::write_csv(prev_sex_all, file.path(tab_dir, "main_prev_any_binge_alladults_by_sex_CI_prop.csv"))

# ===== E1) By age group (10-bin, all adults) =====
prev_age10_all <- survey::svyby(
  ~ any_binge_cdc, ~ year + age_group, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    age_group10 = as.character(age_group),
    year  = as.integer(year),
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  )
readr::write_csv(prev_age10_all, file.path(tab_dir, "main_prev_any_binge_alladults_by_age10_CI_prop.csv"))

# ===== E2) By age group (5-bin fallback, all adults) =====
prev_age5_all <- survey::svyby(
  ~ any_binge_cdc, ~ year + age_fallback, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    age_group5 = as.character(age_fallback),
    year  = as.integer(year),
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  )
readr::write_csv(prev_age5_all, file.path(tab_dir, "main_prev_any_binge_alladults_by_age5_CI_prop.csv"))

# ===== F) Side-by-side (All adults vs Current) for quick plots/tables =====
both_prop <- dplyr::bind_rows(
  prev_all_prop  %>% dplyr::mutate(denom = "All adults"),
  prev_curr_prop %>% dplyr::mutate(denom = "Current drinkers")
) %>% dplyr::arrange(denom, year)

readr::write_csv(both_prop, file.path(tab_dir, "main_prev_any_binge_both_denoms_CI_prop.csv"))

# ===== G) Year-to-year deltas (adjacent pairs) for both denominators =====
mk_pairs <- function(df_ci) {
  df_se <- df_ci %>% dplyr::mutate(se = (ucl - lcl) / (2*1.96)) %>% dplyr::select(year, est, se)
  yrs   <- sort(unique(df_se$year))
  pairs <- tibble::tibble(year1 = yrs[-length(yrs)], year2 = yrs[-1])
  pairs %>%
    dplyr::left_join(df_se, by = c("year1"="year")) %>% dplyr::rename(est1=est, se1=se) %>%
    dplyr::left_join(df_se, by = c("year2"="year")) %>% dplyr::rename(est2=est, se2=se) %>%
    dplyr::mutate(
      diff_pp = (est2 - est1)*100,
      sed     = sqrt(se1^2 + se2^2),
      z       = (est2 - est1)/sed,
      p_two   = 2*(1 - pnorm(abs(z)))
    ) %>%
    dplyr::select(year1, year2, diff_pp, z, p_two)
}

diff_all_years  <- mk_pairs(prev_all_prop)  %>% dplyr::mutate(denom = "All adults", .before=1)
diff_curr_years <- mk_pairs(prev_curr_prop) %>% dplyr::mutate(denom = "Current drinkers", .before=1)

diff_both_years <- dplyr::bind_rows(diff_all_years, diff_curr_years) %>%
  dplyr::arrange(denom, year1, year2)

# ---- Step 9a: BH FDR for overall deltas ----
diff_both_years <- diff_both_years %>%
  dplyr::group_by(denom) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()


readr::write_csv(diff_both_years, file.path(tab_dir, "main_prev_any_binge_adjacent_diffs.csv"))


# ===== H) By race/ethnicity (all adults) =====
prev_race_all <- survey::svyby(
  ~ any_binge_cdc, ~ year + race6_nat, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    race6_nat = as.character(race6_nat),
    year = as.integer(year),
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(race6_nat, year)

readr::write_csv(prev_race_all, file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_CI_prop.csv"))
readr::write_csv(prev_race_all %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100)),
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_CI_pct.csv"))

# ===== I) By race/ethnicity among current drinkers =====
prev_race_curr <- survey::svyby(
  ~ any_binge_cdc, ~ year + race6_nat, des_curr, survey::svymean, na.rm=TRUE, vartype=c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::transmute(
    race6_nat = as.character(race6_nat),
    year = as.integer(year),
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(race6_nat, year)


readr::write_csv(prev_race_curr, file.path(tab_dir, "main_prev_any_binge_current_by_race6_CI_prop.csv"))
readr::write_csv(prev_race_curr %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100)),
                 file.path(tab_dir, "main_prev_any_binge_current_by_race6_CI_pct.csv"))

# ===== Step 10) Weighted Table 1 & Rao–Scott tests =====
make_table1 <- function(des, by = c("sex_f","age_group","race6_nat","educ4")){
  tabs <- lapply(by, function(v){
    f <- stats::as.formula(paste0("~", v, " + any_binge_f"))
    out <- survey::svyby(~one, f, stats::update(des, one=1), survey::svytotal, na.rm=TRUE)
    out %>%
      dplyr::mutate(var = .data[[v]]) %>%
      dplyr::group_by(any_binge_f) %>%
      dplyr::mutate(pct = 100 * one / sum(one, na.rm=TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(variable = v, level = as.character(var),
                       binge = as.integer(any_binge_f), n_wt = one, pct = pct)
  })
  dplyr::bind_rows(tabs)
}


run_tests <- function(des, by = c("sex_f","age_group","race4","educ4")){
  lapply(by, function(v){
    f <- stats::as.formula(paste0("~", v, " + any_binge_f"))
    rs <- survey::svychisq(f, design = des, statistic = "F")
    tibble::tibble(variable = v,
                   Fstat = unname(rs$statistic),
                   df1   = unname(rs$parameter[1]),
                   df2   = unname(rs$parameter[2]),
                   p     = unname(rs$p.value))
  }) %>% dplyr::bind_rows()
}

tab1_all  <- make_table1(des_all,  by = c("sex_f","age7","race6_nat","educ4","income6","marital3"))
test_all  <- run_tests(des_all,    by = c("sex_f","age7","race6_nat","educ4","income6","marital3"))

tab1_curr <- make_table1(des_curr, by = c("sex_f","age7","race6_nat","educ4","income6","marital3"))
test_curr <- run_tests(des_curr,   by = c("sex_f","age7","race6_nat","educ4","income6","marital3"))


readr::write_csv(tab1_all,  file.path(tab_dir, "table1_alladults_weighted.csv"))
readr::write_csv(test_all,  file.path(tab_dir, "table1_alladults_tests.csv"))
readr::write_csv(tab1_curr, file.path(tab_dir, "table1_currentdrinkers_weighted.csv"))
readr::write_csv(test_curr, file.path(tab_dir, "table1_currentdrinkers_tests.csv"))


# ===== J) Adjacent year deltas within each race group (both denominators) =====
mk_pairs_by <- function(df_ci, group_var){
  gname <- rlang::as_name(rlang::ensym(group_var))
  df_ci <- dplyr::mutate(df_ci, se = (ucl - lcl)/(2*1.96))
  dplyr::bind_rows(lapply(split(df_ci, df_ci[[gname]]), function(dd){
    yrs <- sort(unique(dd$year))
    if (length(yrs) < 2) return(tibble::tibble())
    pairs <- tibble::tibble(year1 = yrs[-length(yrs)], year2 = yrs[-1])
    dd2 <- dd %>% dplyr::select(year, est, se)
    pairs %>%
      dplyr::left_join(dd2, by = c("year1"="year")) %>% dplyr::rename(est1=est, se1=se) %>%
      dplyr::left_join(dd2, by = c("year2"="year")) %>% dplyr::rename(est2=est, se2=se) %>%
      dplyr::mutate(
        diff_pp = (est2 - est1)*100,
        sed     = sqrt(se1^2 + se2^2),
        z       = (est2 - est1)/sed,
        p_two   = 2*(1 - pnorm(abs(z))),
        !!gname := unique(dd[[gname]])
      ) %>%
      dplyr::select(!!rlang::sym(gname), year1, year2, diff_pp, z, p_two)
  }))
}

race_diffs_all  <- mk_pairs_by(prev_race_all,  race6_nat) %>% dplyr::mutate(denom="All adults", .before=1)
race_diffs_curr <- mk_pairs_by(prev_race_curr, race6_nat) %>% dplyr::mutate(denom="Current drinkers", .before=1)

# ---- Step 9b: BH FDR for race deltas ----
race_diffs_all  <- race_diffs_all %>%
  dplyr::group_by(denom, race6_nat) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()

race_diffs_curr <- race_diffs_curr %>%
  dplyr::group_by(denom, race6_nat) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()


readr::write_csv(race_diffs_all,  file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_adjacent_diffs.csv"))
readr::write_csv(race_diffs_curr, file.path(tab_dir, "main_prev_any_binge_current_by_race6_adjacent_diffs.csv"))



# ===== STATES ANALYSIS (All adults + Current drinkers) =====
# Requires: df_all, des_all, des_curr, tab_dir



# ---- QC: ensure state present; keep numeric code for joins ----
if (!"state" %in% names(df_all)) {
  stop("Variable 'state' is missing. Add it in read_year_min() per the tiny patch.")
}

# (A) State × Year prevalence — All adults
state_prev_all_prop <- survey::svyby(
  ~ any_binge_cdc, ~ year + state, des_all, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    state_code = to_int(state),
    year = as.integer(year)
  ) %>%
  dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::transmute(
    year, state_code, state_abbr, state_name,
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(state_code, year)

readr::write_csv(state_prev_all_prop, file.path(tab_dir, "main_state_prev_any_binge_alladults_CI_prop.csv"))
readr::write_csv(
  state_prev_all_prop %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                                        ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
  file.path(tab_dir, "main_state_prev_any_binge_alladults_CI_pct.csv")
)

# (B) State × Year prevalence — Current drinkers
state_prev_curr_prop <- survey::svyby(
  ~ any_binge_cdc, ~ year + state, des_curr, survey::svymean,
  na.rm = TRUE, vartype = c("ci")
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    state_code = to_int(state),
    year = as.integer(year)
  ) %>%
  dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::transmute(
    year, state_code, state_abbr, state_name,
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(state_code, year)

readr::write_csv(state_prev_curr_prop, file.path(tab_dir, "main_state_prev_any_binge_current_CI_prop.csv"))
readr::write_csv(
  state_prev_curr_prop %>% dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~ .x*100),
                                         ci = sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
  file.path(tab_dir, "main_state_prev_any_binge_current_CI_pct.csv")
)

# ===== Small-N suppression for state-year cells =====
MIN_N <- 50  # adjust as needed

# Unweighted state-year counts (ALL)
df_all <- df_all %>%
  dplyr::mutate(state_code = to_int(state)) %>%
  dplyr::filter(!is.na(state_code), !(state_code %in% territories)) %>%
  dplyr::select(-state_code)


# Unweighted counts among CURRENT drinkers
df_counts_curr <- df_all %>%
  dplyr::mutate(
    is_current = dplyr::case_when(
      is.na(alcday5) ~ NA,
      alcday5 %in% c(777, 999) ~ NA,
      alcday5 == 888 ~ FALSE,
      TRUE ~ TRUE
    ),
    state_code = to_int(state)
  ) %>%
  dplyr::group_by(year, state_code) %>%
  dplyr::summarise(n_unw_curr = sum(is_current %in% TRUE, na.rm = TRUE), .groups = "drop")

df_counts_all <- df_all %>%
  dplyr::mutate(state_code = to_int(state)) %>%
  dplyr::group_by(year, state_code) %>%
  dplyr::summarise(n_unw = dplyr::n(), .groups = "drop")

suppress_fn <- function(df_est, ncol, new_suffix){
  df_est %>%
    dplyr::left_join(df_counts_all, by = c("year", "state_code")) %>%
    dplyr::mutate(across(c(est,lcl,ucl), ~ ifelse(!is.na(n_unw) & n_unw < MIN_N, NA_real_, .))) %>%
    dplyr::rename(!!ncol := n_unw) %>%
    { readr::write_csv(., file.path(tab_dir, paste0(new_suffix, ".csv"))); . }
}

suppress_fn_curr <- function(df_est, ncol, new_suffix){
  df_est %>%
    dplyr::left_join(df_counts_curr, by = c("year", "state_code")) %>%
    dplyr::mutate(across(c(est,lcl,ucl), ~ ifelse(!is.na(n_unw_curr) & n_unw_curr < MIN_N, NA_real_, .))) %>%
    dplyr::rename(!!ncol := n_unw_curr) %>%
    { readr::write_csv(., file.path(tab_dir, paste0(new_suffix, ".csv"))); . }
}

# Write suppressed versions (keep originals too)
suppress_fn(state_prev_all_prop,  "n_unw",      "main_state_prev_any_binge_alladults_CI_prop_suppressed_MIN50")
suppress_fn_curr(state_prev_curr_prop, "n_unw_curr", "main_state_prev_any_binge_current_CI_prop_suppressed_MIN50")



# (C) Adjacent year deltas within each state (both denoms)
mk_pairs_by_state <- function(df_ci, group_var){
  gname <- rlang::as_name(rlang::ensym(group_var))
  df_ci <- dplyr::mutate(df_ci, se = (ucl - lcl)/(2*1.96))
  dplyr::bind_rows(lapply(split(df_ci, df_ci[[gname]]), function(dd){
    yrs <- sort(unique(dd$year))
    if (length(yrs) < 2) return(tibble::tibble())
    pairs <- tibble::tibble(year1 = yrs[-length(yrs)], year2 = yrs[-1])
    dd2 <- dd %>% dplyr::select(year, est, se)
    pairs %>%
      dplyr::left_join(dd2, by = c("year1"="year")) %>% dplyr::rename(est1=est, se1=se) %>%
      dplyr::left_join(dd2, by = c("year2"="year")) %>% dplyr::rename(est2=est, se2=se) %>%
      dplyr::mutate(
        diff_pp = (est2 - est1)*100,
        sed     = sqrt(se1^2 + se2^2),
        z       = (est2 - est1)/sed,
        p_two   = 2*(1 - pnorm(abs(z))),
        !!gname := unique(dd[[gname]])
      ) %>%
      dplyr::select(!!rlang::sym(gname), year1, year2, diff_pp, z, p_two)
  }))
}


state_diffs_all  <- mk_pairs_by_state(
  state_prev_all_prop %>% dplyr::select(state_code, year, est, lcl, ucl),
  state_code
) %>% dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::mutate(denom = "All adults", .before = 1)

state_diffs_curr <- mk_pairs_by_state(
  state_prev_curr_prop %>% dplyr::select(state_code, year, est, lcl, ucl),
  state_code
) %>% dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::mutate(denom = "Current drinkers", .before = 1)


# ---- Step 9c: BH FDR for state deltas ----
state_diffs_all  <- state_diffs_all %>%
  dplyr::group_by(denom, state_code) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()

state_diffs_curr <- state_diffs_curr %>%
  dplyr::group_by(denom, state_code) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()


readr::write_csv(state_diffs_all,  file.path(tab_dir, "main_state_adjacent_diffs_alladults.csv"))
readr::write_csv(state_diffs_curr, file.path(tab_dir, "main_state_adjacent_diffs_current.csv"))

# (D) Trend tests by state on PR scale (per +1 year)
trend_by_state_PR <- function(des, label){
  ids <- sort(unique(to_int(des$variables$state)))
  do_fit <- function(sc){
    dsub <- subset(des, to_int(state) == sc)
    tryCatch({
      dsub$variables$year_lin <- as.numeric(dsub$variables$year) - 2011
      fit <- survey::svyglm(any_binge_cdc ~ year_lin, design = dsub, family = quasipoisson(link = "log"))
      co  <- summary(fit)$coefficients["year_lin", ]
      tibble::tibble(
        state_code   = sc,
        slope_log    = unname(co["Estimate"]),
        PR_per_year  = exp(unname(co["Estimate"])),
        se           = unname(co["Std. Error"]),
        z            = unname(co["Estimate"]/co["Std. Error"]),
        p_two        = 2*(1 - pnorm(abs(z)))
      )
    }, error = function(e){
      tibble::tibble(state_code = sc, slope_log = NA_real_, PR_per_year = NA_real_, se = NA_real_, z = NA_real_, p_two = NA_real_)
    })
  }
  out <- if (exists("USE_PARALLEL") && isTRUE(USE_PARALLEL)) {
    furrr::future_map_dfr(ids, do_fit, .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map_dfr(ids, do_fit)
  }
  out %>%
    dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
    dplyr::mutate(denom = label, .before = 1)
}

state_trend_all_PR  <- trend_by_state_PR(des_all,  "All adults")
state_trend_curr_PR <- trend_by_state_PR(des_curr, "Current drinkers")

readr::write_csv(state_trend_all_PR,  file.path(tab_dir, "main_state_trends_alladults_PR.csv"))
readr::write_csv(state_trend_curr_PR, file.path(tab_dir, "main_state_trends_current_PR.csv"))

# (E) Optional: per-year state ranks (use % scale for quick reporting)
rank_states <- function(df_prop){
  df_prop %>%
    dplyr::mutate(est_pct = est*100) %>%
    dplyr::group_by(year) %>%
    dplyr::arrange(year, dplyr::desc(est_pct)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, state_code, state_abbr, state_name, est_pct, rank)
}

state_rank_all  <- rank_states(state_prev_all_prop)
state_rank_curr <- rank_states(state_prev_curr_prop)

readr::write_csv(state_rank_all,  file.path(tab_dir, "main_state_ranks_alladults_pct.csv"))
readr::write_csv(state_rank_curr, file.path(tab_dir, "main_state_ranks_current_pct.csv"))

# ===== Step 11) Sensitivity: including territories (overall prevalence by year) =====

CACHE_DIR <- file.path(PROJ_DIR, "data/cache")

cache_files <- list.files(
  CACHE_DIR,
  pattern = paste0("^BRFSS_\\d{4}_", CACHE_VER, "\\.rds$"),
  full.names = TRUE
)
dlist_incl <- lapply(cache_files, readRDS)

df_all_incl <- dplyr::bind_rows(dlist_incl) %>%
  dplyr::mutate(
    strata = interaction(year, strata, drop = TRUE, lex.order = TRUE),
    psu    = interaction(year, psu,    drop = TRUE, lex.order = TRUE)
  ) %>%
  dplyr::filter(!is.na(sex), !is.na(age_group))


des_all_incl <- survey::svydesign(id=~psu, strata=~strata, weights=~wt,
                                  data = df_all_incl, nest = TRUE)
des_all_incl <- stats::update(
  des_all_incl,
  is_current = ifelse(is.na(alcday5) | alcday5 %in% c(777,999), NA, alcday5 != 888)
)

prev_statesDC <- survey::svyby(~any_binge_cdc, ~year, des_all,       survey::svymean, na.rm=TRUE) %>%
  dplyr::transmute(year = as.integer(year), est_statesDC = any_binge_cdc)
prev_incl     <- survey::svyby(~any_binge_cdc, ~year, des_all_incl,  survey::svymean, na.rm=TRUE) %>%
  dplyr::transmute(year = as.integer(year), est_incl = any_binge_cdc)

sens_overall <- dplyr::left_join(prev_statesDC, prev_incl, by="year") %>%
  dplyr::mutate(diff_pp = (est_incl - est_statesDC)*100) %>%
  dplyr::arrange(year)

readr::write_csv(sens_overall, file.path(tab_dir, "sensitivity_territories_overall_prev_by_year.csv"))

