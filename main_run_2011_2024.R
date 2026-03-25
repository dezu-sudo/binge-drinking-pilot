# ===== 0) Project paths (portable) =====

PROJ_DIR <- tryCatch(
  normalizePath("C:/Users/Anigma PC/Desktop/Binge Drinking Systematic Review/binge-drinking-pilot",
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
      
      
      # ---------- NEW: Harmonized race/ethnicity ----------
      imprace_i = as_int(imprace_raw),
      race1_i   = as_int(race1_raw),
      mrace1_i = as_int(mrace1_raw),
      mrace2_i = as_int(mrace2_raw),
      hisp_i    = as_int(hisp_raw),
      
      race4 = dplyr::case_when(
        # Hispanic FIRST — always comes from hispanc/hispanc2, not imprace
        !is.na(hisp_i) & hisp_i == 1L ~ "Hispanic",
        # Then imprace for NH categories (codes 1-6, no 8)
        !is.na(imprace_i) & imprace_i == 1L ~ "NH White",
        !is.na(imprace_i) & imprace_i == 2L ~ "NH Black",
        !is.na(imprace_i) & imprace_i %in% c(3L,4L,5L,6L,7L) ~ "NH Other",
        # fallback to RACE1 when imprace missing
        is.na(imprace_i) & !is.na(race1_i) & race1_i == 1L ~ "NH White",
        is.na(imprace_i) & !is.na(race1_i) & race1_i == 2L ~ "NH Black",
        is.na(imprace_i) & !is.na(race1_i) & race1_i %in% c(3L,4L,5L,6L,7L) ~ "NH Other",
        # fallback to mrace2/mrace1
        is.na(imprace_i) & is.na(race1_i) & !is.na(mrace2_i) & mrace2_i == 10L ~ "NH White",
        is.na(imprace_i) & is.na(race1_i) & !is.na(mrace2_i) & mrace2_i == 20L ~ "NH Black",
        is.na(imprace_i) & is.na(race1_i) & !is.na(mrace2_i) & mrace2_i %in% c(30L,40L,50L,51L,52L,53L,54L,60L,70L) ~ "NH Other",
        TRUE ~ NA_character_
      ),
      
      # ---------- NEW: Detailed race for national reporting (6 levels) ----------
      race6_nat = dplyr::case_when(
        !is.na(hisp_i) & hisp_i == 1L ~ "Hispanic",
        !is.na(imprace_i) & imprace_i == 1L ~ "NH White",
        !is.na(imprace_i) & imprace_i == 2L ~ "NH Black",
        !is.na(imprace_i) & imprace_i == 3L ~ "NH AI/AN",
        !is.na(imprace_i) & imprace_i %in% c(4L,5L) ~ "NH Asian/NHPI",
        !is.na(imprace_i) & imprace_i %in% c(6L,7L) ~ "NH Other/Multiracial",
        # mrace1 BEFORE race1 — gives detailed categories for 2015-2016
        is.na(imprace_i) & !is.na(mrace1_i) & mrace1_i == 1L ~ "NH White",
        is.na(imprace_i) & !is.na(mrace1_i) & mrace1_i == 2L ~ "NH Black",
        is.na(imprace_i) & !is.na(mrace1_i) & mrace1_i == 3L ~ "NH AI/AN",
        is.na(imprace_i) & !is.na(mrace1_i) & mrace1_i %in% c(4L,5L) ~ "NH Asian/NHPI",
        is.na(imprace_i) & !is.na(mrace1_i) & mrace1_i %in% c(6L,7L) ~ "NH Other/Multiracial",
        # race1/racegr3 last resort
        !is.na(race1_i) & race1_i == 1L ~ "NH White",
        !is.na(race1_i) & race1_i == 2L ~ "NH Black",
        !is.na(mrace2_i) & mrace2_i == 30L ~ "NH AI/AN",
        !is.na(mrace2_i) & mrace2_i %in% c(40L,50L,51L,52L,53L,54L) ~ "NH Asian/NHPI",
        !is.na(mrace2_i) & mrace2_i %in% c(60L,70L) ~ "NH Other/Multiracial",
        TRUE ~ NA_character_
      )
      
    
    ) %>%
    select(
      year, wt, strata, psu, state, sex,
      age5, age_group, age_fallback,
      educ4, race4, race6_nat,
      income3, income6, income_hi4,
      employ3, marital2, marital3, insured,
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
  nm_mrace1 <- find_var(df, c("_mrace1","mrace1"))  
  nm_mrace2 <- find_var(df, c("_mrace2","mrace2"))  
  nm_hisp   <- find_var(df, c("hispanc","hispanic","hispan2","hispan1","_hispanc","hispanc2"))
  
  
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
    mrace1_raw = if (!is.na(nm_mrace1)) .data[[nm_mrace1]] else NA,
    mrace2_raw = if (!is.na(nm_mrace2)) .data[[nm_mrace2]] else NA,
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

CACHE_VER <- "v2026-01-16_popTrend_racefix2.1"

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

# ===== Outcome variable source documentation =====
# Documents which binge flag variable was used per year
# Required for supplementary methods in publication

binge_source_doc <- purrr::map2_dfr(
  as.integer(names(YEAR_PATHS)), YEAR_PATHS,
  function(yr, path) {
    nms <- names(haven::read_xpt(path, n_max = 1)) %>%
      janitor::make_clean_names()
    
    rfbing  <- find_var(
      setNames(as.list(nms), nms),
      c("_rfbing6","rfbing6","_rfbing5","rfbing5","_rfbing4","rfbing4")
    )
    drnk    <- find_var(
      setNames(as.list(nms), nms),
      c("drnk3ge5","_drnk3ge5")
    )
    
    tibble::tibble(
      year            = yr,
      rfbing_var      = ifelse(is.na(rfbing), "NOT FOUND", rfbing),
      drnk3ge5_var    = ifelse(is.na(drnk),   "NOT FOUND", drnk),
      primary_source  = ifelse(!is.na(rfbing), rfbing, "fallback: drnk3ge5"),
      uses_fallback   = is.na(rfbing)
    )
  }
)

readr::write_csv(
  binge_source_doc,
  file.path(PROJ_DIR, "docs/binge_outcome_source_by_year.csv")
)

# Print to console for immediate review
print(binge_source_doc, n = Inf)

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

# ---- helper: force complete 51-jurisdiction state-year panel ----
pad_state_panel <- function(df, years = 2011:2024) {
  panel_51 <- tidyr::expand_grid(
    year = as.integer(years),
    state_code = sort(unique(state_xwalk$fips[!(state_xwalk$fips %in% territories)]))
  ) %>%
    dplyr::left_join(
      state_xwalk %>%
        dplyr::filter(!(fips %in% territories)) %>%
        dplyr::select(fips, state_abbr, state_name),
      by = c("state_code" = "fips")
    )
  
  panel_51 %>%
    dplyr::left_join(df, by = c("year", "state_code", "state_abbr", "state_name")) %>%
    dplyr::arrange(state_code, year)
}

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

df_all <- df_all %>%
  mutate(
    strata_id = as.integer(factor(strata)),
    psu_id    = as.integer(factor(psu))
  )

# 2) Rebuild the survey design objects that depend on age_group
des_all <- survey::svydesign(id = ~psu_id, strata = ~strata_id, weights = ~wt, data = df_all, nest = TRUE)

des_all <- stats::update(
  des_all,
  alcday5 = suppressWarnings(as.integer(haven::zap_labels(alcday5))),
  is_current = dplyr::case_when(
    is.na(alcday5) ~ NA,
    alcday5 %in% c(777, 999) ~ NA,
    alcday5 == 888 ~ FALSE,
    TRUE ~ TRUE
  ),
  year_lin   = as.numeric(year) - 2011,
  drink_days = alcday5_to_days30(alcday5),
  hi_binge = dplyr::case_when(
    is.na(max_drinks) | is.na(sex) ~ NA_integer_,
    as.integer(sex) == 2L & max_drinks >= 8  ~ 1L,
    as.integer(sex) == 1L & max_drinks >= 10 ~ 1L,
    as.integer(sex) == 2L & max_drinks < 8   ~ 0L,
    as.integer(sex) == 1L & max_drinks < 10  ~ 0L,
    TRUE ~ NA_integer_
  ),
  sex_f      = factor(as.integer(sex), levels = c(2,1), labels = c("Female","Male")),
  race4      = factor(race4, levels = c("NH White","NH Black","Hispanic","NH Other")),
  race6_nat  = factor(race6_nat, levels = c("NH White","NH Black","Hispanic","NH Asian/NHPI","NH AI/AN","NH Other/Multiracial")),
  educ4      = factor(educ4, levels = c("<HS","HS/GED","Some college/AA","College"), ordered = TRUE),
  income3    = factor(income3, levels = c("<$25k","$25–<50k","≥$50k","Unknown"), ordered = TRUE),
  income6    = factor(income6, levels = c("<$15k","$15–<25k","$25–<35k","$35–<50k","$50–<75k","≥$75k","Unknown"), ordered = TRUE),
  employ3    = factor(employ3, levels = c("Employed","Unemployed","NILF","Unknown")),
  marital2   = factor(marital2, levels = c("Married/Partnered","Not married","Unknown")),
  marital3   = factor(marital3, levels = c("Married/Partnered","Never married","Previously married","Unknown")),
  insured    = factor(insured, levels = c("Yes","No","Unknown")),
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

# ===== Step 13) CDC binge flag coding check=====
chk <- purrr::map_dfr(sort(unique(df_all$year)), function(y){
  d <- dplyr::filter(df_all, year == y)
  tab <- d %>%
    dplyr::mutate(rf = suppressWarnings(as.integer(rfbing5))) %>%
    dplyr::filter(!is.na(rf) & !is.na(any_binge_cdc)) %>%
    dplyr::count(rf, any_binge_cdc)
  mis <- tab %>%
    dplyr::filter(!(rf==1 & any_binge_cdc==0) & !(rf==2 & any_binge_cdc==1)) %>%
    dplyr::summarise(mis = sum(n), .groups="drop") %>%
    dplyr::pull(mis)
  tibble::tibble(year = y, mismatches = ifelse(length(mis)==0, 0L, mis))
})
readr::write_csv(chk, file.path(tab_dir, "qc_cdc_binge_flag_mismatches_by_year.csv"))
message("CDC binge flag check complete. See qc_cdc_binge_flag_mismatches_by_year.csv")

# Align to the data’s factor level order
std2000_adult10 <- std2000_adult10[levels(df_all$age_group)]


# Optional safety check
stopifnot(identical(names(std2000_adult10), levels(df_all$age_group)),
          all(!is.na(std2000_adult10)))


# Before running svyby, multi-core processing for speed
library(parallel)
options(survey.multicore = TRUE)

rm(dlist, df_all, bad, aud, exists_vec, yrs, paths)
gc()


# ===== Age-adjusted prevalence (Year 2000 standard) =====

# (1) All adults, by year (AGE-ADJUSTED, 7-band), looped by year

prev_all_prop_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age7))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    population = w_2000_age7_counts
  )
  
  out <- survey::svymean(
    ~any_binge_cdc,
    des_std,
    na.rm = TRUE
  )
  
  ci <- suppressWarnings(confint(out))
  
  tibble::tibble(
    year = as.integer(yr),
    est  = as.numeric(out)[1],
    lcl  = ci[1, 1],
    ucl  = ci[1, 2]
  )
}) %>%
  dplyr::arrange(year)

readr::write_csv(
  prev_all_prop_ageadj,
  file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  prev_all_prop_ageadj %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_alladults_AGEADJ2000_CI_pct.csv")
)


# (2) Current drinkers, by year (AGE-ADJUSTED, 7-band), looped by year

prev_curr_prop_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(age7))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    population = w_2000_age7_counts
  )
  
  out <- survey::svymean(
    ~any_binge_cdc,
    des_std,
    na.rm = TRUE
  )
  
  ci <- suppressWarnings(confint(out))
  
  tibble::tibble(
    year = as.integer(yr),
    est  = as.numeric(out)[1],
    lcl  = ci[1, 1],
    ucl  = ci[1, 2]
  )
}) %>%
  dplyr::arrange(year)

readr::write_csv(
  prev_curr_prop_ageadj,
  file.path(tab_dir, "main_prev_any_binge_current_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  prev_curr_prop_ageadj %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_current_AGEADJ2000_CI_pct.csv")
)  



# (3) By race/ethnicity (all adults)

prev_race_all_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(race6_nat))
  des_yr <- survey::svydesign(id = ~psu_id, strata = ~strata_id,
                              weights = ~wt, data = df_yr, nest = TRUE)
  des_std <- survey::svystandardize(des_yr, by = ~age7, over = ~race6_nat,
                                    population = w_2000_age7_counts)
  out <- survey::svyby(~any_binge_cdc, ~race6_nat, des_std, survey::svymean,
                       na.rm = TRUE, vartype = "ci") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  rm(df_yr, des_yr, des_std); gc()
  out
}) %>%
  dplyr::transmute(race6_nat = as.character(race6_nat), year = as.integer(year),
                   est = any_binge_cdc, lcl = ci_l, ucl = ci_u) %>%
  dplyr::arrange(race6_nat, year)

readr::write_csv(prev_race_all_ageadj,
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_AGEADJ2000_CI_prop.csv"))



# (4) By race/ethnicity among current drinkers
prev_race_curr_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(race6_nat))
  
  des_yr <- survey::svydesign(id = ~psu_id, strata = ~strata_id,
                              weights = ~wt, data = df_yr, nest = TRUE)
  
  des_std <- survey::svystandardize(des_yr, by = ~age7, over = ~race6_nat,
                                    population = w_2000_age7_counts)
  
  out <- survey::svyby(~any_binge_cdc, ~race6_nat, des_std, survey::svymean,
                       na.rm = TRUE, vartype = "ci") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std); gc()
  out
}) %>%
  dplyr::transmute(
    race6_nat = as.character(race6_nat),
    year = as.integer(year),
    est = any_binge_cdc, lcl = ci_l, ucl = ci_u
  ) %>%
  dplyr::arrange(race6_nat, year)

readr::write_csv(prev_race_curr_ageadj,
                 file.path(tab_dir, "main_prev_any_binge_current_by_race6_AGEADJ2000_CI_prop.csv"))


#########################################################################

# ===== Export for NCI Joinpoint Regression =====

# Overall all adults
readr::write_csv(
  prev_all_prop_ageadj %>%
    dplyr::mutate(
      se = (ucl - lcl) / (2 * 1.96)
    ) %>%
    dplyr::select(year, est, se),
  file.path(tab_dir, "joinpoint_input_alladults_overall.csv")
)

# Overall current drinkers
readr::write_csv(
  prev_curr_prop_ageadj %>%
    dplyr::mutate(
      se = (ucl - lcl) / (2 * 1.96)
    ) %>%
    dplyr::select(year, est, se),
  file.path(tab_dir, "joinpoint_input_current_overall.csv")
)

# By race — all adults (one file per race group, Joinpoint expects this)
prev_race_all_ageadj %>%
  dplyr::mutate(se = (ucl - lcl) / (2 * 1.96)) %>%
  dplyr::select(race6_nat, year, est, se) %>%
  dplyr::group_by(race6_nat) %>%
  dplyr::group_walk(~ readr::write_csv(
    .x %>% dplyr::select(year, est, se),
    file.path(tab_dir, paste0("joinpoint_input_alladults_",
                              gsub("[^A-Za-z0-9]", "_", .y$race6_nat), ".csv"))
  ))

# By race — current drinkers
prev_race_curr_ageadj %>%
  dplyr::mutate(se = (ucl - lcl) / (2 * 1.96)) %>%
  dplyr::select(race6_nat, year, est, se) %>%
  dplyr::group_by(race6_nat) %>%
  dplyr::group_walk(~ readr::write_csv(
    .x %>% dplyr::select(year, est, se),
    file.path(tab_dir, paste0("joinpoint_input_current_",
                              gsub("[^A-Za-z0-9]", "_", .y$race6_nat), ".csv"))
  ))

######################################################################################



# (5) States — All adults (AGE-ADJUSTED, 7-band), looped by year

state_prev_all_prop_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(state))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    over = ~state,
    population = w_2000_age7_counts
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~state,
    des_std,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std)
  gc()
  
  out
}) %>%
  dplyr::mutate(
    state_code = to_int(state),
    year = as.integer(year)
  ) %>%
  dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::transmute(
    year,
    state_code,
    state_abbr,
    state_name,
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  pad_state_panel() %>%
  dplyr::arrange(state_code, year)

readr::write_csv(
  state_prev_all_prop_ageadj,
  file.path(tab_dir, "main_state_prev_any_binge_alladults_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  state_prev_all_prop_ageadj %>%
    dplyr::mutate(
      dplyr::across(c(est, lcl, ucl), ~ .x * 100),
      ci = dplyr::if_else(
        is.na(est),
        NA_character_,
        sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
      )
    ),
  file.path(tab_dir, "main_state_prev_any_binge_alladults_AGEADJ2000_CI_pct.csv")
)

# optional QC file: which rows were padded
readr::write_csv(
  state_prev_all_prop_ageadj %>%
    dplyr::filter(is.na(est)) %>%
    dplyr::arrange(year, state_code),
  file.path(tab_dir, "qc_missing_state_years_alladults.csv")
)


# (6) States — Current drinkers (AGE-ADJUSTED, 7-band), looped by year

state_prev_curr_prop_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(state))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    over = ~state,
    population = w_2000_age7_counts
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~state,
    des_std,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std)
  gc()
  
  out
}) %>%
  dplyr::mutate(
    state_code = to_int(state),
    year = as.integer(year)
  ) %>%
  dplyr::left_join(state_xwalk, by = c("state_code" = "fips")) %>%
  dplyr::transmute(
    year,
    state_code,
    state_abbr,
    state_name,
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  pad_state_panel() %>%
  dplyr::arrange(state_code, year)

readr::write_csv(
  state_prev_curr_prop_ageadj,
  file.path(tab_dir, "main_state_prev_any_binge_current_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  state_prev_curr_prop_ageadj %>%
    dplyr::mutate(
      dplyr::across(c(est, lcl, ucl), ~ .x * 100),
      ci = dplyr::if_else(
        is.na(est),
        NA_character_,
        sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
      )
    ),
  file.path(tab_dir, "main_state_prev_any_binge_current_AGEADJ2000_CI_pct.csv")
)

# optional QC file: which rows were padded
readr::write_csv(
  state_prev_curr_prop_ageadj %>%
    dplyr::filter(is.na(est)) %>%
    dplyr::arrange(year, state_code),
  file.path(tab_dir, "qc_missing_state_years_current.csv")
)


library(dplyr)
library(tidyr)

# -------------------------------------------------
# ==== Generic preflight QC for prevalence code ====
# -------------------------------------------------
qc_prev_preflight <- function(des_obj,
                              subgroup,
                              design_name = "design",
                              years = 2011:2024,
                              age_adjust = FALSE,
                              expected_levels = NULL,
                              std_levels = c("18-29","30-39","40-49","50-59","60-69","70-79","80+")) {
  
  df <- des_obj$variables
  subgroup_sym <- rlang::ensym(subgroup)
  subgroup_chr <- rlang::as_string(subgroup_sym)
  
  cat("\n=============================\n")
  cat("QC preflight:", design_name, "| subgroup =", subgroup_chr, "\n")
  cat("=============================\n")
  
  # 1) Core variable existence
  needed <- c("year", "wt", "psu_id", "strata_id", "any_binge_cdc", subgroup_chr)
  if (age_adjust) needed <- c(needed, "age7")
  
  missing_vars <- setdiff(needed, names(df))
  if (length(missing_vars) > 0) {
    stop("Missing required variable(s): ", paste(missing_vars, collapse = ", "))
  }
  
  # 2) Core validity checks
  core_tab <- tibble::tibble(
    design = design_name,
    subgroup = subgroup_chr,
    n_rows = nrow(df),
    year_min = min(df$year, na.rm = TRUE),
    year_max = max(df$year, na.rm = TRUE),
    n_years = dplyr::n_distinct(df$year),
    bad_outcome = sum(!is.na(df$any_binge_cdc) & !(df$any_binge_cdc %in% c(0L, 1L))),
    bad_wt = sum(is.na(df$wt) | !is.finite(df$wt) | df$wt <= 0),
    bad_psu = sum(is.na(df$psu_id)),
    bad_strata = sum(is.na(df$strata_id)),
    missing_subgroup = sum(is.na(df[[subgroup_chr]])),
    missing_age7 = if (age_adjust) sum(is.na(df$age7)) else NA_integer_
  )
  
  print(core_tab)
  
  # 3) Age7 level check when needed
  if (age_adjust) {
    age7_levels <- levels(df$age7)
    cat("\nAge7 levels in data:\n")
    print(age7_levels)
    
    cat("\nAge7 levels missing from standard:\n")
    print(setdiff(age7_levels, std_levels))
    
    cat("\nStandard levels missing from data:\n")
    print(setdiff(std_levels, age7_levels))
  }
  
  # 4) Year-by-subgroup raw cell coverage
  df_nonmiss <- df %>%
    dplyr::filter(!is.na(.data[[subgroup_chr]]))
  
  year_level_counts <- df_nonmiss %>%
    dplyr::count(year, !!subgroup_sym, name = "n_unwtd") %>%
    dplyr::arrange(year, !!subgroup_sym)
  
  cat("\nFirst few year-by-subgroup counts:\n")
  print(utils::head(year_level_counts, 20))
  
  # 5) Expected panel completeness check
  if (is.null(expected_levels)) {
    if (is.factor(df[[subgroup_chr]])) {
      expected_levels <- levels(df[[subgroup_chr]])
    } else {
      expected_levels <- sort(unique(df_nonmiss[[subgroup_chr]]))
    }
  }
  
  expected_panel <- tidyr::expand_grid(
    year = years,
    subgroup_level = expected_levels
  )
  
  observed_panel <- df_nonmiss %>%
    dplyr::distinct(year, subgroup_level = .data[[subgroup_chr]])
  
  missing_cells <- expected_panel %>%
    dplyr::anti_join(observed_panel, by = c("year", "subgroup_level")) %>%
    dplyr::arrange(year, subgroup_level)
  
  cat("\nMissing subgroup-year cells:\n")
  print(missing_cells, n = Inf)
  
  # 6) Age-adjustment-specific coverage:
  # within each subgroup-year, do we have at least some nonmissing age7?
  if (age_adjust) {
    age_cover <- df %>%
      dplyr::filter(!is.na(.data[[subgroup_chr]])) %>%
      dplyr::group_by(year, !!subgroup_sym) %>%
      dplyr::summarise(
        n_unwtd = dplyr::n(),
        n_age7_nonmiss = sum(!is.na(age7)),
        n_age7_levels_present = dplyr::n_distinct(age7[!is.na(age7)]),
        .groups = "drop"
      ) %>%
      dplyr::arrange(year, !!subgroup_sym)
    
    cat("\nAge-adjustment coverage summary (first 20 rows):\n")
    print(utils::head(age_cover, 20))
    
    problem_age_cells <- age_cover %>%
      dplyr::filter(n_age7_nonmiss == 0)
    
    cat("\nSubgroup-year cells with ZERO usable age7 rows:\n")
    print(problem_age_cells, n = Inf)
  }
  
  invisible(list(
    core = core_tab,
    counts = year_level_counts,
    missing_cells = missing_cells
  ))
}

# Sex: age-adjusted
qc_prev_preflight(des_all,  sex_f,   "des_all",  age_adjust = TRUE)
qc_prev_preflight(des_curr, sex_f,   "des_curr", age_adjust = TRUE)

# Age group: NOT age-adjusted
qc_prev_preflight(des_all,  age_group, "des_all",  age_adjust = FALSE)
qc_prev_preflight(des_curr, age_group, "des_curr", age_adjust = FALSE)

# Education: age-adjusted
qc_prev_preflight(des_all,  educ4,   "des_all",  age_adjust = TRUE)
qc_prev_preflight(des_curr, educ4,   "des_curr", age_adjust = TRUE)


# (7) Sex — All adults (AGE-ADJUSTED, 7-band), looped by year

prev_sex_all_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(sex_f))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    over = ~sex_f,
    population = w_2000_age7_counts
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~sex_f,
    des_std,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std)
  gc()
  
  out
}) %>%
  dplyr::transmute(
    sex = as.character(sex_f),
    year = as.integer(year),
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  dplyr::arrange(sex, year)

readr::write_csv(
  prev_sex_all_ageadj,
  file.path(tab_dir, "main_prev_any_binge_alladults_by_sex_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  prev_sex_all_ageadj %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_alladults_by_sex_AGEADJ2000_CI_pct.csv")
)


# (8) Sex — Current drinkers (AGE-ADJUSTED, 7-band), looped by year

prev_sex_curr_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(sex_f))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    over = ~sex_f,
    population = w_2000_age7_counts
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~sex_f,
    des_std,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std)
  gc()
  
  out
}) %>%
  dplyr::transmute(
    sex = as.character(sex_f),
    year = as.integer(year),
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  dplyr::arrange(sex, year)

readr::write_csv(
  prev_sex_curr_ageadj,
  file.path(tab_dir, "main_prev_any_binge_current_by_sex_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  prev_sex_curr_ageadj %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_current_by_sex_AGEADJ2000_CI_pct.csv")
)

# (9) Age group — All adults (NOT age-adjusted), looped by year

prev_age_all <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age_group))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~age_group,
    des_yr,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr)
  gc()
  
  out
}) %>%
  dplyr::transmute(
    age_group = as.character(age_group),
    year = as.integer(year),
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(
  prev_age_all,
  file.path(tab_dir, "main_prev_any_binge_alladults_by_agegroup_CI_prop.csv")
)

readr::write_csv(
  prev_age_all %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_alladults_by_agegroup_CI_pct.csv")
)

# (10) Age group — Current drinkers (NOT age-adjusted), looped by year

prev_age_curr <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(age_group))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~age_group,
    des_yr,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr)
  gc()
  
  out
}) %>%
  dplyr::transmute(
    age_group = as.character(age_group),
    year = as.integer(year),
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  dplyr::arrange(age_group, year)

readr::write_csv(
  prev_age_curr,
  file.path(tab_dir, "main_prev_any_binge_current_by_agegroup_CI_prop.csv")
)

readr::write_csv(
  prev_age_curr %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_current_by_agegroup_CI_pct.csv")
)

# (11) Education — All adults (AGE-ADJUSTED, 7-band), looped by year

prev_educ_all_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(educ4))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    over = ~educ4,
    population = w_2000_age7_counts
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~educ4,
    des_std,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std)
  gc()
  
  out
}) %>%
  dplyr::transmute(
    educ4 = as.character(educ4),
    year = as.integer(year),
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  dplyr::arrange(educ4, year)

readr::write_csv(
  prev_educ_all_ageadj,
  file.path(tab_dir, "main_prev_any_binge_alladults_by_educ4_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  prev_educ_all_ageadj %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_alladults_by_educ4_AGEADJ2000_CI_pct.csv")
)

# (12) Education — Current drinkers (AGE-ADJUSTED, 7-band), looped by year

prev_educ_curr_ageadj <- purrr::map_dfr(2011:2024, function(yr) {
  message("Processing year: ", yr)
  
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(age7), !is.na(educ4))
  
  des_yr <- survey::svydesign(
    id = ~psu_id,
    strata = ~strata_id,
    weights = ~wt,
    data = df_yr,
    nest = TRUE
  )
  
  des_std <- survey::svystandardize(
    des_yr,
    by = ~age7,
    over = ~educ4,
    population = w_2000_age7_counts
  )
  
  out <- survey::svyby(
    ~any_binge_cdc,
    ~educ4,
    des_std,
    survey::svymean,
    na.rm = TRUE,
    vartype = "ci"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = yr)
  
  rm(df_yr, des_yr, des_std)
  gc()
  
  out
}) %>%
  dplyr::transmute(
    educ4 = as.character(educ4),
    year = as.integer(year),
    est = any_binge_cdc,
    lcl = ci_l,
    ucl = ci_u
  ) %>%
  dplyr::arrange(educ4, year)

readr::write_csv(
  prev_educ_curr_ageadj,
  file.path(tab_dir, "main_prev_any_binge_current_by_educ4_AGEADJ2000_CI_prop.csv")
)

readr::write_csv(
  prev_educ_curr_ageadj %>%
    dplyr::mutate(
      est = est * 100,
      lcl = lcl * 100,
      ucl = ucl * 100,
      ci  = sprintf("%.1f (%.1f–%.1f)", est, lcl, ucl)
    ),
  file.path(tab_dir, "main_prev_any_binge_current_by_educ4_AGEADJ2000_CI_pct.csv")
)

# ===========================================================

# =========================================================
# State post-processing
# =========================================================

# ---------- Helpers ----------

mk_state_adjacent_diffs <- function(df) {
  df %>%
    dplyr::arrange(state_code, year) %>%
    dplyr::group_by(state_code, state_abbr, state_name) %>%
    dplyr::mutate(
      prev_year = dplyr::lag(year),
      prev_est  = dplyr::lag(est),
      prev_lcl  = dplyr::lag(lcl),
      prev_ucl  = dplyr::lag(ucl)
    ) %>%
    dplyr::mutate(
      delta_prop = est - prev_est,
      delta_pp   = 100 * delta_prop,
      prev_ci    = dplyr::if_else(
        is.na(prev_est),
        NA_character_,
        sprintf("%.1f (%.1f–%.1f)", prev_est * 100, prev_lcl * 100, prev_ucl * 100)
      ),
      curr_ci    = dplyr::if_else(
        is.na(est),
        NA_character_,
        sprintf("%.1f (%.1f–%.1f)", est * 100, lcl * 100, ucl * 100)
      ),
      diff_note = dplyr::case_when(
        is.na(prev_est) & !is.na(est) ~ "No prior year estimate available",
        !is.na(prev_est) & is.na(est) ~ "Current year unavailable",
        is.na(prev_est) & is.na(est)  ~ "Both years unavailable",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(prev_year)) %>%
    dplyr::transmute(
      state_code, state_abbr, state_name,
      year_prev = prev_year,
      year_curr = year,
      prev_est, prev_lcl, prev_ucl,
      curr_est = est, curr_lcl = lcl, curr_ucl = ucl,
      delta_prop,
      delta_pp,
      prev_ci,
      curr_ci,
      diff_note
    ) %>%
    dplyr::arrange(state_code, year_curr)
}

rank_states_by_year <- function(df) {
  df %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      n_ranked = sum(!is.na(est)),
      rank_desc = dplyr::if_else(
        is.na(est),
        NA_integer_,
        dplyr::dense_rank(dplyr::desc(est))
      ),
      rank_asc = dplyr::if_else(
        is.na(est),
        NA_integer_,
        dplyr::dense_rank(est)
      ),
      percentile_desc = dplyr::if_else(
        is.na(est),
        NA_real_,
        dplyr::percent_rank(est)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      est_pct = est * 100,
      lcl_pct = lcl * 100,
      ucl_pct = ucl * 100,
      ci = dplyr::if_else(
        is.na(est),
        NA_character_,
        sprintf("%.1f (%.1f–%.1f)", est_pct, lcl_pct, ucl_pct)
      )
    ) %>%
    dplyr::arrange(year, rank_desc, state_code)
}

# ---------- Adjacent-year diffs ----------

state_diffs_all_ageadj <- mk_state_adjacent_diffs(state_prev_all_prop_ageadj)
state_diffs_curr_ageadj <- mk_state_adjacent_diffs(state_prev_curr_prop_ageadj)

readr::write_csv(
  state_diffs_all_ageadj,
  file.path(tab_dir, "main_state_adjacent_diffs_alladults_AGEADJ2000_prop.csv")
)

readr::write_csv(
  state_diffs_all_ageadj %>%
    dplyr::mutate(
      prev_est = prev_est * 100,
      prev_lcl = prev_lcl * 100,
      prev_ucl = prev_ucl * 100,
      curr_est = curr_est * 100,
      curr_lcl = curr_lcl * 100,
      curr_ucl = curr_ucl * 100
    ),
  file.path(tab_dir, "main_state_adjacent_diffs_alladults_AGEADJ2000_pct.csv")
)

readr::write_csv(
  state_diffs_curr_ageadj,
  file.path(tab_dir, "main_state_adjacent_diffs_current_AGEADJ2000_prop.csv")
)

readr::write_csv(
  state_diffs_curr_ageadj %>%
    dplyr::mutate(
      prev_est = prev_est * 100,
      prev_lcl = prev_lcl * 100,
      prev_ucl = prev_ucl * 100,
      curr_est = curr_est * 100,
      curr_lcl = curr_lcl * 100,
      curr_ucl = curr_ucl * 100
    ),
  file.path(tab_dir, "main_state_adjacent_diffs_current_AGEADJ2000_pct.csv")
)

# ---------- State ranks by year ----------

state_rank_all_ageadj <- rank_states_by_year(state_prev_all_prop_ageadj)
state_rank_curr_ageadj <- rank_states_by_year(state_prev_curr_prop_ageadj)

readr::write_csv(
  state_rank_all_ageadj,
  file.path(tab_dir, "main_state_ranks_alladults_AGEADJ2000.csv")
)

readr::write_csv(
  state_rank_curr_ageadj,
  file.path(tab_dir, "main_state_ranks_current_AGEADJ2000.csv")
)

# ---------- Optional QC: show unavailable state-years carried as NA ----------

readr::write_csv(
  state_rank_all_ageadj %>%
    dplyr::filter(is.na(est)) %>%
    dplyr::select(year, state_code, state_abbr, state_name),
  file.path(tab_dir, "qc_state_unavailable_years_alladults_AGEADJ2000.csv")
)

readr::write_csv(
  state_rank_curr_ageadj %>%
    dplyr::filter(is.na(est)) %>%
    dplyr::select(year, state_code, state_abbr, state_name),
  file.path(tab_dir, "qc_state_unavailable_years_current_AGEADJ2000.csv")
)


# =========================================================
# State trend tests from NEW age-adjusted state outputs
# =========================================================

trend_by_state_ageadj_PR <- function(df_prop, label, min_years = 5) {
  fit_one <- function(dd) {
    dd2 <- dd %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(
        year_lin = year - 2011,
        se_prop  = (ucl - lcl) / (2 * 1.96),
        se_log   = se_prop / pmax(est, 1e-8)
      ) %>%
      dplyr::filter(
        !is.na(est), !is.na(lcl), !is.na(ucl),
        est > 0,
        is.finite(se_prop), is.finite(se_log),
        se_prop > 0, se_log > 0
      )
    
    if (nrow(dd2) < min_years) {
      return(tibble::tibble(
        state_code    = dd$state_code[1],
        slope_log     = NA_real_,
        PR_per_year   = NA_real_,
        PR_lcl        = NA_real_,
        PR_ucl        = NA_real_,
        se            = NA_real_,
        z             = NA_real_,
        p_two         = NA_real_,
        n_years_used  = nrow(dd2),
        first_year    = if (nrow(dd2) == 0) NA_integer_ else min(dd2$year),
        last_year     = if (nrow(dd2) == 0) NA_integer_ else max(dd2$year),
        note          = "Insufficient non-missing age-adjusted years"
      ))
    }
    
    fit <- tryCatch(
      stats::lm(
        log(est) ~ year_lin,
        data = dd2,
        weights = 1 / (se_log^2)
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(tibble::tibble(
        state_code    = dd$state_code[1],
        slope_log     = NA_real_,
        PR_per_year   = NA_real_,
        PR_lcl        = NA_real_,
        PR_ucl        = NA_real_,
        se            = NA_real_,
        z             = NA_real_,
        p_two         = NA_real_,
        n_years_used  = nrow(dd2),
        first_year    = min(dd2$year),
        last_year     = max(dd2$year),
        note          = "Model failed"
      ))
    }
    
    co <- summary(fit)$coefficients["year_lin", ]
    b  <- unname(co["Estimate"])
    se <- unname(co["Std. Error"])
    z  <- b / se
    p  <- 2 * (1 - pnorm(abs(z)))
    ci <- b + c(-1, 1) * 1.96 * se
    
    tibble::tibble(
      state_code    = dd$state_code[1],
      slope_log     = b,
      PR_per_year   = exp(b),
      PR_lcl        = exp(ci[1]),
      PR_ucl        = exp(ci[2]),
      se            = se,
      z             = z,
      p_two         = p,
      n_years_used  = nrow(dd2),
      first_year    = min(dd2$year),
      last_year     = max(dd2$year),
      note          = NA_character_
    )
  }
  
  # Inside trend_by_state_ageadj_PR, replace the final left_join block with:
  purrr::map_dfr(
    df_prop %>%
      dplyr::group_by(state_code, state_abbr, state_name) %>%
      dplyr::group_split(),
    fit_one
  ) %>%
    # state_abbr/state_name already carried from group_split keys
    dplyr::left_join(
      state_xwalk %>%
        dplyr::select(fips, state_abbr, state_name) %>%
        dplyr::rename(state_abbr_xwalk = state_abbr,
                      state_name_xwalk = state_name),
      by = c("state_code" = "fips")
    ) %>%
    # Use xwalk values to fill any gaps
    dplyr::mutate(
      state_abbr = dplyr::coalesce(state_abbr, state_abbr_xwalk),
      state_name = dplyr::coalesce(state_name, state_name_xwalk)
    ) %>%
    dplyr::select(-state_abbr_xwalk, -state_name_xwalk) %>%
    dplyr::mutate(
      p_adj_bh = p.adjust(p_two, method = "BH"),
      denom = label,
      .before = 1
    ) %>%
    dplyr::select(
      denom, state_code, state_abbr, state_name,
      n_years_used, first_year, last_year,
      PR_per_year, PR_lcl, PR_ucl,
      slope_log, se, z, p_two, p_adj_bh, note
    ) %>%
    dplyr::arrange(state_code)
}

state_trend_all_PR_ageadj <- trend_by_state_ageadj_PR(
  state_prev_all_prop_ageadj,
  "All adults"
)

state_trend_curr_PR_ageadj <- trend_by_state_ageadj_PR(
  state_prev_curr_prop_ageadj,
  "Current drinkers"
)

readr::write_csv(
  state_trend_all_PR_ageadj,
  file.path(tab_dir, "main_state_trends_alladults_AGEADJ2000_PR.csv")
)

readr::write_csv(
  state_trend_curr_PR_ageadj,
  file.path(tab_dir, "main_state_trends_current_AGEADJ2000_PR.csv")
)


# Check which states lose trend estimates due to min_years = 5
readr::read_csv(file.path(tab_dir, "main_state_trends_alladults_AGEADJ2000_PR.csv")) %>%
  dplyr::filter(!is.na(note)) %>%
  dplyr::select(state_abbr, state_name, n_years_used, note) %>%
  print(n = Inf)

## "State-specific trend estimates were suppressed for jurisdictions with fewer than 5 years of stable age-adjusted estimates."

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
  any_binge_cdc ~ year_lin + sex_f + age7 + race6_nat + 
    educ4 + income6 + employ3 + marital3 + insured + factor(region),
  design = des_curr,
  family = quasipoisson(link = "log")
)

# ---- COVID sensitivity ----
des_curr <- stats::update(des_curr, 
                          covid = as.integer(year >= 2020))
des_all  <- stats::update(des_all,  
                          covid = as.integer(year >= 2020))

fit_pr_curr_covid <- survey::svyglm(
  any_binge_cdc ~ year_lin + covid + sex_f + age7 + race6_nat +
    educ4 + income6 + employ3 + marital3 + insured + factor(region),
  design = des_curr,
  family = quasipoisson(link = "log")
)

fit_pr_all_lin <- survey::svyglm(
  any_binge_cdc ~ year_lin + sex_f + age7 + race6_nat +
    educ4 + income6 + employ3 + marital3 + insured + factor(region),
  design = des_all,
  family = quasipoisson(link = "log")
)

fit_pr_all_covid <- survey::svyglm(
  any_binge_cdc ~ year_lin + covid + sex_f + age7 + race6_nat +
    educ4 + income6 + employ3 + marital3 + insured + factor(region),
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

pr_curr       <- mk_pr_table(fit_pr_curr_lin)
pr_curr_covid <- mk_pr_table(fit_pr_curr_covid)
pr_all        <- mk_pr_table(fit_pr_all_lin)
pr_all_covid  <- mk_pr_table(fit_pr_all_covid)

readr::write_csv(pr_curr,       file.path(tab_dir, "adj_PR_currentdrinkers_linetime_regionFE.csv"))
readr::write_csv(pr_curr_covid, file.path(tab_dir, "adj_PR_currentdrinkers_linetime_covidSens_regionFE.csv"))
readr::write_csv(pr_all,        file.path(tab_dir, "adj_PR_alladults_linetime_regionFE.csv"))
readr::write_csv(pr_all_covid,  file.path(tab_dir, "adj_PR_alladults_linetime_covidSens_regionFE.csv"))

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


# ---- Race × Time interaction — primary (continuous year, one PR per group) ----
fit_pr_race_time_lin <- survey::svyglm(
  any_binge_cdc ~ year_lin * race6_nat + sex_f + age7 +
    educ4 + income6 + employ3 + marital3 + insured + factor(region),
  design = des_curr,
  family = quasipoisson(link = "log")
)

# Joint Wald test
wt_race_time <- survey::regTermTest(fit_pr_race_time_lin, ~ year_lin:race6_nat)
race_time_test <- tibble::tibble(
  interaction = "Year×Race",
  chisq = unname(wt_race_time$test),
  df    = unname(wt_race_time$df),
  p     = unname(wt_race_time$p)
)
readr::write_csv(race_time_test,
                 file.path(tab_dir, "adj_PR_currentdrinkers_interaction_YearByRace_jointWald.csv"))

# Extract race-specific annual PRs
race_levels <- levels(des_curr$variables$race6_nat)
co <- stats::coef(fit_pr_race_time_lin)
vc <- stats::vcov(fit_pr_race_time_lin)

race_annual_pr <- purrr::map_dfr(race_levels, function(r) {
  if (r == "NH White") {  # reference group
    b  <- unname(co["year_lin"])
    se <- sqrt(vc["year_lin", "year_lin"])
  } else {
    int_term <- paste0("year_lin:race6_nat", r)
    b  <- unname(co["year_lin"]) + unname(co[int_term])
    se <- sqrt(
      vc["year_lin","year_lin"] +
        vc[int_term, int_term] +
        2 * vc["year_lin", int_term]
    )
  }
  tibble::tibble(
    race6_nat   = r,
    PR_per_year = exp(b),
    LCL         = exp(b - 1.96*se),
    UCL         = exp(b + 1.96*se),
    p           = 2*(1 - pnorm(abs(b/se)))
  )
})
readr::write_csv(race_annual_pr,
                 file.path(tab_dir, "adj_PR_currentdrinkers_TrendByRace_annualPR.csv"))

# ---- Race × Time sensitivity — factor(year), flexible trajectory ----
fit_pr_race_time_cat <- survey::svyglm(
  any_binge_cdc ~ factor(year) * race6_nat + sex_f + age7 +
    educ4 + income6 + employ3 + marital3 + insured + factor(region),
  design = des_curr,
  family = quasipoisson(link = "log")
)

wt_race_timecat <- survey::regTermTest(fit_pr_race_time_cat, 
                                       ~ `factor(year)`:race6_nat)
race_timecat_test <- tibble::tibble(
  interaction = "Year(categorical)×Race",
  chisq = unname(wt_race_timecat$test),
  df    = unname(wt_race_timecat$df),
  p     = unname(wt_race_timecat$p)
)
readr::write_csv(race_timecat_test,
                 file.path(tab_dir, "adj_PR_currentdrinkers_interaction_YearCatByRace_jointWald.csv"))


# ---- Secondary outcome: High-intensity binge (PRs), current drinkers ----
fit_hi_curr <- survey::svyglm(
  hi_binge ~ year_lin + sex_f + age7  + race6_nat  + educ4 +
    income6  + employ3 + marital3  + insured + factor(region),
  design = des_curr,
  family = quasipoisson(link = "log")
)

hi_pr <- mk_pr_table(fit_hi_curr)
readr::write_csv(hi_pr, file.path(tab_dir, "adj_PR_currentdrinkers_highIntensity.csv"))

# ---- Secondary outcome: Binge episode frequency (RR per drinking day) ----
des_curr_freq <- subset(des_curr, !is.na(binge_episodes) & !is.na(drink_days) & drink_days > 0)

fit_freq <- survey::svyglm(
  binge_episodes ~ year_lin + sex_f + age7  + race6_nat  + educ4 + income6  + employ3 + marital3  + insured + factor(region),
  design = des_curr_freq,
  family = quasipoisson(link = "log"),
  offset = log(drink_days)
)

freq_rr <- mk_pr_table(fit_freq)  # exp(coef) are rate ratios
readr::write_csv(freq_rr, file.path(tab_dir, "adj_RR_currentdrinkers_bingeFrequency_perDrinkDay.csv"))



# ---- Effect modification (Year × Sex), current drinkers ----
fit_pr_curr_int_sex <- survey::svyglm(any_binge_cdc ~ year_lin * sex_f + age7 + race6_nat + educ4 + income6 + employ3 + marital3 + insured + factor(region),
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




# ===== B) Overall all-adults — unadjusted, looped by year =====
prev_all_prop <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_all$variables %>% dplyr::filter(year == yr)
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svymean(~any_binge_cdc, des_yr, na.rm=TRUE)
  ci  <- suppressWarnings(confint(out))
  rm(df_yr, des_yr); gc()
  tibble::tibble(year=as.integer(yr),
                 est=as.numeric(out)[1], lcl=ci[1,1], ucl=ci[1,2])
}) %>% dplyr::arrange(year)

readr::write_csv(prev_all_prop,
                 file.path(tab_dir, "main_prev_any_binge_alladults_CI_prop.csv"))
readr::write_csv(prev_all_prop %>%
                   dplyr::mutate(est=est*100, lcl=lcl*100, ucl=ucl*100,
                                 ci=sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
                 file.path(tab_dir, "main_prev_any_binge_alladults_CI_pct.csv"))


# ===== C) Overall current drinkers — unadjusted, looped by year =====
prev_curr_prop <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_curr$variables %>% dplyr::filter(year == yr)
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svymean(~any_binge_cdc, des_yr, na.rm=TRUE)
  ci  <- suppressWarnings(confint(out))
  rm(df_yr, des_yr); gc()
  tibble::tibble(year=as.integer(yr),
                 est=as.numeric(out)[1], lcl=ci[1,1], ucl=ci[1,2])
}) %>% dplyr::arrange(year)

readr::write_csv(prev_curr_prop,
                 file.path(tab_dir, "main_prev_any_binge_current_CI_prop.csv"))
readr::write_csv(prev_curr_prop %>%
                   dplyr::mutate(est=est*100, lcl=lcl*100, ucl=ucl*100,
                                 ci=sprintf("%.1f (%.1f–%.1f)", est,lcl,ucl)),
                 file.path(tab_dir, "main_prev_any_binge_current_CI_pct.csv"))


# ===== D) By sex — unadjusted, looped by year =====
prev_sex_all <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(sex_f))
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svyby(~any_binge_cdc, ~sex_f, des_yr,
                       survey::svymean, na.rm=TRUE, vartype="ci") %>%
    tibble::as_tibble() %>% dplyr::mutate(year=yr)
  rm(df_yr, des_yr); gc()
  out
}) %>%
  dplyr::transmute(sex=as.character(sex_f), year=as.integer(year),
                   est=any_binge_cdc, lcl=ci_l, ucl=ci_u) %>%
  dplyr::arrange(sex, year)

readr::write_csv(prev_sex_all,
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_sex_CI_prop.csv"))


# ===== E1) By age group (10-bin) — unadjusted, looped by year =====
prev_age10_all <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age_group))
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svyby(~any_binge_cdc, ~age_group, des_yr,
                       survey::svymean, na.rm=TRUE, vartype="ci") %>%
    tibble::as_tibble() %>% dplyr::mutate(year=yr)
  rm(df_yr, des_yr); gc()
  out
}) %>%
  dplyr::transmute(age_group10=as.character(age_group),
                   year=as.integer(year),
                   est=any_binge_cdc, lcl=ci_l, ucl=ci_u) %>%
  dplyr::arrange(age_group10, year)

readr::write_csv(prev_age10_all,
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_age10_CI_prop.csv"))


# ===== E2) By age group (5-bin) — unadjusted, looped by year =====
prev_age5_all <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(age_fallback))
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svyby(~any_binge_cdc, ~age_fallback, des_yr,
                       survey::svymean, na.rm=TRUE, vartype="ci") %>%
    tibble::as_tibble() %>% dplyr::mutate(year=yr)
  rm(df_yr, des_yr); gc()
  out
}) %>%
  dplyr::transmute(age_group5=as.character(age_fallback),
                   year=as.integer(year),
                   est=any_binge_cdc, lcl=ci_l, ucl=ci_u) %>%
  dplyr::arrange(age_group5, year)

readr::write_csv(prev_age5_all,
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_age5_CI_prop.csv"))


# ===== F) Side-by-side both denominators =====
both_prop <- dplyr::bind_rows(
  prev_all_prop  %>% dplyr::mutate(denom = "All adults"),
  prev_curr_prop %>% dplyr::mutate(denom = "Current drinkers")
) %>% dplyr::arrange(denom, year)

readr::write_csv(both_prop,
                 file.path(tab_dir, "main_prev_any_binge_both_denoms_CI_prop.csv"))


# ===== G) Adjacent year deltas — unadjusted =====

# NOTE: Adjacent-year differences are computed as DESCRIPTIVE summaries only.
# Serial correlation between adjacent tests means BH-adjusted p-values are
# approximate. Inferential trend claims rely on the regression PR models and
# Joinpoint APC. These tables support figures and are not used for hypothesis testing.

mk_pairs <- function(df_ci) {
  df_se <- df_ci %>%
    dplyr::mutate(se = (ucl - lcl) / (2 * 1.96)) %>%
    dplyr::select(year, est, se)
  
  yrs   <- sort(unique(df_se$year))
  pairs <- tibble::tibble(year1 = yrs[-length(yrs)], year2 = yrs[-1])
  
  pairs %>%
    dplyr::left_join(df_se, by = c("year1" = "year")) %>%
    dplyr::rename(est1 = est, se1 = se) %>%
    dplyr::left_join(df_se, by = c("year2" = "year")) %>%
    dplyr::rename(est2 = est, se2 = se) %>%
    dplyr::mutate(
      diff_pp = (est2 - est1) * 100,
      sed     = sqrt(se1^2 + se2^2),
      z       = (est2 - est1) / sed,
      p_two   = 2 * (1 - pnorm(abs(z)))
    ) %>%
    dplyr::select(year1, year2, diff_pp, z, p_two)
}

diff_all_years  <- mk_pairs(prev_all_prop)  %>%
  dplyr::mutate(denom="All adults", .before=1)
diff_curr_years <- mk_pairs(prev_curr_prop) %>%
  dplyr::mutate(denom="Current drinkers", .before=1)

diff_both_years <- dplyr::bind_rows(diff_all_years, diff_curr_years) %>%
  dplyr::arrange(denom, year1) %>%
  dplyr::group_by(denom) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method="BH")) %>%
  dplyr::ungroup()

readr::write_csv(diff_both_years,
                 file.path(tab_dir, "main_prev_any_binge_adjacent_diffs.csv"))


# ===== H) By race — all adults, unadjusted, looped by year =====
prev_race_all <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_all$variables %>%
    dplyr::filter(year == yr, !is.na(race6_nat))
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svyby(~any_binge_cdc, ~race6_nat, des_yr,
                       survey::svymean, na.rm=TRUE, vartype="ci") %>%
    tibble::as_tibble() %>% dplyr::mutate(year=yr)
  rm(df_yr, des_yr); gc()
  out
}) %>%
  dplyr::transmute(race6_nat=as.character(race6_nat),
                   year=as.integer(year),
                   est=any_binge_cdc, lcl=ci_l, ucl=ci_u) %>%
  dplyr::arrange(race6_nat, year)

readr::write_csv(prev_race_all,
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_CI_prop.csv"))
readr::write_csv(prev_race_all %>%
                   dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~.x*100)),
                 file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_CI_pct.csv"))


# ===== I) By race — current drinkers, unadjusted, looped by year =====
prev_race_curr <- purrr::map_dfr(2011:2024, function(yr) {
  df_yr <- des_curr$variables %>%
    dplyr::filter(year == yr, !is.na(race6_nat))
  des_yr <- survey::svydesign(id=~psu_id, strata=~strata_id,
                              weights=~wt, data=df_yr, nest=TRUE)
  out <- survey::svyby(~any_binge_cdc, ~race6_nat, des_yr,
                       survey::svymean, na.rm=TRUE, vartype="ci") %>%
    tibble::as_tibble() %>% dplyr::mutate(year=yr)
  rm(df_yr, des_yr); gc()
  out
}) %>%
  dplyr::transmute(race6_nat=as.character(race6_nat),
                   year=as.integer(year),
                   est=any_binge_cdc, lcl=ci_l, ucl=ci_u) %>%
  dplyr::arrange(race6_nat, year)

readr::write_csv(prev_race_curr,
                 file.path(tab_dir, "main_prev_any_binge_current_by_race6_CI_prop.csv"))
readr::write_csv(prev_race_curr %>%
                   dplyr::mutate(dplyr::across(c(est,lcl,ucl), ~.x*100)),
                 file.path(tab_dir, "main_prev_any_binge_current_by_race6_CI_pct.csv"))

# Clean up unadjusted race objects — needed for section J
# NOTE: keep prev_race_all and prev_race_curr alive until after J runs

# ===== Step 10) Weighted Table 1 & Rao–Scott tests =====
make_table1 <- function(des, by_vars) {
  purrr::map_dfr(by_vars, function(v) {
    f <- as.formula(paste0("~any_binge_cdc + ", v))
    
    out <- survey::svyby(
      ~any_binge_cdc, 
      as.formula(paste0("~", v)),
      des,
      survey::svymean,
      na.rm = TRUE,
      vartype = "ci"
    ) %>%
      tibble::as_tibble() %>%
      dplyr::transmute(
        variable = v,
        level    = as.character(.data[[v]]),
        prev_binge     = any_binge_cdc * 100,
        prev_binge_lcl = ci_l * 100,
        prev_binge_ucl = ci_u * 100,
        ci = sprintf("%.1f (%.1f–%.1f)",
                     prev_binge, prev_binge_lcl, prev_binge_ucl)
      )
    out
  })
}

# Run it
tab1_all <- make_table1(
  des_all,
  c("sex_f","age7","race6_nat","educ4","income6","marital3")
)

tab1_curr <- make_table1(
  des_curr,
  c("sex_f","age7","race6_nat","educ4","income6","marital3")
)

readr::write_csv(tab1_all,
                 file.path(tab_dir, "table1_alladults_binge_prev_by_covariate.csv"))
readr::write_csv(tab1_curr,
                 file.path(tab_dir, "table1_current_binge_prev_by_covariate.csv"))





# ===== J) Adjacent year deltas within each race group (both denominators) =====

mk_pairs_by <- function(df_ci, group_var) {
  gname <- rlang::as_name(rlang::ensym(group_var))
  df_ci <- dplyr::mutate(df_ci, se = (ucl - lcl) / (2 * 1.96))
  
  dplyr::bind_rows(lapply(split(df_ci, df_ci[[gname]]), function(dd) {
    yrs <- sort(unique(dd$year))
    if (length(yrs) < 2) return(tibble::tibble())
    
    pairs <- tibble::tibble(year1 = yrs[-length(yrs)], year2 = yrs[-1])
    dd2   <- dd %>% dplyr::select(year, est, se)
    
    pairs %>%
      dplyr::left_join(dd2, by = c("year1" = "year")) %>%
      dplyr::rename(est1 = est, se1 = se) %>%
      dplyr::left_join(dd2, by = c("year2" = "year")) %>%
      dplyr::rename(est2 = est, se2 = se) %>%
      dplyr::mutate(
        diff_pp = (est2 - est1) * 100,
        sed     = sqrt(se1^2 + se2^2),
        z       = (est2 - est1) / sed,
        p_two   = 2 * (1 - pnorm(abs(z))),
        !!gname := unique(dd[[gname]])
      ) %>%
      dplyr::select(!!rlang::sym(gname), year1, year2, diff_pp, z, p_two)
  }))
}

race_diffs_all_ageadj  <- mk_pairs_by(prev_race_all_ageadj,  race6_nat) %>%
  dplyr::mutate(denom = "All adults", .before = 1)

race_diffs_curr_ageadj <- mk_pairs_by(prev_race_curr_ageadj, race6_nat) %>%
  dplyr::mutate(denom = "Current drinkers", .before = 1)

race_diffs_all_ageadj <- race_diffs_all_ageadj %>%
  dplyr::group_by(denom, race6_nat) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()

race_diffs_curr_ageadj <- race_diffs_curr_ageadj %>%
  dplyr::group_by(denom, race6_nat) %>%
  dplyr::mutate(p_adj_bh = p.adjust(p_two, method = "BH")) %>%
  dplyr::ungroup()

readr::write_csv(
  race_diffs_all_ageadj,
  file.path(tab_dir, "main_prev_any_binge_alladults_by_race6_AGEADJ2000_adjacent_diffs.csv")
)

readr::write_csv(
  race_diffs_curr_ageadj,
  file.path(tab_dir, "main_prev_any_binge_current_by_race6_AGEADJ2000_adjacent_diffs.csv")
)

# ===== Step 11) Sensitivity: including territories =====

cache_files <- list.files(
  file.path(PROJ_DIR, "data/cache"),
  pattern = paste0("^BRFSS_\\d{4}_", CACHE_VER, "\\.rds$"),
  full.names = TRUE
)

df_all_incl <- lapply(cache_files, readRDS) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    # year-specific strata/PSU — same as main analysis
    strata = interaction(year, strata, drop = TRUE, lex.order = TRUE),
    psu    = interaction(year, psu,    drop = TRUE, lex.order = TRUE)
  ) %>%
  dplyr::filter(!is.na(sex), !is.na(age_group)) %>%
  dplyr::mutate(
    strata_id = as.integer(factor(strata)),
    psu_id    = as.integer(factor(psu))
  )

des_all_incl <- survey::svydesign(
  id      = ~psu_id,
  strata  = ~strata_id,
  weights = ~wt,
  data    = df_all_incl,
  nest    = TRUE
)

des_all_incl <- stats::update(
  des_all_incl,
  is_current = dplyr::case_when(
    is.na(alcday5) | alcday5 %in% c(777,999) ~ NA,
    alcday5 == 888 ~ FALSE,
    TRUE ~ TRUE
  )
)

# Overall prevalence comparison: 50 states+DC vs including territories
prev_statesDC <- survey::svyby(
  ~any_binge_cdc, ~year, des_all, survey::svymean, na.rm = TRUE
) %>%
  dplyr::transmute(year = as.integer(year), est_statesDC = any_binge_cdc)

prev_incl <- survey::svyby(
  ~any_binge_cdc, ~year, des_all_incl, survey::svymean, na.rm = TRUE
) %>%
  dplyr::transmute(year = as.integer(year), est_incl = any_binge_cdc)

sens_overall <- dplyr::left_join(prev_statesDC, prev_incl, by = "year") %>%
  dplyr::mutate(diff_pp = (est_incl - est_statesDC) * 100) %>%
  dplyr::arrange(year)

readr::write_csv(
  sens_overall,
  file.path(tab_dir, "sensitivity_territories_overall_prev_by_year.csv")
)

rm(df_all_incl, des_all_incl); gc()