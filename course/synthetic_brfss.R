# ============================================================
# synthetic_brfss.R
# Generates realistic synthetic BRFSS-like data for course exercises.
# NOT real BRFSS data. Used only for teaching purposes.
# ============================================================

make_synthetic_brfss <- function(n_per_year = 3000, years = 2011:2024, seed = 42) {
  set.seed(seed)

  n_total <- n_per_year * length(years)

  # True binge prevalence trend: rising slightly from 2011, peaking ~2019, mild drop
  true_prev <- function(yr) {
    base <- 0.165
    trend <- 0.003 * (yr - 2011)
    covid <- ifelse(yr == 2020, 0.01, 0)
    pmin(pmax(base + trend + covid, 0.10), 0.30)
  }

  df <- lapply(years, function(yr) {
    n <- n_per_year

    # Demographics
    sex      <- sample(1:2, n, replace = TRUE, prob = c(0.48, 0.52))  # 1=M, 2=F
    age5     <- sample(1:13, n, replace = TRUE,
                       prob = c(0.09, 0.08, 0.09, 0.10, 0.09, 0.09, 0.09, 0.08, 0.08, 0.07, 0.06, 0.05, 0.03))
    imprace  <- sample(1:6, n, replace = TRUE,
                       prob = c(0.63, 0.12, 0.14, 0.02, 0.05, 0.04))
    educag   <- sample(1:4, n, replace = TRUE, prob = c(0.10, 0.28, 0.29, 0.33))
    income2  <- sample(1:8, n, replace = TRUE)
    employ1  <- sample(1:8, n, replace = TRUE, prob = c(0.35, 0.15, 0.04, 0.03, 0.12, 0.10, 0.10, 0.11))
    marital  <- sample(1:6, n, replace = TRUE, prob = c(0.45, 0.10, 0.08, 0.05, 0.22, 0.10))
    hlthpln1 <- sample(1:2, n, replace = TRUE, prob = c(0.87, 0.13))
    state    <- sample(1:51, n, replace = TRUE)  # 50 states + DC

    # Survey design variables
    psu    <- sample(1:200, n, replace = TRUE)
    strata <- sample(1:100, n, replace = TRUE)
    wt     <- runif(n, 100, 15000) * (1 + 0.5 * (age5 > 10))  # older adults upweighted

    # Alcohol behavior — depends on age, sex, year
    prev_yr <- true_prev(yr)
    # Young adults (18-34) binge more
    age_mult <- ifelse(age5 %in% 1:3, 1.7, ifelse(age5 %in% 4:6, 1.1, 0.7))
    # Males binge more
    sex_mult <- ifelse(sex == 1, 1.4, 0.8)
    p_binge  <- pmin(pmax(prev_yr * age_mult * sex_mult, 0.01), 0.70)

    any_binge <- rbinom(n, 1, p_binge)

    # CDC binge flag: rfbing5 (1 = No, 2 = Yes — CDC reverse codes!)
    rfbing5 <- ifelse(any_binge == 1, 2L, 1L)
    # Small random non-response
    rfbing5[sample(n, round(n * 0.03))] <- NA

    # Drinking days (ALCDAY5 coding: 101-107 = N days/week, 201-231 = N days/month, 888 = none)
    drink_status <- ifelse(any_binge == 1, "current",
                    ifelse(runif(n) < 0.30, "none", "current_light"))
    alcday5 <- ifelse(drink_status == "none", 888L,
                ifelse(drink_status == "current",
                       sample(c(101:107, 201:231), n, replace = TRUE,
                              prob = c(0.05, 0.08, 0.10, 0.12, 0.10, 0.08, 0.07,
                                       rep(0.01, 31))),
                       sample(c(201:231), n, replace = TRUE)))
    alcday5 <- as.integer(alcday5)
    alcday5[sample(n, round(n * 0.02))] <- 777L  # Don't know

    # Binge episodes (DRNK3GE5): 0-76 times in past 30 days, 88 = zero
    drnk3ge5 <- ifelse(any_binge == 1,
                       sample(1:15, n, replace = TRUE, prob = dexp(1:15, rate = 0.4) / sum(dexp(1:15, rate = 0.4))),
                       88L)
    drnk3ge5[drink_status == "none"] <- 88L

    # Max drinks (MAXDRNKS)
    maxdrnks <- ifelse(any_binge == 1,
                       pmin(pmax(round(rnorm(n, 6, 2)), 1), 30),
                       sample(1:3, n, replace = TRUE))
    maxdrnks[sample(n, round(n * 0.05))] <- 99L  # Refused/DK

    # Average drinks per drinking day (AVEDRNK3)
    avedrnk3 <- ifelse(any_binge == 1,
                       pmin(pmax(round(rnorm(n, 4, 1.5)), 1), 20),
                       sample(1:3, n, replace = TRUE))
    avedrnk3[sample(n, round(n * 0.05))] <- 99L

    data.frame(
      year     = yr,
      wt       = wt,
      strata   = strata,
      psu      = psu,
      state    = state,
      sex      = sex,
      age5     = age5,
      imprace  = imprace,
      educag   = educag,
      income2  = income2,
      employ1  = employ1,
      marital  = marital,
      hlthpln1 = hlthpln1,
      alcday5  = alcday5,
      rfbing5  = rfbing5,
      drnk3ge5 = drnk3ge5,
      maxdrnks = maxdrnks,
      avedrnk3 = avedrnk3,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(df)
}


# Also create a small single-year slice for quick demos
make_demo_year <- function(yr = 2019, n = 500, seed = 99) {
  make_synthetic_brfss(n_per_year = n, years = yr, seed = seed)
}
