install.packages(c("survey","srvyr","tidyverse","haven","labelled",
                   "janitor","marginaleffects","broom","ggplot2"))
library(survey); library(srvyr); library(tidyverse); library(haven)
library(labelled); library(janitor); library(marginaleffects)
library(broom); library(ggplot2)


# 1) Install renv if needed
install.packages("renv")

# 2) Initialize the project (creates renv/ and renv.lock)
renv::init()   # choose 'Yes' if prompted to snapshot current packages

# 4) Snapshot package versions into renv.lock
renv::snapshot()

writeLines(capture.output(sessionInfo()), "docs/sessionInfo.txt")

# --- Create folders ---
dirs <- c(
  "data/raw/2011","data/raw/2015","data/raw/2019",
  "data/derived","code","outputs/tables","outputs/figures","docs"
)
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# --- Reproducibility: renv ---
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!file.exists("renv.lock")) renv::init() else renv::restore()

# --- Install core packages if missing ---
pkgs <- c("survey","srvyr","tidyverse","haven","labelled",
          "janitor","marginaleffects","broom","ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)

# --- Save session info (useful for the docs folder) ---
writeLines(capture.output(sessionInfo()), "docs/sessionInfo.txt")