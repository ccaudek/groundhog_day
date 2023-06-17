# Frost Multidimensional Perfectionism Scale (F-MPS).

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

fmps_items <- rio::import(
  here::here("data", "prep", "quest_scales", "fmps_items.csv")
)

# Source fmps.R on GitHub, which includes the function scoring_fmps().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/fmps.R"
)

fmps_subscales <- scoring_fmps(fmps_items)

rio::export(
  fmps_subscales,
  here::here("data", "prep", "quest_scales", "fmps_scores.csv")
)

# eof ----
