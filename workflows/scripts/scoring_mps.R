# FMPS

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

mps_items <- rio::import(
  here::here("data", "prep", "quest_scales", "mps_items.csv")
)

# Source mps.R on GitHub, which includes the function scoring_mps().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/mps.R"
)

mps_subscales <- scoring_mps(mps_items)

rio::export(
  mps_subscales, 
  here::here("data", "prep", "quest_scales", "mps_scores.csv")
)

# eof ----

