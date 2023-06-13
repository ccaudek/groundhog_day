# BSQ-14

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

bsq14_items <- rio::import(
  here::here("data", "prep", "quest_scales", "bsq14_items.csv")
)

# Source bsq14.R on GitHub, which includes the function scoring_bsq14().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/bsq14.R"
)

bsq14_subscales <- scoring_bsq14(bsq14_items)

rio::export(
  bsq14_subscales,
  here::here("data", "prep", "quest_scales", "bsq14_scores.csv")
)

# eof ----
