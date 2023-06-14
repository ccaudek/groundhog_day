# Self-Compassion Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

scs_items <- rio::import(
  here::here("data", "prep", "quest_scales", "scs_items.csv")
)

# Source scs.R on GitHub, which includes the function scoring_scs().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/scs.R"
)

scs_subscales <- scoring_scs(scs_items)

rio::export(
  scs_subscales,
  here::here("data", "prep", "quest_scales", "scs_scores.csv")
)

# eof ----
