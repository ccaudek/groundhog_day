# Tri_PM

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

tripm_items <- rio::import(
  here::here("data", "prep", "quest_scales", "tripm_items.csv")
)

# Source sias.R on GitHub, which includes the function scoring_tripm().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/tripm.R"
)

tripm_subscales <- scoring_tripm(tripm_items)

rio::export(
  tripm_subscales,
  here::here("data", "prep", "quest_scales", "tripm_scores.csv")
)

# eof ----
