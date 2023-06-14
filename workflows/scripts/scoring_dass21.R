# DASS-21

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

dass21_items <- rio::import(
  here::here("data", "prep", "quest_scales", "dass21_items.csv")
)

# Source sias.R on GitHub, which includes the function scoring_tripm().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/dass21.R"
)

dass21_subscales <- scoring_dass21(dass21_items)

rio::export(
  dass21_subscales, 
  here::here("data", "prep", "quest_scales", "dass21_scores.csv")
)

# eof ----

