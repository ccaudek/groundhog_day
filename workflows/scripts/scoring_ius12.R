# IUS-12

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

ius12_items <- rio::import(
  here::here("data", "prep", "quest_scales", "ius12_items.csv")
)

# Source sias.R on GitHub, which includes the function scoring_tripm().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/ius12.R"
)

ius12_subscales <- scoring_ius12(ius12_items)

rio::export(
  ius12_subscales,
  here::here("data", "prep", "quest_scales", "ius12_scores.csv")
)

# eof ----
