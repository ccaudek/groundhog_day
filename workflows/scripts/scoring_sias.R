suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

sias_items <- rio::import(
  here::here("data", "prep", "quest_scales", "sias_items.csv")
)

# Source sias.R on GitHub, which includes the function scoring_sias().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/sias.R"
)

sias_subscales <- scoring_sias(sias_items)

rio::export(
  sias_subscales, 
  here::here("data", "prep", "quest_scales", "sias_scores.csv")
)

# eof ----

