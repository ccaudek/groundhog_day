# COPE

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

cope_items <- rio::import(
  here::here("data", "prep", "quest_scales", "cope_items.csv")
)

# Source cope.R on GitHub, which includes the function scoring_cope().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/cope.R"
)

cope_subscales <- scoring_cope(cope_items)

rio::export(
  cope_subscales,
  here::here("data", "prep", "quest_scales", "cope_scores.csv")
)

# eof ----
