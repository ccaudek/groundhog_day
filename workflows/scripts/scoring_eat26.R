suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

eat26_items <- rio::import(
  here::here("data", "prep", "quest_scales", "eat26_items.csv")
)

# Source sias.R on GitHub, which includes the function scoring_sias().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/eat26.R"
)

eat26_subscales <- scoring_eat26(eat26_items)

rio::export(
  eat26_subscales,
  here::here("data", "prep", "quest_scales", "eat26_scores.csv")
)

# eof ----
