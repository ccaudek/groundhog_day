# CERQ

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

cerq_items <- rio::import(
  here::here("data", "prep", "quest_scales", "cerq_items.csv")
)

# Source cerq.R on GitHub, which includes the function scoring_cerq().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/cerq.R"
)

cerq_subscales <- scoring_cerq(cerq_items)

rio::export(
  cerq_subscales,
  here::here("data", "prep", "quest_scales", "cerq_scores.csv")
)

# eof ----
