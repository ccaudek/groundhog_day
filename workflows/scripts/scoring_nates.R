# No Attachment to Ego Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

nates_items <- rio::import(
  here::here("data", "prep", "quest_scales", "nates_items.csv")
)

# Source nates.R on GitHub, which includes the function scoring_nates().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/nates.R"
)

nates_subscales <- scoring_nates(nates_items)

rio::export(
  nates_subscales,
  here::here("data", "prep", "quest_scales", "nates_scores.csv")
)

# eof ----
