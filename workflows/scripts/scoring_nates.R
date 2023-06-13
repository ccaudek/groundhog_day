# No attachment to Ego scale

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
  library(here)
})

ego_items <- rio::import(
  here::here("data", "prep", "quest_scales", "nates_items.csv")
)

# Identify numeric columns
numeric_cols <- sapply(ego_items, is.numeric)

# Create new column with the sum of numeric columns
ego_items$nates_score <- rowSums(ego_items[, numeric_cols]) 

ego_scores_df <- ego_items |> 
  dplyr::select(-c(ego_1, ego_2, ego_3, ego_4, ego_5, ego_6, ego_7))

rio::export(
  ego_scores_df, 
  here::here("data", "prep", "quest_scales", "nates_scores.csv")
)

# eof ----

