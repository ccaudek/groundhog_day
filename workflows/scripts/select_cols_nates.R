# No-attachment to Ego Scale

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 7

ego_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("ego_")) |>
  dplyr::rename(user_id = num_cell_tot_1) |>
  # catch item
  dplyr::select(-"ego_5")

ego_items_names <- paste0("nates_", 1:NITEMS)
ego_items_names_plus_id <- c("user_id", ego_items_names)
colnames(ego_items) <- ego_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$ego_5)

rio::export(
  ego_items,
  here::here("data", "prep", "quest_scales", "nates_items.csv")
)

# eof ----
