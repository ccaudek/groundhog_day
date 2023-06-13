# EAT 26

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 26

eat26_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("eat")) |>
  dplyr::rename(user_id = num_cell_tot_1)

eat26_items_names <- paste0("eat26_", 1:NITEMS)
eat26_items_names_plus_id <- c("user_id", eat26_items_names)

colnames(eat26_items) <- eat26_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr1_1)

rio::export(
  eat26_items,
  here::here("data", "prep", "quest_scales", "eat26_items.csv")
)

# eof ----
