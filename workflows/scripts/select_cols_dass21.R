# DASS-21

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 21

dass21_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("dass")) |>
  dplyr::rename(user_id = num_cell_tot_1) 
  # there is no catch item with name dass_something

dass21_items_names <- paste0("dass21_", 1:NITEMS)
dass21_items_names_plus_id <- c("user_id", dass21_items_names)

colnames(dass21_items) <- dass21_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr2_1.x)

rio::export(
  dass21_items,
  here::here("data", "prep", "quest_scales", "dass21_items.csv")
)

# eof ----

