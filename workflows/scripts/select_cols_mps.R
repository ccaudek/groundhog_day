# MPS

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 35

mps_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("mps")) |>
  dplyr::rename(user_id = num_cell_tot_1)

mps_items_names <- paste0("mps_", 1:NITEMS)
mps_items_names_plus_id <- c("user_id", mps_items_names)

colnames(mps_items) <- mps_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr3_1.x)

rio::export(
  mps_items,
  here::here("data", "prep", "quest_scales", "mps_items.csv")
)

# eof ----
