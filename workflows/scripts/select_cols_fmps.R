# Frost Multidimensional Perfectionism Scale (F-MPS).

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 35

fmps_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("mps")) |>
  dplyr::rename(user_id = num_cell_tot_1)

fmps_items_names <- paste0("fmps_", 1:NITEMS)
fmps_items_names_plus_id <- c("user_id", fmps_items_names)
colnames(fmps_items) <- fmps_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr3_1.x)

rio::export(
  fmps_items,
  here::here("data", "prep", "quest_scales", "fmps_items.csv")
)

# eof ----
