# COPE

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 60

cope_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("cope")) |>
  dplyr::rename(user_id = num_cell_tot_1) |>
  # catch item
  dplyr::select(-"cope_nvi_12")

cope_items_names <- paste0("cope_", 1:NITEMS)
cope_items_names_plus_id <- c("user_id", cope_items_names)
colnames(cope_items) <- cope_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cope_nvi_12)

rio::export(
  cope_items,
  here::here("data", "prep", "quest_scales", "cope_items.csv")
)

# eof ----

