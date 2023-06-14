# Self-Compassion Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 26

scs_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("scs")) |>
  dplyr::rename(user_id = num_cell_tot_1) |>
  # catch item
  dplyr::select(-"scs_25")

scs_items_names <- paste0("scs_", 1:NITEMS)
scs_items_names_plus_id <- c("user_id", scs_items_names)
colnames(scs_items) <- scs_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$scs_25)

rio::export(
  scs_items,
  here::here("data", "prep", "quest_scales", "scs_items.csv")
)
