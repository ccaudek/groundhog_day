#' CERQ

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

d <- rio::import(here::here("data", "prep", "quest.csv"))

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

NITEMS <- 36

cerq_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("cerq")) |>
  dplyr::rename(user_id = num_cell_tot_1) |>
  # catch item
  dplyr::select(-"cerq_17")

cerq_items_names <- paste0("cerq_", 1:NITEMS)
cerq_items_names_plus_id <- c("user_id", cerq_items_names)

colnames(cerq_items) <- cerq_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cerq_17)

rio::export(
  cerq_items,
  here::here("data", "prep", "quest_scales", "cerq_items.csv")
)

# eof ----

