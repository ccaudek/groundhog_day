
suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 19

sias_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("sias")) |>
  dplyr::rename(user_id = num_cell_tot_1) 

sias_items_names <- paste0("sias_", 1:NITEMS)
sias_items_names_plus_id <- c("user_id", sias_items_names)

colnames(sias_items) <- sias_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr3_1.y)

rio::export(
  sias_items,
  here::here("data", "prep", "quest_scales", "sias_items.csv")
)

# eof ----


