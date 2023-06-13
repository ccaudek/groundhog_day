# The intolerance of uncertainty scale - Short Form (IUS-SF)
# IUS-12; Carleton et al., 2007)
# The items were rated on a 5-point Likert scale ranging from 1 (not at
# all characteristic of me) to 5 (entirely characteristic of me). 

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 12

ius_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("ius")) |>
  dplyr::rename(user_id = num_cell_tot_1) |> 
  dplyr::select(-ius_9)

ius_items_names <- paste0("ius_", 1:NITEMS)
ius_items_names_plus_id <- c("user_id", ius_items_names)

colnames(ius_items) <- ius_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$ius_9)

# ius_items <- tripm_items %>%
#   mutate(across(2:59, ~ case_when(
#     . == 4 ~ 0,
#     . == 3 ~ 1,
#     . == 2 ~ 2,
#     . == 1 ~ 3, # sempre
#     TRUE ~ NA_integer_
#   )))

rio::export(
  ius_items,
  here::here("data", "prep", "quest_scales", "ius12_items.csv")
)

# eof ----
