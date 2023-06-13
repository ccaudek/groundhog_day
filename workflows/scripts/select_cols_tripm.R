# TRI-PM

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 58

tripm_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("tripm")) |>
  dplyr::rename(user_id = num_cell_tot_1) |> 
  dplyr::select(-tripm_54)

tripm_items_names <- paste0("tripm_", 1:NITEMS)
tripm_items_names_plus_id <- c("user_id", tripm_items_names)

colnames(tripm_items) <- tripm_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$tripm_54)

# The coding for the items (not reversed) is the following:
# True = 3; Somewhat true = 2; Somewhat false = 1; False = 0.
# To obtain this coding we must perform this transformation.
tripm_items <- tripm_items %>%
  mutate(across(2:59, ~ case_when(
    . == 4 ~ 0,
    . == 3 ~ 1,
    . == 2 ~ 2,
    . == 1 ~ 3, # sempre
    TRUE ~ NA_integer_
  )))

rio::export(
  tripm_items,
  here::here("data", "prep", "quest_scales", "tripm_items.csv")
)

# eof ----
