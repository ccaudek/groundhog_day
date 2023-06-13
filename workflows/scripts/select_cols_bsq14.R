#' BSQ-14

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 14

bsq14_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("bsq")) |>
  dplyr::rename(user_id = num_cell_tot_1) 

bsq14_items_names <- paste0("bsq14_", 1:NITEMS)
bsq14_items_names_plus_id <- c("user_id", bsq14_items_names)
colnames(bsq14_items) <- bsq14_items_names_plus_id


# bdi2_items <- bdi2_items %>%
#   mutate(all_of(across(set1, ~ case_when(
#     . == 4 ~ 3,
#     . == 3 ~ 2,
#     . == 2 ~ 1,
#     . == 1 ~ 0,
#     TRUE ~ NA_integer_
#   ))))


# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr2_1.y)

rio::export(
  bsq14_items,
  here::here("data", "prep", "quest_scales", "bsq14_items.csv")
)

# eof ----
