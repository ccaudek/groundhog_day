#' BDI-2
#' The BDI-II is scored by summing the ratings for the 21 items. Each item is 
#' rated on a 4-point scale ranging from 0 to 3. The maximum total score is 63.
#' Special attention must be paid to the correct scoring of the Changes in 
#' Sleeping Pattern (Item 16) and Changes in Appetite (Item 18) items. Each of 
#' these items contains seven options rated, in order, 0, 1a, 1b, 2a, 2b, 3a, 
#' 3b, to differentiate between increases and decreases in behavior or 
#' motivation. If a higher rated option is chosen by the respondent, the 
#' presence of an increase or decrease in either symptom should be clinically 
#' noted for diagnostic purposes. 

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest.csv"))

NITEMS <- 21

bdi2_items <- d |>
  dplyr::select("num_cell_tot_1", starts_with("bdi")) |>
  dplyr::rename(user_id = num_cell_tot_1) 

bdi2_items_names <- paste0("bdi2_", 1:NITEMS)
bdi2_items_names_plus_id <- c("user_id", bdi2_items_names)
colnames(bdi2_items) <- bdi2_items_names_plus_id

# The coding for the items (not reversed) is the following:
# True = 3; Somewhat true = 2; Somewhat false = 1; False = 0.
# To obtain this coding we must perform this transformation.

set1 <- c(
  "bdi2_1"  , "bdi2_2"  , "bdi2_3"  , "bdi2_4"  , "bdi2_5"  , 
  "bdi2_6"  , "bdi2_7" ,"bdi2_8"  , "bdi2_9"  , "bdi2_10" , "bdi2_11" , 
  "bdi2_12" , "bdi2_13" , "bdi2_14" , "bdi2_15", "bdi2_17" , 
  "bdi2_19" , "bdi2_20" , "bdi2_21"
)

bdi2_items <- bdi2_items %>%
  mutate(all_of(across(set1, ~ case_when(
    . == 4 ~ 3,
    . == 3 ~ 2,
    . == 2 ~ 1,
    . == 1 ~ 0,
    TRUE ~ NA_integer_
  ))))

set2 <- c("bdi2_16", "bdi2_18")

bdi2_items <- bdi2_items %>%
  mutate(all_of(across(set2, ~ case_when(
    . == 7 ~ 3,
    . == 6 ~ 3,
    . == 5 ~ 2,
    . == 4 ~ 2,
    . == 3 ~ 1,
    . == 2 ~ 1,
    . == 1 ~ 0, 
    TRUE ~ NA_integer_
  ))))

# Add catch item to catch_items.csv file.
add_catch_item(d$num_cell_tot_1, d$cr4_1.x)

rio::export(
  bdi2_items,
  here::here("data", "prep", "quest_scales", "bdi2_items.csv")
)

# eof ----
