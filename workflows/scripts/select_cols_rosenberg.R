# Rosenberg

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
})

# d <- rio::import(here::here("data", "prep", "quest.csv"))
d <- rio::import(snakemake@input[["quest"]])

rosenberg_items <- d |> 
  dplyr::select("num_cell_tot_1", starts_with("ros")) |> 
  dplyr::rename(user_id = num_cell_tot_1)

item_names <- paste0("ros_", 1:10)

colnames(rosenberg_items) <- c("user_id", item_names)

rio::export(
  rosenberg_items, 
  # here::here("data", "prep", "quest_scales", "rosenberg_items.csv")
  snakemake@output[["csv"]]
)

