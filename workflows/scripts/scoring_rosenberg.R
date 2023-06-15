# Rosenberg Self-Esteem Scale.

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

rosenberg_items <- rio::import(
  here::here("data", "prep", "quest_scales", "rosenberg_items.csv")
)

# Source rosenberg.R on GitHub, which includes the function scoring_rosenberg().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/rosenberg.R"
)

rses <- scoring_rosenberg(rosenberg_items)

rio::export(
  rses, 
  file = snakemake@output[["csv"]]
  # here::here("data", "prep", "quest_scales", "rosenberg_scores.csv")
)

# eof ----
