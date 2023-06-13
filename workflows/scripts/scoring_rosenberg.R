suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

rosenberg_items <- rio::import(
  here::here("data", "prep", "quest_scales", "rosenberg_items.csv")
)

rses <- recode_rosenberg_numeric(rosenberg_items)

rio::export(
  rses, 
  file = snakemake@output[["csv"]]
  # here::here("data", "prep", "quest_scales", "rosenberg_scores.csv")
)


# hist(rses$rosenberg)

# The scale ranges from 0-30. Scores between 15 and 25 are within normal 
# range; scores below 15 suggest low self-esteem.
# The Italian version responses are between 1 and 4 (the original response
# scale is between 0 and 3).
