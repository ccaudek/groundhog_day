suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
  library(here)
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

dass21_items <- rio::import(
  here::here("data", "prep", "quest_scales", "dass21_items.csv")
)

dass21_subscales <- recode_dass21_numeric(dass21_items)

rio::export(
  dass21_subscales, 
  here::here("data", "prep", "quest_scales", "dass21_scores.csv")
)

# eof ----

