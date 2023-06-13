library(tidyverse)
library(rio)
library(here)

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

cope_items <- rio::import(
  here::here("data", "prep", "quest_scales", "cope_items.csv")
)

cope_nvi <- recode_cope_numeric(cope_items)

rio::export(
  cope_nvi, 
  here::here("data", "prep", "quest_scales", "cope_scores.csv")
)
