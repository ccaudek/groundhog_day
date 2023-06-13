library(tidyverse)
library(rio)
library(here)

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

scs_items <- rio::import(
  here::here("data", "prep", "quest_scales", "scs_items.csv")
)

scs <- recode_scs_numeric(scs_items)

rio::export(
  scs, 
  here::here("data", "prep", "quest_scales", "scs_scores.csv")
)
