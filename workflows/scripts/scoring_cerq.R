library(tidyverse)
library(rio)
library(here)

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

cerq_items <- rio::import(
  here::here("data", "prep", "quest_scales", "cerq_items.csv")
)

# Coding from:
# https://www.frontiersin.org/articles/10.3389/fpsyg.2017.02075/full

# Adaptive strategies
# 
# Positive refocusing
# Refocus on planning
# Positive reappraisal
# Putting into perspective

# Maladaptive strategies
# 
# Acceptance
# Rumination
# Self-blame
# Catastrophizing
# Other-blame

df <- cerq_items[, 2:37]

# Create a vector to store the subscale sums
subscale_sums <- matrix(NA, nrow = nrow(df), ncol = 9)

# Iterate over each subscale
for (i in 1:9) {
  # Calculate the sum of items for the current subscale
  start_index <- (i - 1) * 4 + 1
  end_index <- i * 4
  subscale_sums[, i] <- rowSums(df[, start_index:end_index])
}

cerq_subscales <- subscale_sums |> 
  as.data.frame()

colnames(cerq_subscales) <- c(
  "self_blame", "acceptance", "rumination", "positive_refocusing", 
  "refocus_on_planning", "positive_reappraisal", "putting_into_perspective",
  "catastrophizing", "other_blame"
)

rio::export(
  cerq_subscales, 
  here::here("data", "prep", "quest_scales", "cerq_scores.csv")
)


