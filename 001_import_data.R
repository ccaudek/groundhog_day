# Script name: 001_import_data.R
# Project: groundhog day
# Script purpose: import data from mpath
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Sun May 14 07:32:36 2023
# Last Modified Date: Sun May 14 07:32:36 2023
#
# 👉 

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("mice")
  library("rio")
})

# Source functions
source(here::here("scripts", "R", "functions", "funs_import_data.R"))


# 01. Import data --------------------------------------------------------------

d <- import_mpath_data()

# 02. Data wrangling -----------------------------------------------------------

# Create numeric variable indicating the days from the fist beep 
# (coded with 0).
d$date <- as.Date(substring(d$TIME_start, 1, 10))
d$time <- difftime(d$date, "2023-04-05", units = "days") |>
  round()
unique(sort(d$time))

# Rename variable. `image_chosen == 1` means answering yes to the question: 
# "Would I repeat the same action that I have actually carried out?".
 d$is_target_chosen <- ifelse(
  d$image_chosen == 1, 1, 0
)

# Find the subjects who have only select one option to answering the question
# option "Would I repeat the same action that I have actually carried out?"
# Always yes or always no.
# - Does it make sense to remove them? Are them the most rigid ones? Or the 
# problem is that they did not understand the task? I assume the second one.

# Compute the proportion of answering yes. If 0, it means that the person 
# would never repeat what she/he has actually done.
temp <- d |>
  group_by(subj_code_1) |>
  summarize(
    y = mean(is_target_chosen)
  )

# Select subjects with 100% yes or 0% yes.
bad_id_df <- temp[temp$y == 1 | temp$y == 0, ]
bad_id <- unique(bad_id_df$subj_code_1)
length(bad_id)

# Remove the participants who have always chosen only one single response option.
d <- d[!(d$subj_code_1 %in% bad_id), ]
length(unique(d$subj_code_1))
# [1] 240

# TODO
# - Check whether each participant has completed all the 30 trials in each 
#   session.
# - Consider whether to estimate the HDDMRL parameters of the separately for 
#   each participant by using a hierarchical model with sessions as the
#   grouping variable. 



# Group by the "subj_code_1" variable and rank the "time" column:
# Create a ranking of time within-subject (the first beep, the second beep, ...)
df_ranked <- d %>% 
  group_by(subj_code_1) %>% 
  mutate(rank = dense_rank(time))

names(df_ranked)

# Show the number of beeps for each subject.
tbl <- table(df_ranked$subj_code_1, df_ranked$rank)
tbl

# Check for duplicated rows
duplicated_rows <- duplicated(row.names(tbl))
# Use indexing to remove the duplicated rows from the original dataframe
# df_no_duplicates <- df[!duplicated_rows,]

# Define `gain` as the difference between the mood level at the beginning of 
# 30 trials and what is found at the end, as the cumulative sum of positive (+1) 
# and negative (-1) feedbacks.
df_ranked$gain <- df_ranked$coin - df_ranked$context_1

df_bysubj <- df_ranked |> 
  group_by(subj_code_1, rank) |> 
  summarize(
    mood_init = mean(context_1, na.rm = TRUE),
    how_much_control = mean(control_1, na.rm = TRUE),
    mood_post = mean(post_context_1, na.rm = TRUE),
    gain = mean(gain),
    how_long = mean(TIME_total)
  )

factor_col <- factor(df_bysubj$subj_code_1)
rand_codes <- as.integer(runif(length(levels(factor_col)))*100000)
# replace the column values with the random codes
df_bysubj$id <- rand_codes[as.integer(factor_col)]

df_bysubj$subj_code_1 <- NULL

hist(df_bysubj$gain)

df_bysubj |> 
  group_by(rank) |> 
  summarize(
    avg_gain = mean(gain, na.rm = T), 
    n = n()
  )

foo <- df_bysubj[df_bysubj$gain > -20 & df_bysubj$gain < 20, ]

fm <- lmer(gain ~ mood_post + rank + (1 + rank | id), foo)
summary(fm)

