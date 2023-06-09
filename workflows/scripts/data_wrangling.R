# Script name: data_wrangling.R
# Project: groundhog_day
# Script purpose: data cleaning
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jun  7 22:20:19 2023
# Last Modified Date: Wed Jun  7 22:20:19 2023
#
# 👉 

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

# suppressPackageStartupMessages(library("rio"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("mice"))

options(max.print = .Machine$integer.max)

# ---------------------
# Read RDS file
# ---------------------

d <- readRDS(file = snakemake@input[["rds"]])
# d <- readRDS("data/prep/groundhog_raw.RDS")

# length(unique(d$subj_code_1))
# [1] 305

# ---------------------
# Clean data
# ---------------------

# Remove variable always equal to 1.
d$has_answered <- NULL

# Correct phone numbers.
d$subj_code_1[d$subj_code_1 == "389 429 1247"] <- "3894291247"
d$subj_code_1[d$subj_code_1 == "334 8921243"] <- "3348921243"
d$subj_code_1[d$subj_code_1 == "338 883 3022"] <- "3388833022"

# Recode epoch with values 1, 2.
d$epochs <-
  ifelse(
    grepl("\\d$", d$epoch), substr(d$epoch, nchar(d$epoch), nchar(d$epoch)),
    NA
  ) |> 
  as.numeric()

d$epoch <- NULL
d <- d |> 
  dplyr::rename(epoch = epochs)

# Rename variables.
d <- d |> 
  dplyr::rename(
    user_ema_code = subj_idx,
    user_id = subj_code_1,
    # Judgment provided by subjects on each trial regarding their mood in 
    # the specific moment.
    instant_mood = item_number,
    # Judgment of the amount of the subject's control on the salient episode. 
    control = control_1,
    # Mood judgment at the beginning of the session.
    mood_pre = context_1,
    # Mood judgment at the end of the session.
    mood_post = post_context_1,
    # Final score obtained by adding to the initial score (mood at the 
    # beginning of the session) the feedbacks that had been received: 
    # +1 for rewards, -1 for punishments.
    final_score = coin
  )

d$is_target_chosen <- ifelse(d$image_chosen == 1, 1, 0)
d$image_chosen <- NULL

# Convert date into numeric variable.
d$date <- as.Date(substring(d$TIME_start, 1, 10))
d$time <- difftime(d$date, "2023-04-05", units = "days") |>
  round()
# length(unique(d$time))

# Rank the variable indicating the days of participation for each subject in 
# ascending order.
# Group by the "user_id" variable and rank the "time" column
df_ranked <- d %>%
  group_by(user_id) %>%
  mutate(days = dense_rank(time))

# Rename and remove uselses dataframe.
d <- df_ranked
rm(df_ranked)

# Remove data for which days == NA.
d <- d[!is.na(d$days), ]
# length(unique(d$user_id))
# [1] 302

# When there are 60 observations per day, I keep the first 30 ones.
# From chatGTP: the following code applies a filter to remove the rows where 
# there are exactly 60 rows (n() == 60) for a combination of subj_code_1 and 
# days, and the row number is greater than 30 (row_number() > 30). This keeps 
# only the first 30 rows for combinations where there are 60 rows.
df_selected <- d %>%
  group_by(user_id, days) %>%
  filter(!(n() == 60 & row_number() > 30)) %>%
  ungroup()

# Rename dataframe.
d <- df_selected
rm(df_selected)

# The net gain/loss produced by the score obtained at the end of the
# experiment (coin) from the mood at the beginning of the session (context_1).
d$gain <- d$final_score - d$mood_pre

# Compute accuracy defined as choosing the more rewarding stimulus from 
# the first and the second epoch (Geana et al., 2021).
# unique(d$is_target_chosen)
d$accuracy <- ifelse(
  d$epoch == 1 & d$is_target_chosen == 1 |
    d$epoch == 2 & d$is_target_chosen == 0,
  1, 0
)

# foo <- d |> 
#   group_by(user_id) |> 
#   summarize(
#     acc = mean(accuracy)
#   )
# hist(foo$acc)

# Plot avg choice of "I would repeat the same choice" as a function of trial number.
# out <- df_ranked |>
#     group_by(trial) |>
#     summarize(
#         y1 = mean(is_target_chosen, na.rm = TRUE)
#     )
# 
# plot(out$trial, out$y1, type = "l")


# Find subjects who have always select only the option "I would repeat the same
# action as I did", or only selected the opposite option.
# Remove participants who have always chosen only one response option.
df_bysubj_choices <- d |>
    group_by(user_id) |>
    summarize(
        avg_trg_chosen = mean(is_target_chosen)
    )

df_bad_ids <- 
  df_bysubj_choices[
    df_bysubj_choices$avg_trg_chosen == 1 | 
      df_bysubj_choices$avg_trg_chosen == 0, ]

bad_ids <- unique(df_bad_ids$user_id)

d <- d[!(d$user_id %in% bad_ids), ]
rm(df_bysubj_choices, df_bad_ids, bad_ids)
# length(unique(d$user_id))
# [1] 290

# Remove subjects who completed the task only once.
# From chatGTP: group the data by subj_code_1 using group_by(). Then, apply 
# a filter to keep only the groups where there is at least one observation 
# with days equal to 1 (any(days == 1)) and at least one observation with days 
# equal to 2 (any(days == 2)). This ensures that the selected levels of 
# subj_code_1 have at least the levels 1, 2, 3, 4 for days.
df <- d %>%
  group_by(user_id) %>%
  filter(
    any(days == 1) & any(days == 2) & 
      any(days == 3) & any(days == 4) 
    ) %>%
  ungroup()

# length(unique(df$user_id))
# [1] 256

# Clean up
d <- df
rm(df)

# length(unique(df$user_id))

# Clean up RTS.
# RTs for the judgment of the mood in the particular moment ("how do you feed 
# in this moment?")
d$rt_inst <- d$rt / 1000
d$rt_inst <- ifelse(d$rt_inst > 10 | d$rt_inst < 0.25, NA, d$rt_inst)

# RTs for the PRL task.
d$rt <- d$rt_choice / 1000
d$rt <- ifelse(d$rt > 15 | d$rt < 0.25, NA, d$rt)
d$rt_choice <- NULL
# hist(d$rt)

# Remove useless columns of DataFrame.
df_clean <- d |>
  dplyr::select(-c(starts_with("V")))

# Perform multiple imputation.
set.seed(124)
imp <- mice(df_clean, m = 1, method = "norm")
# Access the completed imputed data
dd <- complete(imp)

# ---------------------
# Remove outliers
# ---------------------

# Outlier detection by using Mahalanobis distance on the following 
# data: mood_pre, mood_post, instant_mood, sd(instant_mood).

mood_dat <- dd |> 
  group_by(user_id, days) |> 
  summarize(
    mood_pre = mean(mood_pre),
    mood_post = mean(mood_post),
    mood_ist = mean(instant_mood),
    mood_ist_sd = sd(instant_mood)
  ) |> 
  ungroup()

mydat <- mood_dat[, 3:6]
md <- mahalanobis(mydat, center = colMeans(mydat), cov = cov(mydat))
alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(mydat)))
names_outliers_MH <- which(md > cutoff)

mood_bad_ids <- mood_dat[names_outliers_MH, ]$user_id |> 
  as.numeric()

df_clean <- dd[!(dd$user_id %in% mood_bad_ids), ]

# length(unique(df_clean$user_id))
# [1] 243

# ---------------------
# Save RDS file
# ---------------------

saveRDS(df_clean, file = snakemake@output[["clean"]])


# eof ----
