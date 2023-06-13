# Script name: hddmrl_data.R
# Project: groundhog_day
# Script purpose: get data for haddm analysis
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  6 08:29:46 2023
# Last Modified Date: Tue Jun  6 08:29:46 2023
#
# 👉 

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

suppressPackageStartupMessages(library("rio"))
suppressPackageStartupMessages(library("tidyverse"))

# ---------------------
# Read RDS file
# ---------------------

d <- readRDS(file = snakemake@input[["clean"]])
# d <- readRDS("data/prep/groundhog_clean.RDS")

# ---------------------
# Add required columns for HDDMrl
# ---------------------

d$subj_idx <- as.numeric(factor(as.character(d$user_id)))

# 'trial' must be a vector from 1 to nj, where nj is the max number of 
# trials for each participant.
df_indexed <- d %>%
    group_by(subj_idx) %>%
    mutate(row_index = row_number())

df_indexed$trial_in_block <- df_indexed$trial
df_indexed$trial <- df_indexed$row_index

# Perhaps, the extreme values (-50, +50) should be considered as NA and
# imputation is necessary.
# hist(df_indexed$mood_pre)

q <- quantile(df_indexed$mood_post, c(0, 1 / 3, 2 / 3, 1))

# maybe use mood_pre? Or the difference mood_post - mood_pre?
df_indexed$fmood <- ifelse(
  df_indexed$mood_post < q[2], "low",
    ifelse(
      df_indexed$mood_post >= q[2] & df_indexed$mood_post < q[3], "med", "high")
) |>
    as.factor()

df_indexed$fmood <- factor(df_indexed$fmood, levels = c("low", "med", "high"))

# table(df_indexed$fmood)

# hist(log(df_indexed$rt_inst))
# hist(log(df_indexed$rt))

# fm <- lme4::lmer(
#   log(rt_inst) ~ trial_in_block + instant_mood + days +
#     (1 + trial_in_block + days + instant_mood | subj_idx), 
#   df_indexed
# )
# Given a strong relation between instant moood and RTs for this response,
# I could filter subjects so as to remove those who do not show this relation
# because of careless responding.

# foo <- df_indexed |>
#     group_by(user_id, days, fmood) |>
#     summarize(
#         mood_post = mean(mood_post),
#         gain = mean(gain)
#     ) |> 
#   ungroup()
# 
# foo$gain <- ifelse(foo$gain < -20 | foo$gain > 20, mean(foo$gain), foo$gain)
# fm <- lmer(gain ~ days + mood_post + (1 + days | user_id), foo)

df_indexed$mood_num <- as.numeric(df_indexed$fmood)
# low: 1; med: 2, high: 3

# Code the response column with [stimulus-coding].
df_indexed$response <- df_indexed$is_target_chosen
# Include a column called ‘split_by’ which identifies the different task 
# conditions (as integers), to ensure reward updating will work properly 
# for each condition without mixing values learned from one trial type to 
# another.  (e.g. if you have stimulus A and get reward you want that 
# updated value to impact choice only for the next stimulus A trial but 
# not necessarily the immediate trial afterwards, which may be of a 
# different condition)
df_indexed$split_by <- df_indexed$is_target_chosen 
df_indexed$q_init <- 0.5

df_sorted <- df_indexed[
  order(df_indexed$subj_idx, df_indexed$mood_num, df_indexed$trial), ]

for_hddm_df <- df_sorted |>
  dplyr::select(
    subj_idx, user_id, epoch, ema_number, control, 
    fmood, mood_num, mood_pre, mood_post,
    rt, rt_inst, 
    response, feedback, split_by, q_init, 
    trial_in_block, trial
  ) |> 
  ungroup()

# ---------------------
# Save CSV file
# ---------------------

rio::export(for_hddm_df, file = snakemake@output[["hddmrl"]])
# rio::export(for_hddm_df, "data/prep/groundhog_hddmrl_data.csv")


# eof ----

# 
# mood_dat <- df_indexed |> 
#   group_by(user_id, days) |> 
#   summarize(
#     mood_pre = mean(mood_pre),
#     mood_post = mean(mood_post),
#     mood_ist = mean(instant_mood),
#     mood_ist_sd = sd(instant_mood)
#   ) |> 
#   ungroup()
# 
# maha <- outlier(mood_dat[, 3:6]) 
# maha[maha > 21]
# bad_maha_mood <- c(357, 555, 776, 1087, 1249 )
# 
# mood_bad_ids <- out[bad_maha_mood, ]$user_id |> 
#   as.numeric()
# 
# mydat <- mood_dat[, 3:6]
# md <- mahalanobis(mydat, center = colMeans(mydat), cov = cov(mydat))
# alpha <- .001
# cutoff <- (qchisq(p = 1 - alpha, df = ncol(mydat)))
# names_outliers_MH <- which(md > cutoff)
# 
# mood_bad_ids <- mood_dat[names_outliers_MH, ]$user_id |> 
#   as.numeric()
# 
# 
# 
# 
# #######################
# 
# 
# 
# foo <- df_sorted |> 
#   group_by(user_id, days) |> 
#   summarize(
#     rt_i = mean(rt_inst),
#     rt = mean(rt),
#     gain = mean(gain),
#     is_target_chosen = mean(is_target_chosen),
#     sd_imood = sd(instant_mood)
#   )
# 
# plot(density(foo$rt_i))
# plot(density(foo$rt))
# plot(density(foo$sd_imood))
# 
# foo[foo$sd_imood == 0, ] |> 
#   data.frame()
# 
# unique(foo[foo$sd_imood == 0, ]$user_id)
# 
# mahalanobis(cov(foo[, 3:7]))
# 
# library(MASS)
# 
# mydat <- foo[, 3:7]
# 
# md <- mahalanobis(mydat, center = colMeans(mydat), cov = cov(mydat))
# alpha <- .001
# cutoff <- (qchisq(p = 1 - alpha, df = ncol(mydat)))
# names_outliers_MH <- which(md > cutoff)
# 
# output95 <- cov.mcd(mydat, quantile.used = nrow(mydat)*.999)
# 
# mhmcd95 <- mahalanobis(mydat, output95$center, output95$cov)
# names_outlier_MCD95 <- which(mhmcd95 > cutoff)
# names_outlier_MCD95
# 
# vars_for_outlier_detection <- df_sorted |> 
#   dplyr::select(
#       "instant_mood"        
#     , "rt"                 
#     , "feedback"               
#     , "mood_pre"           
#     , "control"             
#     , "mood_post"                  
#     , "TIME_total"         
#     , "is_target_chosen"           
#     # , "days"              
#     , "rt_inst"         
#     #, "response"
#   )
# vars_for_outlier_detection$subj_idx <- NULL
# 
# maha <- outlier(vars_for_outlier_detection)
# 
# md <- mahalanobis(
#   vars_for_outlier_detection, 
#   center = colMeans(vars_for_outlier_detection), 
#   cov = cov(vars_for_outlier_detection)
# )
# 
# alpha <- .0001
# cutoff <- (qchisq(p = 1 - alpha, df = ncol(vars_for_outlier_detection)))
# names_outliers_MH <- which(md > cutoff)
