# Add item to catch_items.csv --------------------------------------------------

add_catch_item <- function(user_id, catch_item) {
  
  suppressPackageStartupMessages({
    library("here")
    library("tidyverse")
    library("rio")
  })
  
  directory <- here::here("data", "prep", "quest_scales")
  file_name <- "catch_items.csv"

  # Check if the file exists
  file_path <- file.path(directory, file_name)
  if (file.exists(file_path)) {
    old_dat <- rio::import(
      file_path
    )

    new_dat <- data.frame(
      user_id, catch_item
    )

    both_dat <- full_join(old_dat, new_dat, by = "user_id")

    rio::export(both_dat, file_path)
  } else {
    new_dat <- data.frame(
      user_id, catch_item
    )

    rio::export(new_dat, file_path)
  }
}


#' # Recode Rosenberg Self-Esteem Scale --------------------------------------
#' 
#' #' @description scoring Rosenberg self-esteem scale.
#' #' @param data.frame.
#' #' @return data.frame.
#' recode_rosenberg <- function(d) {
#'   rses <- d %>%
#'     mutate(
#'       across(
#'         c(ros_1, ros_2, ros_4, ros_6, ros_7),
#'         ~ recode(.,
#'           "Fortemente in disaccordo" = 0L,
#'           "In disaccordo"            = 1L,
#'           "D'accordo"                = 2L,
#'           "Fortemente d'accordo"     = 3L
#'         )
#'       )
#'     ) %>%
#'     mutate(
#'       across(
#'         c(ros_3, ros_5, ros_8, ros_9, ros_10),
#'         ~ recode(.,
#'           "Fortemente in disaccordo" = 3L,
#'           "In disaccordo"            = 2L,
#'           "D'accordo"                = 1L,
#'           "Fortemente d'accordo"     = 0L
#'         )
#'       )
#'     )
#' 
#'   # Compute total score.
#'   rses <- rses %>%
#'     rowwise() %>%
#'     mutate(
#'       ros_tot = sum(c_across(ros_1:ros_10))
#'     )
#' 
#'   # Add subj_code.
#'   rses$subj_code <- d$subj_code
#' 
#'   rses
#' }
#' 
#' #' @description scoring Rosenberg self-esteem scale.
#' #' @param data.frame.
#' #' @return data.frame.
#' recode_rosenberg_numeric <- function(d) {
#'   rses <- d %>%
#'     # reversed items
#'     mutate(
#'       across(
#'         c(ros_3, ros_5, ros_8, ros_9, ros_10),
#'         ~ case_match(
#'           .,
#'           1 ~ 4L,
#'           2 ~ 3L,
#'           3 ~ 2L,
#'           4 ~ 1L
#'         )
#'       )
#'     )
#' 
#'   # Compute total score.
#'   rses <- rses %>%
#'     rowwise() %>%
#'     mutate(
#'       ros_tot = sum(c_across(ros_1:ros_10))
#'     )
#' 
#'   rses_final <- data.frame(
#'     user_id = rses$user_id,
#'     rosenberg = rses$ros_tot
#'   )
#' 
#'   # Add subj_code.
#'   # rses$user_id <- d$user_id
#' 
#'   rses_final
#' }
#' 
#' 
#' # Recode DASS-21 ----------------------------------------------------------
#' 
#' #' @description recode DASS-21
#' #' cut-offs: https://maic.qld.gov.au/wp-content/uploads/2016/07/DASS-21.pdf
#' #' @param data.frame.
#' #' @return data.frame.
#' recode_dass21 <- function(d) {
#'   # Recode values from alphanumeric to numeric for all columns in the
#'   # dataframe.
#'   d <- d %>%
#'     mutate_all(
#'       ~ recode(
#'         .,
#'         "Non mi è mai accaduto"                 = 0L,
#'         "Mi è capitato qualche volta"           = 1L,
#'         "Mi è capitato con una certa frequenza" = 2L,
#'         "Mi è capitato quasi sempre"            = 3L
#'       )
#'     )
#' 
#'   # Stress
#'   d$dass21_s <-
#'     with(
#'       d,
#'       dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18
#'     )
#' 
#'   # Anxiety
#'   d$dass21_a <-
#'     with(
#'       d,
#'       dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20
#'     )
#' 
#'   # Depression
#'   d$dass21_d <-
#'     with(
#'       d,
#'       dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21
#'     )
#' 
#'   d
#' }
#' 
#' 
#' # Recode DASS-21 numeric -------------------------------------------------------
#' 
#' #' @description recode DASS-21
#' #' cut-offs: https://maic.qld.gov.au/wp-content/uploads/2016/07/DASS-21.pdf
#' #' @param data.frame.
#' #' @return data.frame.
#' recode_dass21_numeric <- function(d) {
#'   # Stress
#'   d$dass21_s <-
#'     with(
#'       d,
#'       dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18
#'     )
#' 
#'   # Anxiety
#'   d$dass21_a <-
#'     with(
#'       d,
#'       dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20
#'     )
#' 
#'   # Depression
#'   d$dass21_d <-
#'     with(
#'       d,
#'       dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21
#'     )
#' 
#'   dass21_subscales <- d |>
#'     dplyr::select(user_id, dass21_s, dass21_a, dass21_d)
#' 
#'   dass21_subscales
#' }
#' 
#' 
#' # Recode COPE NVI 60 ------------------------------------------------------
#' 
#' recode_cope_numeric <- function(d) {
#'   # Social support
#'   d$social_support <-
#'     (d$cope_4 + d$cope_14 + d$cope_30 +
#'       d$cope_45 + d$cope_11 + d$cope_23 +
#'       d$cope_34 + d$cope_52 + d$cope_3 +
#'       d$cope_17 + d$cope_28 + d$cope_46)
#' 
#'   # Avoiding strategies
#'   d$avoiding_strategies <-
#'     (d$cope_6 + d$cope_27 + d$cope_40 +
#'       d$cope_57 + d$cope_9 + d$cope_24 +
#'       d$cope_37 + d$cope_51 + d$cope_2 +
#'       d$cope_16 + d$cope_31 + d$cope_43 +
#'       d$cope_12 + d$cope_26 + d$cope_35 +
#'       d$cope_53)
#' 
#'   # Positive attitude
#'   d$positive_attitude <- (d$cope_10 + d$cope_22 +
#'     d$cope_41 + d$cope_49 + d$cope_1 +
#'     d$cope_29 + d$cope_38 + d$cope_59 +
#'     d$cope_13 + d$cope_21 + d$cope_44 +
#'     d$cope_54)
#' 
#'   # Problem orientation
#'   d$problem_orientation <- (d$cope_5 + d$cope_25 + d$cope_47 +
#'     d$cope_58 + d$cope_19 + d$cope_32 +
#'     d$cope_39 + d$cope_56 + d$cope_15 +
#'     d$cope_33 + d$cope_42 + d$cope_55)
#' 
#'   # Transcendent orientation
#'   d$transcendent_orientation <- (abs(d$cope_8 - 5) +
#'     abs(d$cope_20 - 5) + abs(d$cope_36 - 5) +
#'     abs(d$cope_50 - 5) + d$cope_7 +
#'     d$cope_18 + d$cope_48 + d$cope_60)
#' 
#'   cope_final <- data.frame(
#'     user_id = d$user_id,
#'     social_support = d$social_support,
#'     avoiding_strategies = d$avoiding_strategies,
#'     positive_attitude = d$positive_attitude,
#'     problem_orientation = d$problem_orientation,
#'     transcendent_orientation = d$transcendent_orientation
#'   )
#' 
#'   # Add subj_code.
#'   # rses$user_id <- d$user_id
#'   cope_final
#' }
#' 
#' # Recode SCS --------------------------------------------------------------
#' 
#' recode_scs_numeric <- function(d) {
#'   # Self-Kindness
#'   d$self_kindness <-
#'     d$scs_5 + d$scs_12 + d$scs_19 +
#'     d$scs_23 + d$scs_26
#' 
#'   # Self-Judgment
#'   d$self_judgment <-
#'     abs(d$scs_1 - 6) + abs(d$scs_8 - 6) +
#'     abs(d$scs_11 - 6) + abs(d$scs_16 - 6) +
#'     abs(d$scs_21 - 6)
#' 
#'   # Common Humanity
#'   d$common_humanity <-
#'     d$scs_3 + d$scs_7 + d$scs_10 + d$scs_15
#' 
#'   # Isolation
#'   d$isolation <-
#'     abs(d$scs_4 - 6) + abs(d$scs_13 - 6) +
#'     abs(d$scs_18 - 6) + abs(d$scs_25 - 6)
#' 
#'   # Mindfulness
#'   d$mindfulness <-
#'     d$scs_9 + d$scs_14 + d$scs_17 + d$scs_22
#' 
#'   # Overidentification
#'   d$over_identification <-
#'     abs(d$scs_2 - 6) + abs(d$scs_6 - 6) +
#'     abs(d$scs_20 - 6) + abs(d$scs_24 - 6)
#' 
#'   d$neg_self_compassion <- d$self_judgment +
#'     d$isolation +
#'     d$over_identification
#' 
#'   d$pos_self_compassion <- d$self_kindness +
#'     d$common_humanity +
#'     d$mindfulness
#' 
#'   # The sk, ch, mi, sj, is, oi variables are the *not reversed* scores of the
#'   # six SCS subscales.
#'   d$sk <- d$self_kindness
#'   d$ch <- d$common_humanity
#'   d$mi <- d$mindfulness
#' 
#'   d$sj <- d$scs_1 + d$scs_8 +
#'     d$scs_11 + d$scs_16 + d$scs_21
#' 
#'   d$is <- d$scs_4 + d$scs_13 +
#'     d$scs_18 + d$scs_25
#' 
#'   d$oi <- d$scs_2 + d$scs_6 +
#'     d$scs_20 + d$scs_24
#' 
#'   d$scs_ts <-
#'     d$neg_self_compassion + d$pos_self_compassion
#' 
#'   scs_final <- data.frame(
#'     user_id = d$user_id,
#'     self_kindness = d$sk,
#'     common_humanity = d$ch,
#'     mindfulness = d$mi,
#'     self_judgment = d$sj,
#'     isolation = d$is,
#'     over_identification = d$oi,
#'     scs_total_score = d$scs_ts
#'   )
#' 
#'   scs_final
#' }
#' 
#' 
#' # Recode Social Interaction Anxiety Scale (SIAS) --------------------------
#' 
#' #' @description recode Social Interaction Anxiety Scale (SIAS)
#' #' @param data.frame.
#' #' @return data.frame.
#' recode_sias <- function(d) {
#'   # change columns names
#'   sias_all <- d[, c(73:91)]
#'   col_names <- sprintf("sias_%s", seq(1:19))
#'   colnames(sias_all) <- col_names
#' 
#'   # Recode values from alphanumeric to numeric.
#'   sias_all <- sias_all %>%
#'     mutate_all(
#'       ~ recode(
#'         .,
#'         "Per nulla"  = 0L,
#'         "Poco"       = 1L,
#'         "Abbastanza" = 2L,
#'         "Molto"      = 3L,
#'         "Moltissimo" = 4L
#'       )
#'     )
#' 
#'   # Reverse items 8 and 10.
#'   sias_all$sias_8 <- misty::item.reverse(
#'     sias_all$sias_8,
#'     min = 0,
#'     max = 4
#'   )
#' 
#'   sias_all$sias_10 <- misty::item.reverse(
#'     sias_all$sias_10,
#'     min = 0,
#'     max = 4
#'   )
#' 
#'   # Compute total score.
#'   sias_all <- sias_all %>%
#'     rowwise() %>%
#'     mutate(
#'       sias_tot = sum(c_across(sias_1:sias_19))
#'     )
#' 
#'   # Add subj_code.
#'   sias_all$subj_code <- d$subj_code
#' 
#'   sias_all
#' }
#' 
#' 
#' # Recode MPS --------------------------------------------------------------
#' 
#' #' @description recode MPS
#' #' @param data.frame.
#' #' @return data.frame.
#' #' The four subscales are:
#' #' Concern over mistakes and doubts about actions
#' #' (Questions 9,10,13,14, 17,18,21,23,25,28,32,33,34)
#' #' Excessive concern with parents’ expectations and evaluation
#' #' (Questions 1,3,5,11,15,20,22,26,35)
#' #' Excessively high personal standards
#' #' (Questions 4,6,12,16,19,24,30)
#' #' Concern with precision, order and organisation
#' #' (Questions, 2,7,8,27,29,31)
#' #' Frost, R. O., & Marten, P. A. (1990). Perfectionism and evaluative threat.
#' #' Cognitive Therapy and Research, 14, 559-572.
#' 
#' recode_mps <- function(d) {
#'   # change columns names
#'   mps_all <- d[, c(92:126)]
#'   col_names <- sprintf("mps_%s", seq(1:35))
#'   colnames(mps_all) <- col_names
#' 
#'   # Recode values from alphanumeric to numeric.
#'   mps_all <- mps_all %>%
#'     mutate_all(
#'       ~ recode(
#'         .,
#'         "Forte disaccordo"           = 1L,
#'         "Disaccordo"                 = 2L,
#'         "Né d'accordo né disaccordo" = 3L,
#'         "D'accordo"                  = 4L,
#'         "Forte accordo"              = 5L
#'       )
#'     )
#' 
#'   # CMD
#'   mps_all$mps_cmd <- with(
#'     mps_all,
#'     mps_9 + mps_10 + mps_13 + mps_14 + mps_17 + mps_18 + mps_21 +
#'       mps_23 + mps_25 + mps_28 + mps_32 + mps_33 + mps_34
#'   )
#' 
#'   # PS
#'   mps_all$mps_ps <- with(
#'     mps_all,
#'     mps_4 + mps_6 + mps_12 + mps_16 + mps_19 + mps_24 + mps_30
#'   )
#' 
#'   # PEPC
#'   mps_all$mps_pepc <- with(
#'     mps_all,
#'     mps_1 + mps_3 + mps_5 + mps_11 + mps_15 + mps_20 + mps_22 + mps_26 +
#'       mps_35
#'   )
#' 
#'   # OR
#'   mps_all$mps_or <- with(
#'     mps_all,
#'     mps_2 + mps_7 + mps_8 + mps_27 + mps_29 + mps_31
#'   )
#' 
#'   mps_all$mps_tot <- with(
#'     mps_all,
#'     mps_cmd + mps_ps + mps_pepc + mps_or
#'   )
#' 
#'   # Add subj_code.
#'   mps_all$subj_code <- d$subj_code
#' 
#'   mps_all
#' }
#' 
#' 
#' # Score EAT-26 ------------------------------------------------------------
#' 
#' #' @description recode EAT-26 items
#' #' @param data.frame.
#' #' @return data.frame.
#' #' https://www.nyeatingdisorders.org/pdf/EAT-26IntpretScoring-Test-3-20-10.pdf
#' #' https://jeatdisord.biomedcentral.com/articles/10.1186/s40337-022-00580-3
#' recode_eat26 <- function(d) {
#'   # change columns names
#'   eat26_all <- d[, c(147:172)]
#'   col_names <- sprintf("eat26_%s", seq(1:26))
#'   colnames(eat26_all) <- col_names
#' 
#'   item_from1_to25 <- eat26_all[, 1:25]
#'   item_26 <- eat26_all[, 26]
#' 
#'   # Recode values from alphanumeric to numeric.
#' 
#'   # Scoring for items 1:25.
#'   item_from1_to25 <- item_from1_to25 %>%
#'     mutate_all(
#'       ~ recode(
#'         .,
#'         "Sempre"        = 3L,
#'         "Molto spesso"  = 2L,
#'         "Spesso"        = 1L,
#'         "Qualche volta" = 0L,
#'         "Raramente"     = 0L,
#'         "Mai"           = 0L
#'       )
#'     )
#' 
#'   # Scoring for item 26.
#'   item_26 <- item_26 %>%
#'     mutate_all(
#'       ~ recode(
#'         .,
#'         "Sempre"        = 0L,
#'         "Molto spesso"  = 0L,
#'         "Spesso"        = 0L,
#'         "Qualche volta" = 1L,
#'         "Raramente"     = 2L,
#'         "Mai"           = 3L
#'       )
#'     )
#' 
#'   eat26_all <- bind_cols(item_from1_to25, item_26)
#' 
#'   # Compute total score.
#'   eat26_all <- eat26_all %>%
#'     rowwise() %>%
#'     mutate(
#'       eat26_tot = sum(c_across(eat26_1:eat26_26))
#'     )
#' 
#'   eat26_all$eat26_at_risk <- ifelse(eat26_all$eat26_tot > 19, 1, 0)
#' 
#'   # Pathological avoidance of fattening foods and shape preoccupations; individuals who
#'   # score high on this factor may be described as overestimators of their body size and
#'   # who are dissatisfied with their shape and desire to be smaller.
#'   eat26_all$dieting <- with(
#'     eat26_all,
#'     eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14,
#'     eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26
#'   )
#' 
#'   # bulimia and food preoccupation: similar to the previous factor, but is positively
#'   # related to bulimia and heavier body weight. High scores on this factor may be
#'   # associated with poor outcome.
#'   eat26_all$bulimia <- with(
#'     eat26_all,
#'     eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
#'   )
#' 
#'   # factor largely comprised of items relecting self-control about food as well as
#'   # those who acknowledge social pressure to gain weight. High scores on this factor
#'   # are related to lower weight and absence of bulimia.
#'   eat26_all$oral_control <- with(
#'     eat26_all,
#'     eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
#'   )
#' 
#'   # Add subj_code.
#'   eat26_all$subj_code <- d$subj_code
#' 
#'   eat26_all
#' }
#' 
#' 
#' # Recode BSQ-14 -----------------------------------------------------------
#' 
#' #' @description recode BSQ-14
#' #' @return data.frame.
#' recode_bsq14 <- function(d) {
#'   # change columns names
#'   bsq14_all <- d[, c(28:41)]
#'   col_names <- sprintf("bsq_%s", seq(1:14))
#'   colnames(bsq14_all) <- col_names
#'   # Recode values from alphanumeric to numeric.
#' 
#'   bsq14_all <- bsq14_all %>%
#'     mutate_all(list(~ recode(.,
#'       "Mai"           = 1L,
#'       "Quasi mai"     = 2L,
#'       "Qualche volta" = 3L,
#'       "Spesso"        = 4L,
#'       "Quasi sempre"  = 5L,
#'       "Sempre"        = 6L
#'     )))
#' 
#'   bsq14_all <- bsq14_all %>%
#'     mutate_all(
#'       ~ recode(
#'         .,
#'         "Mai"           = 1L,
#'         "Quasi mai"     = 2L,
#'         "Qualche volta" = 3L,
#'         "Spesso"        = 4L,
#'         "Quasi sempre"  = 5L,
#'         "Sempre"        = 6L
#'       )
#'     )
#' 
#'   # Compute total score.
#'   bsq14_all <- bsq14_all %>%
#'     rowwise() %>%
#'     mutate(
#'       bsq14_tot = sum(c_across(bsq_1:bsq_14))
#'     )
#' 
#'   # Add subj_code.
#'   bsq14_all$subj_code <- d$subj_code
#' 
#'   bsq14_all
#' }
