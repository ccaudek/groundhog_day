# Script name: prl_mood.R
# Project: groundhog_day
# Script purpose: association between accuracy and mood
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  6 11:13:37 2023
# Last Modified Date: Tue Jun  6 11:13:37 2023
#
# 👉 

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

d <- readRDS("data/prep/groundhog_clean.RDS")

library("tidyverse")
library("sjPlot")
library("sjstats")
library("lme4")
library("brms")
library("effectsize")


# QUESTION: 
# Is mood affected by accuracy?

# d <- readRDS("data/prep/groundhog_clean.RDS")

d$mood_change <- d$mood_post - d$mood_pre

d$subj_day <- paste(d$user_id, d$days, sep="")
d$epoch <- ifelse(d$epoch == 1, 0, 1)
d$epoch <- factor(d$epoch)

unique_subjects <- unique(d$user_id)
num_subjects = 40
selected_subjects <- sample(unique_subjects, size = num_subjects, replace = FALSE)
selected_data <- d[d$user_id %in% selected_subjects, ]  
selected_data$epoch <- factor(selected_data$epoch)

fm <- lmer(
  accuracy ~ epoch * mood_post * days + 
    (1 + epoch + mood_post + days | user_id) + (1 | subj_day), 
  # control = lmerControl(optimizer ="Nelder_Mead"),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  selected_data
)

fm <- lmer(
  mood_post ~ epoch * accuracy * days + 
    (1 + epoch * accuracy * days | user_id) + (1 | subj_day), 
  # control = lmerControl(optimizer ="Nelder_Mead"),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  d
)
summary(fm)
get_model_pval(fm, kr=TRUE)
summary(rePCA(fm))

plot_model(fm, "eff", terms = c("days", "epoch", "mood_post"))


df_by_subj <- d |> 
  group_by(user_id, epoch, days) |> 
  summarize(
    mood_post = mean(mood_post),
    mood_pre = mean(mood_pre),
    mood_change = mean(mood_change),
    accuracy = mean(accuracy),
    gain = mean(gain)
  ) |> 
  ungroup()

df_by_subj <- df_by_subj |> 
  dplyr::filter(days < 9)

fm <- lmer(
  gain ~ mood_pre + mood_post * days * epoch + 
    (1 + mood_post * epoch | user_id) + (1 | days), 
  # control = lmerControl(optimizer ="Nelder_Mead"),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  df_by_subj
)
summary(fm)
plot_model(fm, "eff", terms = c("days", "mood_post"))

m1 <- brm(
  gain ~ mood_pre + mood_post * days * epoch + 
    (1 + mood_post * epoch | user_id) + (1 | days), 
  family = asym_laplace(),
  backend = "cmdstanr",
  data = df_by_subj
)
summary(m1)
conditional_effects(m1, "mood_post:days")
pp_check(m1)








df_days_by_subj <- d |> 
  group_by(user_id, days) |> 
  summarize(
    mood_post = mean(mood_post),
    mood_pre = mean(mood_pre),
    mood_change = mean(mood_change),
    accuracy = mean(accuracy),
    gain = mean(gain)
  ) |> 
  ungroup()

fm2 <- lmer(
  mood_post ~ days + 
    (1 + days | user_id), 
  # control = lmerControl(optimizer ="Nelder_Mead"),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  df_days_by_subj
)

summary(fm2)

m1 <- brm(
  mood_post ~ days + (1 + days | user_id), 
  family=asym_laplace(),
  backend="cmdstanr",
  data = df_days_by_subj
)
summary(m1)
conditional_effects(m1, "days")
pp_check(m1)

m2 <- brm(
  gain ~ (mood_pre + mood_post) * days +
    (1 + mood_pre + mood_post + days | user_id),
  family = gaussian(),
  backend = "cmdstanr",
  data = df_by_subj
)

summary(m2)
conditional_effects(m2, "mood_post")
conditional_effects(m2, "days")
performance::r2_bayes(m2)
performance::r2_loo(m2)

