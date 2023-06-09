# Script name: prl_mood.R
# Project: groundhog_day
# Script purpose: association between accuracy and mood
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  6 11:13:37 2023
# Last Modified Date: Fri Jun  9 08:52:17 2023
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
library("scales")
library("bmmb")



# QUESTION: 
# Is mood affected by accuracy?

# d <- readRDS("data/prep/groundhog_clean.RDS")

hist(d[d$TIME_total < 15, ]$TIME_total)

# There is no obvious increase of mood_post as a function of ema_number.
d |> 
  dplyr::filter(ema_number < 9) |> 
  group_by(ema_number) %>%
  summarise(
    sd = sd(mood_post, na.rm = TRUE),
    mood_post = mean(mood_post)
  ) |> 
  ggplot(aes(x=ema_number, y=mood_post)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mood_post-sd, ymax=mood_post+sd), width=.2)

# There is evidence of an increase of gain as a function of ema_number.
d |> 
  dplyr::filter(ema_number < 9) |> 
  group_by(ema_number) %>%
  summarise(
    sd = sd(gain, na.rm = TRUE),
    gain = mean(gain)
  ) |> 
  ggplot(aes(x=ema_number, y=gain)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=gain-sd, ymax=gain+sd), width=.2)

fm <- lmer(
  gain ~ ema_number + mood_post +
    (ema_number + mood_post | user_id),
  d
)
summary(fm)


unique_subjects <- unique(d$user_id)
num_subjects = 20
selected_subjects <- sample(unique_subjects, size = num_subjects, replace = FALSE)
selected_data <- d[d$user_id %in% selected_subjects, ]  
selected_data$epoch <- factor(selected_data$epoch)

df_for_plot <- selected_data |> 
  group_by(user_id, epoch, ema_number) |> 
  summarize(
    mood_post = mean(mood_post, na.rm = TRUE),
    gain = mean(gain, na.rm = TRUE),
    TIME_total = mean(TIME_total, na.rm = TRUE)
  ) |> 
  ungroup()

# Lattice plot for mood_post vs. days
df_for_plot |> 
  ggplot(aes(x=factor(TIME_total),y=gain, group=1)) + 
  # geom_line() +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  facet_wrap(~user_id, ncol=5) +   
  theme(strip.text.x=element_blank()) + 
  labs(x="EMA number",y="Affect")


df_bysubj <- d |> 
  group_by(user_id, ema_number) |> 
  summarize(
    mood_pre = mean(mood_pre, na.rm = TRUE),
    mood_post = mean(mood_post, na.rm = TRUE),
    gain = mean(gain, na.rm = TRUE),
    accuracy = mean(accuracy, na.rm = TRUE),
    TIME_total = mean(TIME_total, trim = 0.1,  na.rm = TRUE)
  ) |> 
  mutate(mood_post = rescale(mood_post, to = c(-1, 1))) %>%
  ungroup()

fm <- lmer(
  gain ~ mood_post + ema_number +
    (1 + mood_post + ema_number | user_id), 
  # control = lmerControl(optimizer ="Nelder_Mead"),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  df_bysubj
)
summary(fm)
plot_model(fm, "eff", terms = c("mood_post"))



fm <- lmer(
  mood_post ~ gain + ema_number + 
    (1 + gain + ema_number | user_id), 
  # control = lmerControl(optimizer ="Nelder_Mead"),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  df_bysubj
)
summary(fm)
summary(rePCA(fm))

plot_model(fm, "eff", terms = c("days", "epoch", "mood_post"))


m1 <- brm(
  gain ~ mood_post * ema_number + 
    (1 + mood_post * ema_number | user_id), 
  family = asym_laplace(),
  backend = "cmdstanr",
  data = df_bysubj
)
summary(m1)
conditional_effects(m1, "mood_post")
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

