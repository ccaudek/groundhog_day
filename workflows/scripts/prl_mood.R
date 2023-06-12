# Script name: prl_mood.R
# Project: groundhog_day
# Script purpose: association between accuracy and mood
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  6 11:13:37 2023
# Last Modified Date: Fri Jun  9 08:52:17 2023
#
# 👉 
#   https://www.frontiersin.org/articles/10.3389/fpsyg.2019.01698/full


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
plot_model(fm, "eff", terms = c("ema_number"))


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
    (1 + mood_post + ema_number | user_id), 
  family = asym_laplace(),
  backend = "cmdstanr",
  data = df_bysubj
)

summary(m1)
conditional_effects(m1, "mood_post:ema_number")
pp_check(m1)


hist((df_bysubj$gain))





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


hist(d$instant_mood)

foo <- d |> 
  group_by(user_id, ema_number) |> 
  summarize(
    instant_mood = mean(instant_mood),
    mood_pre = mean(mood_pre),
    mood_post = mean(mood_post),
    gain = mean(gain),
    TIME_total = mean(TIME_total)
  ) |> 
  ungroup()

fm <- lmer(
  instant_mood ~ 1 + ema_number + mood_pre +
    (1 + ema_number | user_id),
  foo
)
summary(fm)

# Lattice plot for NA vs. Performance Type
d |> 
  dplyr::filter(ema_number < 9) |> 
  group_by(trial) |> 
  summarize(
    instant_mood = mean(instant_mood)
  ) |> 
  #dplyr::filter(ema_number == 1) |> 
ggplot(aes(x=factor(trial), y=instant_mood)) + 
  geom_point() + 
  #facet_wrap(~ema_number,ncol=4) +   
  #theme(strip.text.x=element_blank()) +
  labs(x="Instant mood",y="Gain")

foo <- d |> 
  group_by(trial) |> 
  summarize(
    imood = mean(instant_mood)
  )

fm <- lm(
  imood ~ poly(trial, 3),
  foo
)

plot(foo$trial, foo$imood)
lines(sort(foo$trial),                 # Draw polynomial regression curve
      fitted(fm)[order(foo$trial)],
      col = "red",
      type = "l")

#----------------
# Instant mood.
#----------------
# Regression Discontinuity
# https://mixtape.scunning.com/06-regression_discontinuity

m0 <- lmer(
  instant_mood ~ 1 + (1 | user_id) + (1 | ema_number),
  data = d
)
summary(m0)  

icc = 0.25737 / (0.25737 + 0.07843 + 1.05783)
icc
# 18.5% of the total variability in performance anxiety scores are 
# attributable to differences among subjects. In this particular model, 
# we can also say that the average correlation for any pair of responses 
# from the same individual is a moderately low .185.

d$user_day <- paste(d$user_id, d$ema_number, sep="_")

library(splines)
m1 <- lmer(
  instant_mood ~ 1 + ns(trial, 3) + 
    (1 + ns(trial, 3) | user_id) + (1 | user_day),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  data = d
)
summary(m1) 

d$predicted <- predict(m1, newdata = d, type = "response")

df_forplot <- d |> 
  group_by(trial) |> 
  summarize(
    y = mean(instant_mood),
    yhat = mean(predicted)
  )

plot(df_forplot$trial, df_forplot$y, pch = 16, xlab = "Predictor 1", ylab = "Judgment")
lines(df_forplot$trial, df_forplot$yhat, col = "blue", lwd = 2)



m3 <- lmer(
  instant_mood ~ 1 + trial + 
    (1 + trial | user_id) + (1 | user_day),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  data = d, subset=trial < 16
)
summary(m3) 

m4 <- lmer(
  instant_mood ~ 1 + trial + 
    (1 + trial | user_id) + (1 | user_day),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  data = d, subset=trial > 15
)
summary(m4) 


df_byday <- d |> 
  group_by(user_id, ema_number) |> 
  summarize(
    control = mean(control),
    mood_pre = mean(mood_pre),
    mood_post = mean(mood_post)
  ) |> 
  ungroup()
df_byday$user_day <- paste(df_byday$user_id, df_byday$ema_number, sep="_")

bm5 <- brm(
  control ~ 1 + mood_pre + mood_post +
    (1 + mood_pre + mood_post | user_id) + (1 | ema_number),
  family = cumulative("probit"),
  data = df_byday,
  init = 0.1,
  backend = "cmdstanr"
)
pp_check(bm5)
conditional_effects(bm5, "mood_pre", categorical = TRUE)
conditional_effects(bm5, "mood_post", categorical = TRUE)
summary(bm5)

plot(density(df_byday$mood_pre))

bm6 <- brm(
  mood_pre ~ 1 + control +
    (1 + control | user_id) + (1 | ema_number),
  family = skew_normal(),
  data = df_byday,
  init = 0.1,
  backend = "cmdstanr"
)
pp_check(bm6)
conditional_effects(bm6, "control")
summary(bm6)



a <- summary(bm6)
summary_mod1 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) )
rownames(summary_mod1) <- c("$\\beta_0$", "$\\beta$", "$\\sigma_{e}$", "$\\alpha$")
colnames(summary_mod1) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "Bulk ESS", "Tail ESS")

summary_mod1 %<>%
  select(-c("Bulk ESS", "Tail ESS")) %>% # removing ESS
  rownames_to_column(var = "parameter") 

summary_mod1 <- summary_mod1 |> mutate_if(is.numeric, round, 3)

write.csv(summary_mod1, file = snakemake@output[["table_1"]], row.names = FALSE)






m5 <- lmer(
  control ~ 1 + mood_pre + mood_post +
    (1 + mood_pre + mood_post | user_id) + (1 | ema_number),
  control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
  data = df_byday
)
summary(m5) 
summary(rePCA(m5))

fm1 <- lmer(
  instant_mood ~ poly(trial, 3) +
    (poly(trial, 3) | user_id) +
    (1 | ema_number),
  data = d
)
summary(fm1)  


foo <- d |> 
  group_by(user_id, ema_number) |> 
  summarize(
    imood = mean(instant_mood),
    control = mean(control),
    mood_post = mean(mood_post),
    mood_pre = mean(mood_pre),
    gain = mean(gain),
    time = mean(TIME_total)
  ) |> 
  ungroup()

fm2 <- lmer(
  control ~ 1 + mood_pre +
    (1 + mood_pre | user_id) +  (1 | ema_number),
  data = foo
)
summary(fm2)  

library(rdrobust)

foo <- d |> 
  group_by(user_id, trial) |> 
  summarize(
    imood = mean(instant_mood),
  ) |> 
  ungroup()

rdr <- rdrobust(
  y = foo$imood,
  x = foo$trial, 
  c = 16
)
summary(rdr)


fm2 <- lmer(
  gain ~ 1 + control * mood_pre +
    (1 + control + mood_pre | user_id) + (1 | ema_number),
  data = foo
)
summary(fm2)  
