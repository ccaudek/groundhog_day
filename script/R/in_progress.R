library("here")
library("tidyverse")
library("mice")
library("rio")
library("lme4")
library("lcsm")
library("brms")


# Read subject codes from Excel file -------------------------------------------

subj_codes <- rio::import("../../data/raw/data.xlsx") |>
  dplyr::select(subj_code_1, Task1Rev_1, 
                context_1, control_1, 
                post_context_1, coin, TIME_start, 
                TIME_end, TIME_total)

subj_codes <- subj_codes[!is.na(subj_codes$Task1Rev_1), ]
subj_codes$subj_idx <- subj_codes$Task1Rev_1
subj_codes$Task1Rev_1 <- NULL


# Import PRL data form multiple files ------------------------------------------

dir <- here("data", "raw", "experiment_data")

file_names <- as.character(list.files(path = dir, pattern = "prl_EMA_reversal"))
n_files <- length(file_names)
n_files

d_list <- list()

for (i in 1:n_files) {
  d <- read.table(here("data", "raw", "experiment_data", file_names[i]))

  d$subj_idx <- file_names[i]
  d$epoch <- d$V1
  d$item_number <- d$V3
  d$rt <- d$V4
  d$has_answered <- d$V5
  d$image_more_rewarded <- d$V6
  d$rt_choice <- d$V7
  d$feedback <- ifelse(d$V8 == 1, 1, 0)
  d$image_chosen <- d$V9 # 1: mi comporterei nello stesso modo
                         # 2: mi comporterei in modo diverso
  d$intertrial_delay <- d$V10
  d$img_1_position <- d$V11 # 230 = dx; -230 sn
  d$img_2_position <- d$V12 # 230 = dx; -230 sn
  d$total_feedback <- d$V13 # mood from the starting value + feedbacks

  d$trial <- 1:30

  d_list[[i]] <- d
}


# Convert list into data.frame -------------------------------------------------

df <- do.call(rbind.data.frame, d_list)


# Join files with PRL data and personal information ----------------------------

d <- left_join(df, subj_codes, by = "subj_idx")


# Data wrangling ---------------------------------------------------------------

d$date <- as.Date(substring(d$TIME_start, 1, 10))
d$time <- difftime(d$date, "2023-04-05", units = "days") |>
  round()

d$is_target_chosen <- ifelse(
  d$image_chosen == 1, 1, 0
)

# Find subjects who have always select only the option "I would repeat the same
# action as I did".
# - Does it make sense to remove them? Are them the most rigid ones? Or the 
# problem is that they did not understand the task? I assume the second one.
foo <- d |>
  group_by(subj_code_1) |>
  summarize(
    y = mean(is_target_chosen)
  )
# Frequency of target chosen (the same option as what really happened). If 0,
# it means that the person would never repeat what she/he has actually done.
bad_id_df <- foo[foo$y == 1 | foo$y == 0, ]
bad_id <- unique(bad_id_df$subj_code_1)
# I remove the participants who always chose only one response option.
d <- d[!(d$subj_code_1 %in% bad_id), ]

# TODO
# - Check whether each participant has completed all the 30 trials in each 
#   session.
# - Consider whether to estimate the HDDMRL parameters of the separately for 
#   each participant by using a hierarchical model with sessions as the
#   grouping variable. 


# Create input for HDDMRL ------------------------------------------------------

temp <- d %>% 
  group_by(subj_code_1) %>% 
  mutate(rank = dense_rank(time))

temp$response <- temp$is_target_chosen
temp$rt <- temp$rt_choice / 1000
temp$rt <- ifelse(temp$rt > 15, 15, temp$rt)
temp$subj_idx <- as.numeric(factor(as.character(temp$subj_code_1)))
temp$split_by = 0
temp$mood = temp$post_context_1
temp$q_init = 0.5


na_counts <- aggregate(. ~ subj_idx, data = df, FUN = function(x) sum(is.na(x)))
print(na_counts)

temp <- temp[!is.na(temp$subj_idx), ]
df_sorted <- temp[order(temp$subj_idx, temp$rank, temp$trial),]

ntrials <- temp |> 
  group_by(subj_idx) |> 
  summarize(
    n = n()
  )

# 'trial' must be a vector from 1 to nj, where nj is the max number of trials 
# for each participant.
df_indexed <- df_sorted %>%
  group_by(subj_idx) %>%
  mutate(row_index = row_number())

df_indexed$trial_in_block <- df_indexed$trial
df_indexed$trial <- df_indexed$row_index

for_hddm_df <- df_indexed |> 
  dplyr::select(
    subj_idx, rank, control_1, context_1, epoch, rt, mood, response, feedback,
    split_by, q_init, row_index, trial_in_block
  ) |> 
  dplyr::rename(
    trial = row_index
  )

# Perhaps, the extreme values (-50, +50) should be considered as NA and
# imputation is necessary.
hist(for_hddm_df$mood)

quantile(for_hddm_df$mood, c(1/3, 2/3))

min(for_hddm_df$mood)
max(for_hddm_df$mood)

for_hddm_df$fmood <- ifelse(
  for_hddm_df$mood < -10, "low", 
  ifelse(for_hddm_df$mood >= -10 & for_hddm_df$mood < 10, "med", "high")
) |> 
  as.factor()
table(for_hddm_df$fmood)

# foo <- temp[temp$rank == 1, ]

rio::export(for_hddm_df, "marmotta.csv")

bysubj_mood_var = df_indexed |> 
  group_by(subj_idx) |> 
  summarize(
    within_var = mean(mood),
    n = n()
  )

bysubj_mood_var |> 
  as.data.frame()
#########






# group by the "group" variable and rank the "value" column
df_ranked <- d %>% 
  group_by(subj_code_1) %>% 
  mutate(rank = dense_rank(time))

# print the result
glimpse(df_ranked)

table(df_ranked$subj_code_1, df_ranked$rank)

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


fm <- lmer(gain ~ mood_post + (1 | id), foo)
summary(fm)

m1 <- brm(
  gain ~ mood_post + rank + mood_init + how_much_control + how_long +
    (1 + rank + mood_post | id), 
  foo,
  family = student(),
  backend = "cmdstanr"
)

pp_check(m1)
summary(m1)

conditional_effects(m1, "mood_post")
conditional_effects(m1, "rank")
conditional_effects(m1, "how_much_control")
conditional_effects(m1, "mood_init")


m2 <- brm(
  bf(gain ~ mood_post + rank + 
       (1 + rank|i|id), 
     sigma ~ 1 + (1|i|id)
  ),
  data=foo, family=student(), cores=4
)
pp_check(m2)
summary(m2)
conditional_effects(m2, "mood_post")


hist(out$sg)


m2 <- brm(
  sg ~ sm, 
  out,
  family = skew_normal(),
  backend = "cmdstanr"
)

pp_check(m2)
summary(m2)









#-------------------------------------------------------------------------------
# Add baseline mood
first_trial <- d[d$trial == 1, ]$total_feedback

d$baseline <- rep(first_trial, each = 30)

d$is_target_chosen <- ifelse(
  d$image_chosen == 1, 1, 0
)

d$ID <- as.numeric(factor(d$subj_code_1))


foo <- d |>
  group_by(ID) |>
  summarize(
    y = mean(is_target_chosen)
  )

bad_id_df <- foo[foo$y == 1 | foo$y == 0, ]
bad_id <- bad_id_df$ID

df_clean <- d |>
  dplyr::filter(!ID %in% bad_id)


df_clean <- df_clean %>%
  mutate(prev_feedback = lag(feedback))

df_clean <- df_clean |>
  dplyr::filter(image_chosen == 1 | image_chosen == 2)

df_clean <- df_clean[!is.na(df_clean$prev_feedback), ]

df_clean$image_chosen <- factor(df_clean$image_chosen)
df_clean$prev_feedback <- factor(df_clean$prev_feedback)

df_clean$rt_choice <- ifelse(
  (df_clean$rt_choice > 12000) | (df_clean$rt_choice < 200), 
  NA, df_clean$rt_choice
)

df_clean$feedback <- ifelse(
  (df_clean$rt_choice > 12000) | (df_clean$rt_choice < 200), 
  NA, df_clean$feedback
)


df_clean$date <- substring(df_clean$subj_idx, 21, 30)

df_clean$date <- as.Date(df_clean$date)
unique(df_clean$date)

table(df_clean$ID)

# https://cran.r-project.org/web/packages/mlim/readme/README.html
library(devtools)
install_github("haghish/mlim")
library(mlim)

mydf <- with(
  df_clean,
  data.frame(ID = factor(ID), epoch=factor(epoch), item_number, rt_choice, 
             feedback, image_chosen, 
             intertrial_delay, img_1_position, img_2_position, total_feedback,
             trial, baseline, is_target_chosen)

  )

df_imp <- mlim(mydf, m=1, seed = 2022, tuning_time = 180) 
df_imp$feedback <- round(df_imp$feedback)

df_imp <- df_imp %>%
  mutate(prev_feedback = lag(feedback))
df_imp$prev_feedback <- factor(df_imp$prev_feedback)



######
# add subject code



######


foo1 <- df_imp |>
  group_by(trial) |>
  summarize(
    y = mean(is_target_chosen)
  )


foo1 |>
  ggplot(
    aes(trial, y)
  ) +
  geom_line(aes())


df_imp |>
  group_by(image_chosen, prev_feedback) |>
  summarize(
    y = median(rt_choice, na.rm = T),
    std = sd(rt_choice, na.rm = T),
    n = n()
  )


fm <- lme4::lmer(
  log(rt_choice) ~ image_chosen * prev_feedback +
    (1 + image_chosen + prev_feedback | ID),
  data = df_imp
)

summary(fm)

car::Anova(fm)


fm <- lme4::glmer(
  is_target_chosen ~ baseline * epoch +
    (1 | ID),
  family = binomial(),
  data = df_imp
)
summary(fm)

mod <- brm(
  is_target_chosen ~ baseline * epoch +
    (1 + epoch | ID),
  family = bernoulli(),
  backend = "cmdstanr",
  data = df_imp
  # algorithm = "meanfield"
)
conditional_effects(mod)

pp_check(mod)


df_imp$gain <- df_imp$total_feedback - df_imp$baseline

plot(jitter(df_imp$baseline), jitter(df_imp$gain))
cor(df_imp$baseline, df_imp$gain)


mod <- brm(
  rt_choice ~ baseline * image_chosen * prev_feedback +
    (1 + image_chosen * prev_feedback | ID),
  family = exgaussian(),
  backend = "cmdstanr",
  data = df_imp,
  algorithm = "meanfield"
)

pp_check(mod)

conditional_effects(mod)

summary(mod)

emmeans::emmeans(mod, specs = c("prev_feedback", "image_chosen"))
# prev_feedback image_chosen emmean lower.HPD upper.HPD
# 0             1              7.47      7.41      7.53
# 1             1              7.49      7.43      7.55
# 0             2              7.39      7.32      7.46
# 1             2              7.30      7.23      7.37

# TODO. Analyze data with DDM.



fm <- lme4::lmer(
  log(rt_choice) ~ date + image_chosen * prev_feedback + 
    baseline + image_chosen:baseline +
    (1 + image_chosen + prev_feedback | ID),
  data = df_clean
)
summary(fm)

emmeans::emmeans(fm, specs = c("prev_feedback", "image_chosen"))

fm1 <- lme4::glmer(
  feedback ~ baseline * image_chosen * prev_feedback + 
    (1 | ID),
  family = binomial(),
  data = df_clean
)
summary(fm1)

mod <- brm(
  feedback ~ baseline * image_chosen * prev_feedback + 
    (1 | ID),
  family = bernoulli(),
  backend = "cmdstanr",
  data = df_clean
  # algorithm = "meanfield"
)

conditional_effects(mod)


mod <- brm(
  total_feedback ~ baseline + trial * image_chosen * prev_feedback + 
    (1 | ID),
  family = gaussian(),
  backend = "cmdstanr",
  data = df_clean
  # algorithm = "meanfield"
)

conditional_effects(mod)

##### total_feedback as a function of time for each participant

df_clean$time <- as.numeric(df_clean$date) - 19450

fm <- lm(
  baseline ~ time,
  data = df_clean
)

out <- df_clean |> 
  group_by(ID, time) |> 
  summarize(
    y = mean(baseline, na.rm = TRUE)
  )


out |> 
  group_by(time) |> 
  summarize(
    avg = mean(y, na.rm = TRUE), 
    n = n()
  )


df_clean %>%
  ggplot(aes(x = time, y = baseline)) +
  geom_point() +
  geom_line() +
  # coord_cartesian(ylim = c(1, 4)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ID)


hist(df_clean$baseline)

plot(df_clean$baseline, df_clean$total_feedback)

# eof ------





df |>
  group_by(image_chosen, prev_feedback) |>
  summarize(
    y = median(rt_choice, na.rm = T),
    std = sd(rt_choice, na.rm = T),
    n = n()
  )


fm <- lme4::lmer(log(rt_choice) ~ image_chosen * prev_feedback +
  (1 + image_chosen + prev_feedback | subj_idx), data = df)

car::Anova(fm)

dd <- df |>
  group_by(subj_idx, image_chosen, prev_feedback) |>
  summarize(
    y = median(rt_choice, na.rm = TRUE)
  )

df1 <- df[df$image_chosen == 1 | df$image_chosen == 2, ]

fm <- lme4::lmer(
  log(rt_choice) ~ image_chosen * prev_feedback +
    (1 + image_chosen + prev_feedback | subj_idx),
  data = df1
)

summary(fm)

df1$image_chosen <- factor(df1$image_chosen)
df1$prev_feedback <- factor(df1$prev_feedback)


mod <- brm(
  rt_choice ~ image_chosen * prev_feedback +
    (1 | subj_idx),
  family = lognormal(),
  backend = "cmdstanr",
  data = df1
  # algorithm = "meanfield"
)

pp_check(mod)

conditional_effects(mod)

