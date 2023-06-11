# Script name: brms_moodpre_control.R
# Project: groundhog_day
# Script purpose: 
#   run multilevl brms model with mood_pre as dv and control as iv
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Sun Jun 11 11:55:20 2023
# Last Modified Date: Sun Jun 11 11:55:20 2023
#
# 👉 

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

# ---------------------
# Read RDS file
# ---------------------

d <- readRDS(file = snakemake@input[["clean"]])
# d <- readRDS("data/prep/groundhog_clean.RDS")

suppressPackageStartupMessages({
  # Data Manipulation:
  library("tidyverse")
  library("magrittr")
  # Modeling:
  library("cmdstanr")
  library("brms")
})

df_byday <- d |> 
  group_by(user_id, ema_number) |> 
  summarize(
    control = mean(control),
    mood_pre = mean(mood_pre),
    mood_post = mean(mood_post)
  ) |> 
  ungroup()

#' I tried this, but it does not work!!
#' A new “level” variable is created before testing a three-level multilevel 
#' model. If the model includes ema_number nested within participants, brm 
#' will not be able to determine the nesting structure automatically because 
#' each participant would have many of the same “ema_number”. Accordingly, a 
#' new variable must be created that “concatenates” (using the paste()) command 
#' the user_id variable and the ema_number variable to create a unique variable 
#' that shows both user_id and ema_number at the same time.
#' https://psyarxiv.com/xf2pw/
df_byday$user_day <- paste(df_byday$user_id, df_byday$ema_number, sep = "_")

plot(density(df_byday$mood_pre))

bmod <- brm(
  mood_pre ~ 1 + control +
    (1 + control | user_id) + (1 | ema_number),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept"),
    prior(student_t(4, 0, 1), class = "sigma")
  ),
  family = skew_normal(),
  data = df_byday,
  init = 0.01,
  backend = "cmdstanr"
)

bmod <- brms::add_criterion(bmod, "loo")

print("Save brm object")
saveRDS(bmod, snakemake@output[["fit"]])

# pp_check(bmod)

# fig_1 <- conditional_effects(bmod, "control")
# ggsave(
#   "results/figures/brms_fig_moodpre_1.pdf", 
#   width = 4, height = 4
# )

a <- summary(bmod)
summary_mod <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) ) %>%
  select(-c("Bulk_ESS", "Tail_ESS")) %>% # removing ESS
  rownames_to_column(var = "parameter") |> # add first column name
  mutate_if(is.numeric, round, 3)

write.csv(
  summary_mod, 
  file = snakemake@output[["csv"]], 
  row.names = FALSE
)

# eof ----
