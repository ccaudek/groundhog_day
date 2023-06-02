# Data wrangling

suppressPackageStartupMessages(library(rio))
suppressPackageStartupMessages(library(tidyverse))

d <- readRDS(snakemake@input[["raw_data"]])
# d <- readRDS("../../../data/prep/groundhog_init.RDS")

# Correct phone numbers.
d$subj_code_1[d$subj_code_1 == "389 429 1247"] <- "3894291247"
d$subj_code_1[d$subj_code_1 == "334 8921243"]  <- "3348921243"
d$subj_code_1[d$subj_code_1 == "338 883 3022"] <- "3388833022"

# Rename variable. `image_chosen == 1` means answering yes to the question: 
# "Would I repeat the same action that I have actually carried out?".
d$is_target_chosen <- ifelse(
  d$image_chosen == 1, 1, 0
)

# Create numeric variable indicating the days from the fist beep 
# (coded with 0).
d$date <- as.Date(substring(d$TIME_start, 1, 10))
d$time <- difftime(d$date, "2023-04-05", units = "days") |>
  round()
# unique(sort(d$time))

# Group by the "subj_code_1" variable and rank the "time" column:
# Create a ranking of time within-subject (the first beep, the second beep, ...)
df_ranked <- d %>% 
  group_by(subj_code_1) %>% 
  mutate(rank = dense_rank(time))

# names(df_ranked)

# Show the number of beeps for each subject.
tbl <- table(df_ranked$subj_code_1, df_ranked$rank)
# tbl 

# Check for duplicated rows
duplicated_rows <- duplicated(row.names(tbl))
sum(duplicated_rows)
# Use indexing to remove the duplicated rows from the original dataframe
# df_no_duplicates <- df[!duplicated_rows,]

