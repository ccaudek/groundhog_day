# Combine the individual subjects' files into a single file
# with all the subjects' data.

suppressPackageStartupMessages(library(rio))
suppressPackageStartupMessages(library(tidyverse))

# Read subject codes from Excel file
subj_codes <- rio::import(
  snakemake@input[["xlsx"]]
) |>
  dplyr::select(
    subj_code_1, Task1Rev_1,
    context_1, control_1,
    post_context_1, coin, TIME_start,
    TIME_end, TIME_total
  )

subj_codes <- subj_codes[!is.na(subj_codes$Task1Rev_1), ]
subj_codes$subj_idx <- subj_codes$Task1Rev_1
subj_codes$Task1Rev_1 <- NULL

# Import PRL data form multiple files
# dir <- "../../data/raw/experiment_data"
dir <- snakemake@input[["dir_data"]]

file_names <-
  as.character(list.files(path = dir, pattern = "prl_EMA_reversal"))
n_files <- length(file_names)
n_files
d_list <- list()

for (i in 1:n_files) {
  # d <- read.table(here("data", "raw", "experiment_data", file_names[i]))
  d <- read.table(stringr::str_glue(dir, "/", file_names[i]))

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

# Convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

# Join files with PRL data and personal information
d <- left_join(df, subj_codes, by = "subj_idx")

saveRDS(
  d,
  file = snakemake@output[["rds"]]
)
