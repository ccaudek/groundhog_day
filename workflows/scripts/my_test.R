# Combine the individual subjects' files into a single file
# with all the subjects' data.

suppressPackageStartupMessages(library(rio))
suppressPackageStartupMessages(library(tidyverse))

# Read subject codes from Excel file
subj_codes <- rio::import(
    # "data/raw/data.xlsx"
    snakemake@input[[1]]
) |>
    dplyr::select(
        subj_code_1, Task1Rev_1,
        context_1, control_1,
        post_context_1, coin, TIME_start,
        TIME_end, TIME_total
    )

# print(unique(subj_codes$subj_code_1))

subj_codes <- subj_codes[!is.na(subj_codes$Task1Rev_1), ]
subj_codes$subj_idx <- subj_codes$Task1Rev_1
subj_codes$Task1Rev_1 <- NULL

saveRDS(
    subj_codes,
    file = snakemake@output[[1]]
    #  file = "data/prep/groundhog_raw.RDS"
)
