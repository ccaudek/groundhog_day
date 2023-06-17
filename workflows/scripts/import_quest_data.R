# import_quest_data.R

suppressPackageStartupMessages({
  library("rio")
  library("dplyr")
}) 

print("Loading data")
# Create a vector file_names containing the file names "quest1", "quest2", 
# "quest3", and "quest4". Iterate over each file name using a loop, import 
# the data, and perform the necessary data transformations. Store the 
# DataFrames in a list called data_frames, with the index i representing the 
# corresponding file.
file_names <- c("quest1", "quest2", "quest3", "quest4")
data_frames <- list()

for (i in seq_along(file_names)) {
  file_name <- file_names[i]
  file_path <- here::here("data", "raw", "quest", file_name, "data.xlsx")
  data <- rio::import(file_path)
  data <- data[!duplicated(data$num_cell_tot_1), ] %>%
    dplyr::select(-TIME_start, -TIME_end) %>%
    dplyr::rename(time = TIME_total) %>%
    dplyr::select(
      -c(
        participant, nome_1, cognome_1, anno_1, mese_1, giorno_1,
        cellulare_1, sesso_1
      )
    )
  data_frames[[i]] <- data
}

# Merge the data frames using the inner_join function in a loop. Starting 
# with the first data frame (data_frames[[1]]), iterate over the remaining 
# DataFrames and perform inner joins based on the "num_cell_tot_1" column.
quest_dat <- data_frames[[1]]
for (i in 2:length(data_frames)) {
  quest_dat <- inner_join(quest_dat, data_frames[[i]], by = "num_cell_tot_1")
} 

# Remove unnecessary variables.
quest_dat <- quest_dat |>
  dplyr::select(-Dichiara_1, -Dichiara_2, -Nazionalita_1)

# Remove last 2 chars from the columns 2:14.
columns_to_modify <- names(quest_dat)[2:14]
remove_last_n <- 2
modified_columns <- 
  substr(columns_to_modify, 1, nchar(columns_to_modify) - remove_last_n)
colnames(quest_dat)[2:14] <- modified_columns

# Save csv file.
print("Saving output")
rio::export(
  quest_dat,
  # here::here("data", "prep", "quest.csv")
  file = snakemake@output[["csv"]]
)

# eof ----

