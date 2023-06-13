library("tidyverse")
library("here")
library("rio")


q1 <- rio::import(
  here::here("data", "raw", "quest", "quest1", "data.xlsx")
)
q1 <- q1[!duplicated(q1$num_cell_tot_1), ] |>
  dplyr::select(-TIME_start, -TIME_end) |>
  dplyr::rename(
    time1 = TIME_total
  ) |>
  dplyr::select(-participant, -Dichiara_1, -Dichiara_2)

q2 <- rio::import(
  here::here("data", "raw", "quest", "quest2", "data.xlsx")
)
q2 <- q2[!duplicated(q2$num_cell_tot_1), ] |>
  dplyr::select(-TIME_start, -TIME_end) |>
  dplyr::rename(
    time2 = TIME_total
  ) |>
  dplyr::select(
    -c(
      participant, nome_1, cognome_1, anno_1, mese_1, giorno_1,
      cellulare_1, sesso_1
    )
  )

q3 <- rio::import(
  here::here("data", "raw", "quest", "quest3", "data.xlsx")
)
q3 <- q3[!duplicated(q3$num_cell_tot_1), ] |>
  dplyr::select(-TIME_start, -TIME_end) |>
  dplyr::rename(
    time3 = TIME_total
  ) |>
  dplyr::select(
    -c(
      participant, nome_1, cognome_1, anno_1, mese_1, giorno_1,
      cellulare_1, sesso_1
    )
  )

q4 <- rio::import(
  here::here("data", "raw", "quest", "quest4", "data.xlsx")
)
q4 <- q4[!duplicated(q4$num_cell_tot_1), ] |>
  dplyr::select(-TIME_start, -TIME_end) |>
  dplyr::rename(
    time4 = TIME_total
  ) |>
  dplyr::select(
    -c(
      participant, nome_1, cognome_1, anno_1, mese_1, giorno_1,
      cellulare_1, sesso_1
    )
  )

q12 <- inner_join(q1, q2, by = "num_cell_tot_1")
q123 <- inner_join(q12, q3, by = "num_cell_tot_1")
quest_dat <- inner_join(q123, q4, by = "num_cell_tot_1")

rio::export(
  quest_dat,
  here::here("data", "prep", "quest.csv")
)


# data.frame(quest_dat$time1, quest_dat$time2, quest_dat$time3, quest_dat$time4)
