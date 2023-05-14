# Recode Rosenberg Self-Esteem Scale --------------------------------------

#' @description scoring Rosenberg self-esteem scale.
#' @param data.frame.
#' @return data.frame.
recode_rosenberg <- function(d) {
  
  rses <- d %>%
    mutate(
      across(
        c(ros_1, ros_2, ros_4, ros_6, ros_7),
        ~ recode(.,
                 "Fortemente in disaccordo" = 0L,
                 "In disaccordo"            = 1L,
                 "D'accordo"                = 2L,
                 "Fortemente d'accordo"     = 3L
        )
      )
    ) %>%
    mutate(
      across(
        c(ros_3, ros_5, ros_8, ros_9, ros_10),
        ~ recode(.,
                 "Fortemente in disaccordo" = 3L,
                 "In disaccordo"            = 2L,
                 "D'accordo"                = 1L,
                 "Fortemente d'accordo"     = 0L
        )
      )
    )
  
  # Compute total score.
  rses <- rses %>%
    rowwise() %>%
    mutate(
      ros_tot = sum(c_across(ros_1:ros_10))
    )
  
  # Add subj_code.
  rses$subj_code <- d$subj_code
  
  rses
}


# Recode DASS-21 ----------------------------------------------------------

#' @description recode DASS-21
#' cut-offs: https://maic.qld.gov.au/wp-content/uploads/2016/07/DASS-21.pdf
#' @param data.frame.
#' @return data.frame.
recode_dass21 <- function(d) {

  # Recode values from alphanumeric to numeric for all columns in the
  # dataframe.
  d <- d %>%
    mutate_all(
      ~ recode(
        .,
        "Non mi è mai accaduto"                 = 0L,
        "Mi è capitato qualche volta"           = 1L,
        "Mi è capitato con una certa frequenza" = 2L,
        "Mi è capitato quasi sempre"            = 3L
      )
    )
  
  # Stress
  d$dass21_s <- 
    with(
      d,
      dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18
    )
  
  # Anxiety
  d$dass21_a <- 
    with(
      d,
      dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20
    )
  
  # Depression
  d$dass21_d <- 
    with(
      d,
      dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21
    )
  
  d
}

