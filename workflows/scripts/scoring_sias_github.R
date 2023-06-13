#' \code{scoring_sias()} generates the total score of the Social Interaction 
#' Anxiety Scale (SIAS)
#' @param data.frame.
#' @return data.frame.
#' The first column has the subjects' ids; the following columns are the 19 
#' items of the SIAS. The items must be named `sias_1`, ... `sias_19`.
scoring_sias <- function(d) {
  
  # Reverse items 8 and 10.
  columns_to_recode <- c("sias_8", "sias_10")
  
  d <- d %>% 
    mutate_at(
      vars(columns_to_recode), 
      ~case_match(., 
        1 ~ 5, 
        2 ~ 4, 
        3 ~ 3,
        4 ~ 2,
        5 ~ 1
      )
    )
  
  # Compute total score.
  d <- d %>%
    rowwise() %>%
    mutate(
      sias_score = sum(c_across(sias_1:sias_19))
    )
  
  sias_scale <- d |> 
    dplyr::select(user_id, sias_score)
  
  sias_scale
}
