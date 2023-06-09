# -------------------------------
# Functions
# -------------------------------


#' Centering on group-means or person-means
#'
#' This function allows you to center on group-mean (traditional centering) or
#' on person-means (also called "centering within clusters"). It requires a
#' column called `user_id`.
#' @param var name of variable to be centered
#' @return A column in your dataframe (with person-centered data)
#' @keywords centering
#' @export
#' @examples
#' \dontrun{data$centeredVAR<-pcenter(data$ID,data$var)}
cent_scale <- function(dat, method = c("GM", "CWC")) {
  
  method <- match.arg(method)
  
  if (method == "GM") {
    
    # grand mean centering
    df <- dat %>%
      mutate_if(is.numeric, ~((.x - mean(.x, na.rm=TRUE)) / sd(.x, na.rm=TRUE)))
    
  } else if (method == "CWC") {
    
    # scale numeric data within clusters
    df <- dat %>%
      group_by(user_id) %>%
      mutate_if(is.numeric, ~((.x - mean(.x, na.rm=TRUE)) / sd(.x, na.rm=TRUE))) %>%
      mutate_if(is.numeric, ~if_else(is.nan(.), 0, .)) %>%
      ungroup()
    
  } else {
    print("ERROR")
  }
  
  return(df)
}

