#' Geography classification. 
#' @description : performs a test for NA values before concatinating features
#'
#' @param data : dataframe with sub_cnt1 and sub_cnt2
#'
#' @return : vector of geographic locations 
#'
geographyClassification<- function(data){
  df <- data %>%
    dplyr::mutate(
      geo =
      case_when(
        !is.na(SUB_CNT2) ~ paste0("United States, "
                                  ,str_trim(SUB_CNT1, side = "right"),
                                  ", ",str_trim(SUB_CNT2, side = "right")),
        !is.na(SUB_CNT1) ~ paste0("United States, "
                                  ,str_trim(SUB_CNT1, side = "right")),
        TRUE ~ "United States" 
      )
    )%>%
    dplyr::select(geo)
  return(df$geo)
}
