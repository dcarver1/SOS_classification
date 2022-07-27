#' process elevation value
#' @name processElevation
#' @param data : dataframe with the orginal sos data
#' @return
#' @export
#'
#' @examples
#' 




processElevation <-function(data){
  data <- data %>%
    dplyr::mutate(elevation2  = as.numeric(ALTITUDE))%>%
    dplyr::mutate("elevation" = 
      case_when(
        ALTITUDE_UNIT == "FT" ~ elevation2 * 0.3048,
        TRUE ~ elevation2
      )
    )
  return(data$elevation)
}