#' @title Assign Slope Valuye
#'
#' @description Classified the slope value based on original value 
#'
#' @param masterTable a dataframe contain the original values for slope
#'
#' @return the \code{masterTable} with the `ECOREGION` column selected 
#'
#' @importFrom dplyr %>% select mutate
#' 
#' @examples
#' 

slopeClassification <- function(masterTable){
  d1 <- masterTable 
  
  d2 <- d1 %>%
    dplyr::select("slope" = "SLOPE Original Value")%>%
    tidyr::separate(col = slope, into = c("val1", "val2"), sep = "-")%>%
    dplyr::mutate(val1 = as.numeric(val1),
                  val2 = as.numeric(val2))%>%
    dplyr::rowwise()%>%
    dplyr::mutate(
      ave = mean(c(val1, val2), na.rm=TRUE))
  
  d2$ave[is.nan(d2$ave)] <- NA
    
  return(d2$ave)
}
