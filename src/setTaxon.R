
#' Set Taxon 
#' @description : replace ssp. with grin notation subsup. 
#' @param data : dataframe with taxone name 
#'
#' @return
#' @export
#'
#' @examples
setTaxon <- function(data){
  # replace values replace ssp. with subsp.
  df <- str_replace(string = data$NAME, pattern = "ssp.", replacement = "subsp.")
  return(df)
}