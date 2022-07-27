#' @title Assign Aspect
#'
#' @description Classifies the original Aspect value recorded in the field to matched the requirement of the GRIN selectors 
#'
#' @param masterTable a dataframe contain the original values for aspect
#'
#' @return the \code{masterTable} with the `ASPECT -lookup picker` column selected 
#'
#' @importFrom dplyr %>% select mutate case_when rowwise
#' @importFrom stringr %>% str_detect
#' 
#' @examples
#' 



aspectClassification <- function(masterTable){
  
  d1 <- masterTable %>%
    dplyr::select("ASPECT"= "ASPECT Original Value")
  d2 <- d1 %>%
    # test for exact matches 
    dplyr::mutate(
      `Coded Values` = case_when(
        `ASPECT` %in% c("N", "n", "North", "north", "NORTH", "mostly north") ~ "North",
        `ASPECT` %in% c("S", "s", "South", "south", "SOUTH") ~ "South",
        `ASPECT` %in% c("N","E", "e", "East", "east", "EAST") ~ "East",
        `ASPECT` %in% c("W", "w", "West", "west", "WEST") ~ "West",
        `ASPECT` %in% c("NE", "ne", "Northeast", "northeast", "NORTHEAST") ~ "Northeast",
        `ASPECT` %in% c("NW", "nw", "Northwest", "northwest", "NORTHWEST") ~ "Northwest",
        `ASPECT` %in% c("SE", "se", "Southeast", "southeast", "SOUTHEAST") ~ "Southeast",
        `ASPECT` %in% c("SW", "sw", "Southwest", "southwest", "SOUTHWEST") ~ "Southwest",
        TRUE ~ "Uncertain"
      )
    )%>%
    # test for multiple cases of specific directions
    dplyr::mutate(
      south = case_when(
        str_detect(string = `ASPECT`,
                   pattern =  regex('s', ignore_case = T)) &
          str_detect(string = `ASPECT`,
                     pattern = regex('sw', ignore_case = T))&
          str_detect(string = `ASPECT`,
                       pattern = regex('se', ignore_case = T)) ~ TRUE),
      north = case_when(
        str_detect(string = `ASPECT`,
                   pattern =  regex('n', ignore_case = T)) &
          str_detect(string = `ASPECT`,
                     pattern = regex('nw', ignore_case = T)) &
          str_detect(string = `ASPECT`,
                     pattern = regex('ne', ignore_case = T)) ~ TRUE ),
      east = case_when(
        str_detect(string = `ASPECT`,
                   pattern =  regex('e', ignore_case = T)) &
          str_detect(string = `ASPECT`,
                     pattern = regex('ne', ignore_case = T))&
          str_detect(string = `ASPECT`,
                     pattern = regex('se', ignore_case = T)) ~ TRUE ),
      west = case_when(
        str_detect(string = `ASPECT`,
                   pattern =  regex('w', ignore_case = T)) &
          str_detect(string = `ASPECT`,
                     pattern = regex('nw', ignore_case = T))&
          str_detect(string = `ASPECT`,
                     pattern = regex('sw', ignore_case = T)) ~ TRUE )
    )%>%
    # test for single match of above conditions
    dplyr::rowwise()%>%
    dplyr::mutate(
      total = sum(c(south, north, east, west), na.rm = TRUE))%>%
    # reclassify is only single element was found. 
    dplyr::mutate(
      `Coded Values` = case_when(
        total == 1 & isTRUE(south) ~ "South",
        total == 1 & isTRUE(north) ~ "North",
        total == 1 & isTRUE(east) ~ "East", 
        total == 1 & isTRUE(west) ~ "West",
        TRUE ~ `Coded Values`
      )
    )
  masterTable$`ASPECT -lookup picker` <- d2$`Coded Values`
  return(masterTable)
}
