

landOwnerClassification <- function(masterTable){
  
  referenceSheet <- read_csv("doc/landOwnerClassificationMike2021_10.csv")
  
  d1 <- masterTable %>%
    dplyr::select("land_owner" = "Land Owner Original Value")%>%
    dplyr::mutate("Land Owner" = NA, "New Feature" = NA)
  
  vals <- na.exclude(unique(d1$land_owner))
  index <- c("Public","Private","Government","Unknown")
    
  for(i in vals){
    val <- referenceSheet %>% 
      dplyr::filter(i == LAND_OWNER)%>%
      dplyr::select(class)%>%
      pull()
    if(identical(val, character(0))){
      d1$`Land Owner`[d1$land_owner == i] <- "Unknown"
      d1$`New Feature`[d1$land_owner == i] <- TRUE
    }else{
      # x <- index[grepl(val, x = index)]
      d1$`Land Owner`[d1$land_owner == i] <- val
    }
  }
  masterTable$`Land Owner` <- d1$`Land Owner`
  masterTable$`Land Owner Original Value` <- d1$land_owner
  masterTable$`Land Owner Evaluation Required` <- d1$`New Feature`
  
  return(masterTable)
}