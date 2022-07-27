

landOwnerClassification <- function(masterTable){
  
  referenceSheet <- read_csv("doc/landOwnerClassificationMike2021_10.csv")
  
  d1 <- masterTable %>%
    dplyr::select("land_owner" = "Land Owner Original Value")%>%
    dplyr::mutate("Land Owner" = NA, "New Feature" = NA)
  
  vals <- na.exclude(unique(d1$land_owner))
  
  for(i in vals[1]){
    val <- referenceSheet %>% 
      dplyr::filter(i == LAND_OWNER)%>%
      dplyr::select(class)%>%
      pull()
    if(val %in% c("Public","Private","Government","Unknown")){
    d1$`Land Owner`[d1$land_owner == i] <- val 
  }else{
    d1$`Land Owner`[d1$land_owner == i] <- "Unknown"
    d1$`New Feature`[d1$land_owner == i] <- TRUE
    }
  }
  
  masterTable$`Land Owner` <- d1$`Land Owner`
  masterTable$`Land Owner Original Value` <- d1$land_owner
  masterTable$`Land Owner Evaluation Required` <- d1$`New Feature`
  
  return(masterTable)

}