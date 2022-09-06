#' @title Assign Soil Texture 
#'
#' @description Classifies the original soil texture value recorded in the field to matched the requirement of the GRIN selectors 
#'
#' @param masterTable a dataframe contain the original values for soil texture
#'
#' @return the \code{masterTable} with the `SOIL TEXTURE - lookup picker` column selected 
#'
#' @importFrom dplyr %>% select mutate case_when bind_rows
#' @importFrom stringr %>% str_detect
#' 
#' @examples
#' 

soilClassification <- function(masterTable){

  d1 <- masterTable %>%
    dplyr::mutate("index" = 1:nrow(masterTable))%>%
    dplyr::select("index", "Soil_type" = "Soil TEXTURE Original Value")
  
  
  soilTypeAll <- c("Clay","Loam","Clay loam","Silt","Silt clay","Silt clay loam",
                   "Silt loam","Sandy clay","Sandy clay loam","Sandy loam",
                   "Fine sandy loam","Coarse sandy loam","Loamy sand",
                   "Loamy very fine","Loamy fine sand","Loamy coarse sand",
                   "Very fine sand","Fine sand","Coarse sand","Sand unsorted",
                   "Sand","Unspecified")
  soilClass3 <- c("Silt clay loam","Sandy clay loam","Fine sandy loam",
                  "Coarse sandy loam","Loamy very fine","Loamy fine sand",
                  "Loamy coarse sand","Very fine sand")
  soilClass2 <-c("Clay loam","Silt clay","Silt loam","Sandy clay","Sandy loam",
                 "Loamy sand","Fine sand","Coarse sand","Sand unsorted")
  soilClass1 <- c("Clay","Loam","Silt","Sand")
  
  

  # Test for any soil terms -------------------------------------------------
  d2 <- d1%>%
  dplyr::mutate(
    `class_1` = case_when(
      str_detect(Soil_type, fixed(pattern= "clay", ignore_case = TRUE)) ~ TRUE,
      str_detect(Soil_type, fixed(pattern= "silt", ignore_case = TRUE)) ~ TRUE,
      str_detect(Soil_type, fixed(pattern= "sand", ignore_case = TRUE)) ~ TRUE,
      str_detect(Soil_type, fixed(pattern= "loam", ignore_case = TRUE)) ~ TRUE,
      TRUE ~ FALSE
    ))
  # test for present of primary soil descriptors to determine if a classifacation is possible. 
  matched <- d2 %>% dplyr::filter(class_1 == FALSE) %>% dplyr::mutate(`Coded Values` ="Unspecified") %>% dplyr::select(-"class_1")
  unmatched <- d2 %>% dplyr::filter(class_1 == TRUE) 
  
  # test for direct match on soil class 3  -----------------
  for(i in soilClass3){
    unmatched[,i] <- str_detect(unmatched$Soil_type, fixed(pattern= i, ignore_case = TRUE))
  }
  
  unmatched <- unmatched %>%
    dplyr::mutate(
      total = rowSums(.[4:10]), 
      `Coded Values` = case_when(
        total > 1 ~ "Unspecified"
      )
    )
  
  # grab unspecified
  matched <- unmatched %>% dplyr::filter(`Coded Values` == "Unspecified")%>%
    dplyr::select(names(matched))%>% 
    dplyr::bind_rows(matched)
  # remove unspecified class 
  unmatched <- unmatched[is.na(unmatched$`Coded Values`), ]
  
  
  # filter on single match 
 
  for(i in soilClass3){
    df <- unmatched %>% 
      dplyr::mutate(`Coded Values` = case_when(
        unmatched[,i] == 1 ~ i
      ))
    unmatched <- df[is.na(df$`Coded Values`),] 
    matched <- df  %>% dplyr::filter(!is.na(`Coded Values` ))%>%
      dplyr::select(names(matched))%>% 
      dplyr::bind_rows(matched)
  }
  # simplfy the unmatched data 
  unmatched <- unmatched %>% dplyr::select(names(matched))
  nrow(matched)
  # classify soil class 2  --------------------------------------
  for(i in soilClass2){
    unmatched[,i] <- str_detect(unmatched$Soil_type, fixed(pattern= i, ignore_case = TRUE))
  }
  unmatched <- unmatched%>%
    dplyr::mutate(
      total = rowSums(.[4:12]), 
      `Coded Values` = case_when(
        total > 1 ~ "Unspecified"
      )
    ) 
  
  # grab unspecified
  matched <- unmatched %>% dplyr::filter(`Coded Values` == "Unspecified")%>%
    dplyr::select(names(matched))%>% 
    dplyr::bind_rows(matched)
  # remove unspecified class 
  unmatched <- unmatched[is.na(unmatched$`Coded Values`), ]
  
  # filter on single match 
  for(i in soilClass2){
    df <- unmatched %>% 
      dplyr::mutate(`Coded Values` = case_when(
        unmatched[,i] == 1 ~ i
      ))
    unmatched <- df[is.na(df$`Coded Values`),] 
    matched <- df  %>% dplyr::filter(!is.na(`Coded Values` ))%>%
      dplyr::select(names(matched))%>% 
      dplyr::bind_rows(matched)
  }
  # simplfy the unmatched data 
  unmatched <- unmatched %>% dplyr::select(names(matched))
  
  nrow(matched)
  # classify soil class1  -----------------------------------------------
  for(i in soilClass1){
    unmatched[,i] <- str_detect(unmatched$Soil_type, fixed(pattern= i, ignore_case = TRUE))
  }
  unmatched <- unmatched%>%
    dplyr::mutate(
      total = rowSums(.[4:7]), 
      `Coded Values` = case_when(
        total > 1 ~ "Unspecified"
      )
    ) 
  
  # grab unspecified
  matched <- unmatched %>% dplyr::filter(`Coded Values` == "Unspecified")%>%
    dplyr::select(names(matched))%>% 
    dplyr::bind_rows(matched)
  # remove unspecified class 
  unmatched <- unmatched[is.na(unmatched$`Coded Values`), ]
  
  
  # filter on single match 
  for(i in soilClass1){
    df <- unmatched %>% 
      dplyr::mutate(`Coded Values` = case_when(
        unmatched[,i] == 1 ~ i
      ))
    unmatched <- df[is.na(df$`Coded Values`),] 
    matched <- df  %>% dplyr::filter(!is.na(`Coded Values` ))%>%
      dplyr::select(names(matched))%>% 
      dplyr::bind_rows(matched)
  }
  # assign rem
  matched <- unmatched %>% dplyr::select(names(matched))%>% 
    dplyr::mutate(`Coded Values` = "Unspecified") %>%
    bind_rows(matched)
  
  nrow(matched)
  # compile final output ----------------------------------------------------

  
  # join to original dataset 
  d1 <- d1 %>% dplyr::left_join(matched, by = "index")
  # I dont know why duplicated values are occuring. this is just a quick fix for the moment. 
  d1 <- d1[!duplicated(d1$index),]
  
 
  masterTable$`SOIL TEXTURE - lookup picker` <- d1$`Coded Values`
  return(masterTable)
}
