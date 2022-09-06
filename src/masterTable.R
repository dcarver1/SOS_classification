#' Generate master data table
#'
#' @param data : csv of the sos input data
#' @description : Populates all potential columns from the SOS data
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
masterTable <- function(data){

  # construct df ------------------------------------------------------------
  ## column names
  c1 <- c(
    "Curator Unique Number",
    "Accession Prefix (NPGS)",
    "Accession Number -Assigned",
    "Inventory Prefix",
    "Inventory Number",
    "Inventory Suffix",
    "Inventory Type - Lookup Picker",
    "Taxon -Lookup Picker in GRIN",
    "Received Date -received by site",
    "Life Form -Lookup Picker",
    "Level of Improvement -Lookup Picker",
    "Reproductive Uniformity -Lookup Picker",
    "Note (Accession Narrative)",
    "Date Collected or Developed",
    "Geography (Collection) -Lookup Picker in GRIN",
    "Geography (Donor)  -Lookup Picker in GRIN",
    "Number Plants Sampled",
    "Collecting or Acquisition Source - List",
    "Environment Description",
    "Associated Species",
    "Collector Verbatim Locality",
    "Elevation (meters)",
    "Latitude -decimal degrees",
    "Longitude -decimal degrees",
    "Georeference Datum",
    "Georeference Protocol - Lookup Picker",
    "Note (Accession Source - Collector)",
    "Cooperator (Donor) 1 -full record",
    "Cooperator (Donor) 2 -full record",
    "Cooperator (Collector) 1 -full record",
    "Cooperator (Collector) 2 -full record",
    "Cooperator (Collector) 3 -full record",
    "Land Owner",
    "Land Owner Original Value",
    "Land Owner Evaluation Required",
    "SLOPE",
    "SLOPE Original Value",
    "ASPECT -lookup picker",
    "ASPECT Original Value",
    "SOIL TEXTURE - lookup picker",
    "Soil TEXTURE Original Value",
    "ECOREGION - Lookup picker",
    "Ecoregion Original Value",
    "Inventory Maintenance Policy",
    "Inventory Maintenance Site -W6",
    "Quantity On Hand",
    "Quantity On Hand Units -'count' or 'packet'",
    "Total Weight -gram (if unknown, leave blank)",
    "Hundred Seed Weight -gram",
    "Seed Count Verification (VALUE CLOSE TO ZERO) -Calcuated column",
    "Parent Inventory",
    "GERMS from DONOR (viability)",
    "GERMS from DONOR (test date)",
    "Note (Inventory)",
    "Accession Name (Identifier 1)",
    "Accession Name Category (Identifier 1) -Lookup Picker",
    "Accession Name Cooperator (Identifier 1) -name, organization",
    "Accession Name (Identifier 2)",
    "Accession Name Category (Identifier 2) -Lookup Picker",
    "Accession Name Cooperator (Identifier 2) -name, organization",
    "Accession Name (Identifier 3)",
    "Accession Name Category (Identifier 3) -Lookup Picker",
    "Accession Name Cooperator (Identifier 3) -name, organization",
    "Collector Voucher Number",
    "Voucher Date",
    "Voucher Location (1)",
    "Voucher Location (2)",
    "Voucher Location (3)",
    "Voucher Collector -name, organization",
    "Note (Voucher)"
  )
  df <- data.frame(matrix(nrow = nrow(data), ncol = length(c1)))
  colnames(df) <- c1


  # create column assignments  ----------------------------------------------
  ## Accession Prefix (NPGS)
  df$`Accession Prefix (NPGS)` <- "W6"
  
  ## "Inventory Prefix"                                               
  df$`Inventory Prefix` <- "W6"
  ## "Inventory Type - Lookup Picker"                                 
  df$`Inventory Type - Lookup Picker` <- "SD"
  
  ## "Taxon -Lookup Picker in GRIN"                                   
  df$`Taxon -Lookup Picker in GRIN` <- setTaxon(data)
  
  ## "Level of Improvement -Lookup Picker"
  df$`Level of Improvement -Lookup Picker` <- "Wild material"
  
  ## "Note (Accession Narrative)"                   
  df$`Note (Accession Narrative)` <- str_replace_all(d1$DESCRIPTION, "[^[:alnum:]]", " ") %>%
    str_replace_all( "[^[:alnum:]]", " ")
  
  
  ## "Date Collected or Developed"                  
  df$`Date Collected or Developed` <- format(dmy(data$COLL_DT), "%m-%d-%Y") 
  
  ## "Geography (Collection) -Lookup Picker in GRIN"

  df$`Geography (Collection) -Lookup Picker in GRIN` <- geographyClassification(data)
  
  ## "Geography (Donor)  -Lookup Picker in GRIN"    
  df$`Geography (Donor)  -Lookup Picker in GRIN`<- "United States, Idaho, Ada"
  
  ## "Number Plants Sampled"                        
  df$`Number Plants Sampled` <- str_extract(data$COLLECTION_MISC, "[[:digit:]]+")
  
  ## "Collecting or Acquisition Source - List"      
  df$`Collecting or Acquisition Source - List` <- "Wild Habitat"
  
  ## "Environment Description"                      
  df$`Environment Description` <- data$USER2
  
  ## "Associated Species"                           
  df$`Associated Species` <- data$ASSOCIATED_TAXA_FULL
  
  ## "Collector Verbatim Locality"              
  ## content removed 202205 : "Sub Unit Description :", data$SUB_CNT3, "/ 
  df$`Collector Verbatim Locality` <- paste0(
    "Sub Unit Description :", str_replace_all(data$SUB_CNT3,"[^[:alnum:]]", " "),
    "/ Geology Description : ", str_replace_all(data$GEOG_AREA,"[^[:alnum:]]", " "),
    "/ Locality Description: ", data$LOCALITY
  )
  
  
  # str_replace_all(pattern ="[^[:alnum:]]", replacement = " ")
  ## "Elevation (meters)"                           
  df$`Elevation (meters)` <- processElevation(data)

  ## "Latitude -decimal degrees"                    
  df$`Latitude -decimal degrees` <- data$LATITUDE_DECIMAL
  
  ## "Longitude -decimal degrees"                   
  df$`Longitude -decimal degrees` <- data$LONGITUDE_DECIMAL
  
  ## "Georeference Datum"                         
  df$`Georeference Datum` <- data$GPS_DATUM
  
  ## "Georeference Protocol - Lookup Picker" 
  df$`Georeference Protocol - Lookup Picker` <- "Lat/lon determined by GPS"
  
  ## "Note (Accession Source - Collector)"          
  df$`Note (Accession Source - Collector)` <- paste0("Collectors: ",data$COLLECTED_WITH)
  
  ## "Cooperator (Donor) 1 -full record"            
  df$`Cooperator (Donor) 1 -full record`<- "Bureau of Land Management, SOS project"
  
  ## "Cooperator (Collector) 1 -full record"   
  df$`Cooperator (Collector) 1 -full record` <- data$COLL_ID
  
  ## "Land Owner Original Value"
  df$`Land Owner Original Value` <- data$LAND_OWNER
  
  ## "Land Owner Evaluation Required" means the term was not in the classification process. 
  df$`Land Owner Evaluation Required` <- NA
  
  ## "SLOPE Original Value"                                           
  df$`SLOPE Original Value` <- data$SLOPE
  
  ## "ASPECT Original Value"                                          
  df$`ASPECT Original Value` <- data$ASPECT
  
  ## "Soil TEXTURE Original Value"                                    
  df$`Soil TEXTURE Original Value` <- data$SOIL_TYPE
  
  ## "Ecoregion Original Value"                                       
  df$`Ecoregion Original Value` <- data$PHYTOREGION_FULL
  
  ## "Inventory Maintenance Policy"                                   
  df$`Inventory Maintenance Policy` <- "w6_native"
  
  ## "Inventory Maintenance Site -W6"                                 
  df$`Inventory Maintenance Site -W6` <- "W6"
  
  ## "Accession Name (Identifier 1)"                               
  df$`Accession Name (Identifier 1)` <- "" 
  
  ## "Accession Name (Identifier 2)"                               
  df$`Accession Name (Identifier 2)` <- data$ACC_NUM 
  
  ## "Accession Name Category (Identifier 2) -Lookup Picker"       
  df$`Accession Name Category (Identifier 2) -Lookup Picker` <- "Collector identifier"
  
  ### **Note
  ### secondary id in BG-base == triciary id in grin. 
  ## "Accession Name (Identifier 3)"  
  df <- df %>%
    dplyr::mutate(`Accession Name (Identifier 3)`=
      case_when(
        is.na(data$SECONDARY_ID) ~ "",
        TRUE ~ data$SECONDARY_ID
      )
    )
  
  ## "Accession Name Category (Identifier 3) -Lookup Picker"       
  df$`Accession Name Category (Identifier 3) -Lookup Picker` <- "Other or unclassified name"
  
  
  

  # format vouchers ---------------------------------------------------------
  voucher <- processVoucher(data[,c("USER4", "USER5")])
  
  ## "Voucher Date"                                                
  df$`Voucher Date` <- voucher$date
  
  ## "Voucher Location (1)"                                        
  df$`Voucher Location (1)` <- str_trim(voucher$loc1, side = "left")
  
  ## "Voucher Location (2)"                                        
  df$`Voucher Location (2)` <- str_trim(voucher$loc2, side = "left")  
  ## "Voucher Collector -name, organization"                       
  df$`Voucher Collector -name, organization` <- voucher$name
  
  ## Note (Voucher)
  df$`Note (Voucher)` <- voucher$notes
  
  return(df)
}
