# work on classification for the shipment features 
# carverd@colostate.edu
# 20211202


# load packages -----------------------------------------------------------
pacman::p_load(dplyr, readr, stringr, lubridate, tidyr, readxl)

# source functions --------------------------------------------------------
lapply(list.files("src", full.names = TRUE), source)

###
# current errors : 89th_SOS_Shipment_MARSB_LT, 91st_SOS_Shipment_NCBG, 99th_SOS_Shipment_NEWFS
# classified : 78th, 88th,98th,105th,106th
### 

# read in data  -----------------------------------------------------------
file <- "Mystery2_bg"
d1 <- readxl::read_excel(paste0("data/",file,".xlsx"))%>%
  dplyr::filter(!is.na(ACC_NUM))

# # read in bgbase with text fixes and use this as the input for classification 
d2 <- readxl::read_excel("data/bgbase/bgBase_formated202209.xlsx")
View(d2)
d21 <- d2[d2$ACC_NUM %in% d1$ACC_NUM, ]
# 
# 

# master table  --------------------------------------------------------
masterTable  <- masterTable(d21)


# classify aspect ---------------------------------------------------------
masterTable <- aspectClassification(masterTable)

# classify ecoregion  -----------------------------------------------------
masterTable <- ecoregionClassification(masterTable)


#  classify slope ---------------------------------------------------------
masterTable$SLOPE <- slopeClassification(masterTable)

# classify soil -----------------------------------------------------------
masterTable <- soilClassification(masterTable)


# classify land owner -----------------------------------------------------
masterTable <- landOwnerClassification(masterTable)


View(masterTable)

# eval results ------------------------------------------------------------
# write to csv
write_csv(masterTable, file = paste0("output/",file, "_classified_"
                                     ,Sys.Date(),".csv"))
    