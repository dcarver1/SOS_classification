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
file <- "BGBASE_Alaska"
d1 <- readxl::read_excel(paste0("data/",file,".xlsx"))%>%
  dplyr::filter(!is.na(ACC_NUM))
View(d1)

# # read in bgbase with text fixes and use this as the input for classification 
d2 <- readxl::read_excel("data/bgbase/bgBase_formated202209.xlsx")
View(d2)
d21 <- d2[d2$ACC_NUM %in% d1$ACC_NUM, ]
View(d21)
# 
# 

n1 <- names(d1)[names(d1) != names(d2)]





# master table  --------------------------------------------------------
masterTable  <- masterTable(d1)


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
    