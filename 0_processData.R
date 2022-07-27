# work on classification for the shipment features 
# carverd@colostate.edu
# 20211202


# load packages -----------------------------------------------------------
pacman::p_load(dplyr, readr, stringr, lubridate, tidyr)

# source functions --------------------------------------------------------
lapply(list.files("src", full.names = TRUE), source)

###
# current errors : 89th_SOS_Shipment_MARSB_LT, 91st_SOS_Shipment_NCBG, 99th_SOS_Shipment_NEWFS
# classified : 78th, 88th,98th,105th,106th
### 

# read in data  -----------------------------------------------------------
file <- "111 or 112th_SOS_Shipment"
d1 <- readr::read_csv(paste0("data/",file,".csv"))
# read in bgbase with text fixes and use this as the input for classification 
# d2 <- readr::read_csv("data/bgbase/BG-Base Export (02-27-2022).csv")
# d1 <- d2[d2$ACC_NUM %in% d1a$ACC_NUM, ]



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
write_csv(masterTable, file = paste0("output/",file, "_classified_"
                                     ,Sys.Date(),".csv"))
  