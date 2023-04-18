
### notes field, concatant both USER4 and USER5 so we retain all information 


processVoucher <- function(df){
  # df : columns user 4 and user 5 from sos data
  df1 <- df %>% 
    # user4 seperate on : into name, notes, data 
    separate(col = "IDENTIFIED_BY", into = c("name","notes","date1"), sep = ":")
  
  #user5 seperate on : nLoc, date, locations 
  df1 <- df1 %>% separate(col = "NUM_PRESSED", into = c("nLoc", "date2", "loc1", "loc2"), sep = ":")
  # if loc > 1, some classification for seperating features 
  df2 <- df1 %>% separate(col = loc1, into = c("loc1", "loc2a"), sep = ",") %>%
    dplyr::mutate(
      loc2 = paste0(loc2a, loc2)
    )
  df2$loc2 <-  str_remove_all(string = df2$loc2, pattern = "NA")
  
  df2$date <- format(dmy(df2$date2), "%m-%d-%Y")
  
  df3 <- df2 %>% dplyr::select("name","notes","loc1","loc2","date" )
  return(df3)
}
