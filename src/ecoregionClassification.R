#' @title Assign Ecoregion
#'
#' @description Classifies the original Ecoregion value recorded in the field to matched the requirement of the GRIN selectors 
#'
#' @param masterTable a dataframe contain the original values for ecoregion
#'
#' @return the \code{masterTable} with the `ECOREGION - Lookup picker` column selected 
#'
#' @importFrom dplyr %>% select mutate
#' 
#' @examples
#' 
ecoregionClassification <- function(masterTable){
  # prep data
  d1 <- masterTable %>%
    dplyr::select("ecoReg" =  "Ecoregion Original Value")%>%
    dplyr::mutate("ecoReg" =str_trim(str_sub(ecoReg, end=-10),side = "right"))
                    
  ### Define all ecoregions 
  ecoList <- c( "Aberdeen Plains"  ,"Abitibi Plains and Riviere Rupert Plateau"  ,"Acadian Plains and Hills"
                ,"Ahklun and Kilbuck Mountains"  ,"Alaska Peninsula Mountains"  ,"Alaska Range"  ,"Aleution Islands" 
                ,"Algonquin/Southern Laurentians"  ,"Amundsen Plains"  ,"Arctic Coastal Plain"  ,"Arctic Foothills"  
                ,"Arizona/New Mexico Mountains"  ,"Arizona/New Mexico Plateau"  ,"Arkansas Valley"  
                ,"Aspen Parkland/Northern Glaciated Plains"  ,"Athabasca Plain and Churchill River Upland"  
                ,"Atlantic Coastal Pine Barrens"  ,"Baffin and Torngat Mountains"  ,"Baffin Uplands"  
                ,"Baja Californian Desert"  ,"Balsas Depression with Low Tropical Deciduous Forest and Xerophytic Shrub"  
                ,"Banks Island and Amundsen Gulf Lowlands"  ,"Blue Mountains"  ,"Blue Ridge"  ,"Boston Mountains"  
                ,"Bristol Bay-Nushagak Lowlands"  ,"Brooks Range/Richardson Mountains"  
                ,"California Coastal Sage, Chaparral, and Oak Woodlands"  ,"Canadian Rockies"  
                ,"Cascades"  ,"Central American Sierra Madre with Conifer, Oak, and Mixed Forests"  
                ,"Central Appalachians"  ,"Central Basin and Range"  ,"Central California Valley"  
                ,"Central Corn Belt Plains"  ,"Central Great Plains"  ,"Central Irregular Plains"  
                ,"Central Laurentians and Mecatina Plateau"  ,"Central Ungava Peninsula and Ottawa and Belcher Islands"  
                ,"Chiapas Depression with Low Deciduous and Medium Semi-Deciduous Tropical Forest"  
                ,"Chiapas Highlands with Conifer, Oak, and Mixed Forest"  ,"Chihuahuan Desert"  
                ,"Chilcotin Ranges and Fraser Plateau"  ,"Clear Hills and Western Alberta Upland"  
                ,"Coast Range"  ,"Coastal Hudson Bay Lowland"  
                ,"Coastal Plain and Hills with High and Medium-High Evergreen Tropical Forest and Wetlands"  
                ,"Coastal Plain with Low Tropical Deciduous Forest" ,"Coastal Western Hemlock-Sitka Spruce Forests" 
                ,"Colorado Plateaus","Columbia Mountains/Northern Rockies"  ,"Columbia Plateau" ,"Cook Inlet" 
                ,"Copper Plateau"  ,"Coppermine River and Tazin Lake Uplands"  ,"Cross Timbers"  ,"Cypress Upland" 
                ,"Driftless Area"  ,"East Central Texas Plains"  ,"Eastern Cascades Slopes and Foothills"   
                ,"Eastern Corn Belt Plains"  ,"Eastern Great Lakes Lowlands","Edwards Plateau"
                ,"Ellesmere and Devon Islands Ice Caps","Ellesmere Mountains and Eureka Hills","Erie Drift Plain"
                ,"Flint Hills","Foxe Uplands","Great Bear Plains","Gulf of Boothia and Foxe Basin Plains"
                ,"Gulf of Mexico Coastal Plain with Wetlands and High Tropical Rain Forest","Hay and Slave River Lowlands"
                ,"Hayes River Upland and Big Trout Lake","High Plains"
                ,"Hills and Interior Plains with Xeric Shrub and Mesquite Low Forest"
                ,"Hills and Sierra with Low Tropical Deciduous Forest and Oak Forest"
                ,"Hills and Sierras with Conifer, Oak, and Mixed Forests"
                ,"Hills with High and Medium Semi-Evergreen Tropical Forest"
                ,"Hills with Medium and High Evergreen Tropical Forest"  ,"Hudson Bay and James Bay Lowlands"    
                ,"Huron/Erie Lake Plains"  ,"Idaho Batholith"    ,"Interior Bottomlands"  
                ,"Interior Forested Lowlands and Uplands"    ,"Interior Highlands and Klondike Plateau"  
                ,"Interior Plains and Piedmonts with Grasslands and Xeric Shrub"    ,"Interior Plateau"  
                ,"Interior River Valleys and Hills"    
                ,"Jalisco and Nayarit Hills and Plains with Medium Semi-Evergreen Tropical Forest"    
                ,"Kazan River and Selwyn Lake Uplands"  ,"Klamath Mountains"    
                ,"La Grande Hills and New Quebec Central Plateau"  
                ,"La Laguna Mountains with Oak and Conifer Forest"    
                ,"Lake Erie Lowland"  ,"Lake Manitoba and Lake Agassiz Plain"    
                ,"Lake Nipigon and Lac Seul Upland"  ,"Lancaster and Borden Peninsula Plateaus"    
                ,"Los Cabos Plains and Hills with Low Tropical Deciduous Forest and Xeric Shrub"  
                ,"Los Tuxtlas Sierra with High Evergreen Tropical Forest"    ,"Mackenzie and Selwyn Mountains"  
                ,"Madrean Archipelago"    ,"Maritime Lowlands"  ,"Mid-Boreal Lowland and Interlake Plain"    
                ,"Mid-Boreal Uplands and Peace-Wabaska Lowlands"  ,"Middle Atlantic Coastal Plain"    ,"Middle Rockies"  
                ,"Mississippi Alluvial Plain"    ,"Mississippi Valley Loess Plains"  ,"Mojave Basin and Range"    
                ,"Nayarit and Sinaloa Plain with Low Thorn Tropical Forest"  ,"Nebraska Sand Hills"    ,"Newfoundland Island"  
                ,"North Cascades"    ,"North Central Appalachians"  ,"North Central Hardwood Forests"    
                ,"Northeastern Coastal Zone"  ,"Northern Allegheny Plateau"    
                ,"Northern Appalachian and Atlantic Maritime Highlands"  ,"Northern Basin and Range"    
                ,"Northern Lakes and Forests"  ,"Northern Minnesota Wetlands"    ,"Northern Piedmont"  
                ,"Northwestern Glaciated Plains"  ,"Northwestern Great Plains"  
                ,"Northwestern Yucatan Plain with Low Tropical Deciduous Forest"    ,"Ogilvie Mountains"  
                ,"Ouachita Mountains"    ,"Ozark Highlands"  ,"Pacific and Nass Ranges"    ,"Pacific Coastal Mountains"  
                ,"Parry Islands Plateau"    ,"Peel River and Nahanni Plateaus"  ,"Piedmont"    
                ,"Piedmonts and Plains with Grasslands, Xeric Shrub, and Oak and Conifer Forests"  
                ,"Plain with Low and Medium Deciduous Tropical Forest"    
                ,"Plain with Medium and High Semi-Evergreen Tropical Forest"  
                ,"Queen Maud Gulf and Chantrey Inlet Lowlands"    ,"Ridge and Valley"  ,"Seward Peninsula"    
                ,"Sierra Madre Occidental with Conifer, Oak, and Mixed Forests"  
                ,"Sierra Madre Oriental with Conifer, Oak, and Mixed Forests"    ,"Sierra Nevada"  
                ,"Sierras of Guerrero and Oaxaca with Conifer, Oak, and Mixed Forests"    
                ,"Sierras of Jalisco and Michoacan with Conifer, Oak, and Mixed Forests"  
                ,"Sinaloa and Sonora Hills and Canyons with Xeric Shrub and Low Tropical Deciduous Forest"    
                ,"Sinaloa Coastal Plain with Low Thorn Tropical Forest and Wetlands"  
                ,"Skeena-Omineca-Central Canadian Rocky Mountains"    ,"Smallwood Uplands"  ,"Snake River Plain"    
                ,"Sonoran Desert"  ,"South Central Plains"    
                ,"South Pacific Hills and Piedmonts with Low Tropical Deciduous Forest"  ,"Southeastern Plains"  
                ,"Southeastern Wisconsin Till Plains" ,"Southern and Baja California Pine-Oak Mountains"   
                ,"Southern Coastal Plain","Southern Florida Coastal Plain"   ,"Southern Michigan/Northern Indiana Drift Plains"
                ,"Southern Rockies"  ,"Southern Texas Plains/Interior Plains and Hills with Xerophytic Shrub and Oak Forest" 
                ,"Southwestern Appalachians"    ,"Southwestern Tablelands"  ,"Strait of Georgia/Puget Lowland"    
                ,"Subarctic Coastal Plains"  ,"Sverdrup Islands Lowland"    
                ,"Tehuantepec Canyon and Plain with Low Tropical Deciduous Forest and Low Thorn Tropical Forest"  
                ,"Texas Blackland Prairies"   ,"Thompson-Okanogan Plateau" ,"Ungava Bay Basin and George Plateau"   
                ,"Valleys and Depressions with Xeric Shrub and Low Tropical Deciduous Forest","Victoria Island Lowlands"   
                ,"Wasatch and Uinta Mountains" ,"Water"   ,"Watson Highlands","Western Allegheny Plateau"  
                ,"Western Corn Belt Plains" ,"Western Gulf Coastal Plain"  
                ,"Willamette Valley","Wrangell and St. Elias Mountains"   ,"Wyoming Basin" ,"Yukon Flats"   
                ,"Yukon-Stikine Highlands/Boreal Mountains and Plateaus" ,"Custom Category")
  
  #loop over all elements as test for an exact match within the data
  for(i in seq_along(d1$ecoReg)){
    val <- d1$ecoReg[i]
    t1 <- unique(grepl(pattern = val, x = ecoList))
    if(TRUE %in% t1){
      d1$`Coded Values`[i] <- val
    }else{
      d1$`Coded Values`[i] <- "Custom Category"
    }
    if(d1$ecoReg[i] == ""){
      d1$`Coded Values`[i] <- NA
    }
  }
  # assign values x
  masterTable$`ECOREGION - Lookup picker` <- d1$`Coded Values`
  return(masterTable)
}