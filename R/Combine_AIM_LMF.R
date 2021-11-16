  Combine_AIM_LMF <- function(TerrADat_Path, EDIT_List_Path, Internal, use_EDIT = TRUE){
   
     if(!Internal){
    TerrADat <- sf::st_read(dsn = TerrADat_Path , layer = "TerrADat")
    LMF <- sf::st_read(dsn = TerrADat_Path , layer = "LMF")
    TerrADat <- as.data.frame(TerrADat)
    TerrADat <- dplyr::select(TerrADat, -Shape)
    LMF <- as.data.frame(LMF)
    LMF <- dplyr::select(LMF, -Shape)}
    
    if(Internal){
    TerrADat <- TerrADat
    LMF <- LMF
     }
    
    #This method only works for ecosites in EDIT currently - there a re a number of older ecosites in teh LMF that arent present - these will be replaced by NAs using the following code
    #Added a check above to ask if this is what you want - otherwise just add R/F
    
    #Merge the dataframe with the full EcologicalSiteId and dropped R/F Id with the LMF
    if(use_EDIT){
      EDIT <- read.csv(file = paste0(EDIT_List_Path, "EDIT_public_ecological_site_list.csv"))
      
      #Read in full csv of ecological site ids from EDIT
      
      EDIT[["EcoSiteId_Stripped"]] <- gsub(EDIT[["new_es_symbol"]],
                                           pattern = "^[RF]", replacement = "")
      
      #Check to see if unique
      ecosite_lut <- unique(EDIT[,c("new_es_symbol" , "EcoSiteId_Stripped")])
      
      #Pull out the repeat ids (fortunatley there are only 15)
      
      trouble_ids <- names(table(ecosite_lut[["EcoSiteId_Stripped"]]))[table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1]
      
      #Drop the repeat ids
      '%notin%' <- Negate('%in%')
      
      ecosite_lut_drop_bad <- ecosite_lut[ecosite_lut$EcoSiteId_Stripped %notin% trouble_ids,]
      
      #merge the lut with LMF table
      LMF_EcoSite <- merge(x = LMF , y = ecosite_lut_drop_bad, by.x = "EcologicalSiteId", by.y = "EcoSiteId_Stripped",  all.x = TRUE, all.y = FALSE)
      
      #Drop the EcologicalSiteId value that we added earlier
      LMF_EcoSite$EcologicalSiteId <- LMF_EcoSite$new_es_symbol
    } else {LMF_EcoSite <- LMF}
    
    # Read in csv of ecological site ids / PKs
    # Merge LUT with terradat and rename ecosite column
    
    if(Groups){
      TDat_grouped <- merge(x = TerrADat,
                            y = groups[,c("PrimaryKey", group_name)],
                            by = "PrimaryKey",
                            all.x = FALSE)
      
      TDat_grouped$EcologicalSiteId <- TDat_grouped[[group_name]]
    }
    
    if(!Groups){
      TDat_grouped <- TerrADat
    }
    # Characterize to prevent rbind factor issues
    TDat_grouped$EcologicalSiteId <- as.character(TDat_grouped$EcologicalSiteId)
    LMF_EcoSite$EcologicalSiteId <- as.character(LMF_EcoSite$EcologicalSiteId)
    
    # r bind function to add NAs to mismatched columns
    rbind.all.columns <- function(x, y) {
      
      x.diff <- setdiff(colnames(x), colnames(y))
      y.diff <- setdiff(colnames(y), colnames(x))
      
      x[, c(as.character(y.diff))] <- NA
      
      y[, c(as.character(x.diff))] <- NA
      
      return(rbind(x, y))
    }
    
    TDat_LMF <- rbind.all.columns(TDat_grouped, LMF_EcoSite)
    
    # add factor levels back in
    TDat_LMF$EcologicalSiteId <- as.factor(TDat_LMF$EcologicalSiteId)
    
    #Pull out year for future
    TDat_LMF$DateVisited <- sub("^(\\d{4}).*$", "\\1", TDat_LMF$DateVisited)
    TDat_LMF <- TDat_LMF %>% dplyr::rename(Year = DateVisited)
    
    
    return(TDat_LMF)
    
  }
  
