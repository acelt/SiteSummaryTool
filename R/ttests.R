### T test function
# need to use tdat long and we'll leave the species summary for now since it has overlapping observations
t.tests <-  function(TDat_LMF_Attributed, EcologicalSite, alpha){
  
  TDat_Cover_Indicators <-  c("AH_NoxAnnForbCover","AH_NoxAnnGrassCover","AH_NoxCover","AH_NonNoxAnnForbCover", "AH_PerenGrassCover","AH_PerenForbCover","AH_TallPerenGrassCover","AH_ShortPerenGrassCover")
  TDat_Cover_Indicators <-  enquo(TDat_Cover_Indicators)
  
  # Filter input dataset to include only relevant plots within the ecosite of interest
  TDat_LMF_Attributed <- TDat_LMF_Attributed[TDat_LMF_Attributed$EcologicalSiteId == EcologicalSite,] # dataset for ground cover and growth habit
  
  # Remove geometry if it exists
  if (any(class(TDat_LMF_Attributed) == "sf")){
    TDat_LMF_Attributed <- st_drop_geometry(TDat_LMF_Attributed)
  }
  
  # Summarizing Tdat to get sample stats across ecological site
  TDat_long <- TDat_LMF_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                     TotalFoliarCover , FH_TotalLitterCover , 
                                                     FH_RockCover, !!TDat_Cover_Indicators, SoilStability_All , 
                                                     SoilStability_Protected , 
                                                     SoilStability_Unprotected, GapCover_25_50 , GapCover_51_100 , 
                                                     GapCover_101_200 , GapCover_200_plus, 
                                                     GapCover_25_plus, !!attribute_title, Time_Period, EcologicalSiteId) %>% # removing un-needed cols
    gather(key = Indicator , value = Percent, BareSoilCover:GapCover_25_plus)%>% # MAKING LONG
    filter(!is.na(Percent))
  
  test <- lapply(X = split(TDat_long, TDat_long[["Indicator"]]),
                 FUN = function(TDat_long){
                   # SPILT INTO GROUPS
                   t1 <- TDat_long[TDat_long[["Time_Period"]]=="2011-2015",]
                   t2 <- TDat_long[TDat_long[["Time_Period"]]=="2016-2020",]
            
                   # logistic transformation
                 # TDat_long$Percent <- qlogis(TDat_long$Percent/100)

                   
                   # CHECK SAMPLE SIZES
                   if(all(is.na(t1$Percent)) || all(is.na(t2$Percent))){
                     output <- list(p.value = NA, means = list(NA,NA))
                     return(output)
                   } else if (nrow(t1)>3 & nrow(t2)>3) {
                     test <- t.test(data = TDat_long, Percent~Time_Period,
                                    conf.level = 1-alpha)
                     
                     p.value <- test$p.value
                     #means <- plogis(test$estimate)*100
                     means <- test$estimate
                     output <-  list(p.value, means)
                     return(output)
                     
                   } else {
                     output <- list(p.value = NA, means = list(NA,NA))
                     return(output)
                                    }
                 })
  return(test)
}
