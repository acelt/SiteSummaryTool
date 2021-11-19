SubsetEcologicalSite <- function(TDat_LMF, Grouping_var){
  if(Groups){
    EcoSitePlots <- TDat_LMF[TDat_LMF[[group_name]] %in% Grouping_var, ]
  }
  
  if(!Groups){
    EcoSitePlots <- TDat_LMF[TDat_LMF[["EcologicalSiteId"]] %in% Grouping_var,]
  }
  
# Add sagebrush indicator
  EcoSitePlots <- EcoSitePlots %>%
                 mutate(AH_SagebrushCover_Dead = AH_SagebrushCover - AH_SagebrushCover_Live)
   
  return(EcoSitePlots)
}

