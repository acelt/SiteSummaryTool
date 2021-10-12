# Function to subset plots based on spatial join (from Attribute function)
SubsetPlots <- function(TDat_LMF, attribute_name){
  TDat_LMF_subset <- TDat_LMF[TDat_LMF[[attribute_title]] == paste0(attribute_name),]
}
