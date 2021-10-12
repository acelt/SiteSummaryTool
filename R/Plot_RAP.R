Plot_RAP <-  function(filepath){
  
  # Read in csv from RAP trend estimates from csv 
  rap_estimates <- read.csv(filepath)%>%
    dplyr::rename(c("Annual Forb & Grass Cover"= "AFGC","Perennial Forb & Grass Cover" ="PFGC", "Shrub Cover"="SHR", "Tree Cover"="TREE", "Annual Temperature (°F)"="annualTemp", "Annual Precipitation (in)"="annualPrecip", "Bare Ground" = "BG"))%>%
    gather(key = "Indicator", value = "value",
           2:8)
  
  estimates <- rap_estimates[rap_estimates$Indicator != "Annual Temperature (°F)" & rap_estimates$Indicator != "Annual Precipitation (in)",] 
    
  covariates <- rap_estimates[rap_estimates$Indicator == "Annual Temperature (°F)" | rap_estimates$Indicator == "Annual Precipitation (in)",]
  
  p <- ggplot(mapping = aes(x = year, y= value)) +
    geom_col(data = covariates, aes(fill = Indicator), position = "dodge")+
    geom_line(data = estimates, aes(col = Indicator), size = 1.5)+
    theme_minimal(base_size = 16)+
    scale_color_brewer(type = "qual")+
    labs(y = "Cover(%)", x = NULL)+
    theme(legend.position = "bottom", legend.title = element_blank())
    
  return(p)
}