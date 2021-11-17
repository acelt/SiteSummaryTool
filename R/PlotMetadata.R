PlotMetadata <- function(dataframe, EcologicalSite){
  Plots <- c("Plots")

  #Create color palettes 
  Plots_Simple <- dataframe[dataframe$EcologicalSiteId == EcologicalSite,] %>% 
          dplyr::select(PrimaryKey, EcologicalSiteId) %>%   
             dplyr::mutate(PlotsPerYear = Plots)

  PlotsPerYear <- ggplot2::ggplot(Plots_Simple , aes(x= Plots, text = stat(count), fill = EcologicalSiteId)) +
    geom_bar(position = "dodge") +
    ggtitle("Count of Plots") +
    theme_minimal()+
    theme(legend.title = element_blank())+
    coord_flip() +
    theme(axis.text.y = element_blank(), legend.position = "bottom")
  
  return(PlotsPerYear)

}
