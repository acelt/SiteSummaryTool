PlotMetadata <- function(dataframe){
  Plots <- c("Plots")

  #Create color palettes 
  Plots_Simple <- dataframe %>% 
          dplyr::select(PrimaryKey, EcologicalSiteId) %>%   
             dplyr::mutate(PlotsPerYear = Plots)

  PlotsPerYear <- ggplot2::ggplot(Plots_Simple , aes(x= Plots, text = stat(count), fill = EcologicalSiteId)) +
    geom_bar(position = position_stack(reverse = TRUE), width = .2) +
    scale_fill_brewer(palette = 7, type = "div") +
    ggtitle("Plots Per Ecoregion") +
    theme_minimal()+
    coord_flip() + theme(axis.text.y = element_blank())
  
  return(PlotsPerYear)

}
