PlotMetadata_alt <- function(dataframe, EcologicalSite){
  Plots <- c("Plots")

  #Create color palettes 
  Plots_Simple <- dataframe %>% 
    filter(EcologicalSiteId == EcologicalSite)%>%
          dplyr::select(PrimaryKey, Time_Period) %>%   
             dplyr::mutate(PlotsPerYear = Plots) %>%
                dplyr::arrange(Time_Period)

  PlotsPerYear <- ggplot2::ggplot(Plots_Simple , aes(x= Plots, text = stat(count))) +
    geom_bar(position = position_stack(reverse = TRUE) ,
             aes(fill = Time_Period) , width = .2) +
    scale_fill_brewer(palette = 7, type = "div") +
    ggtitle(paste("Plots Per Time Period in", EcologicalSite)) +
    theme_minimal()+
    coord_flip() + theme(axis.text.y = element_blank())

  
  return(PlotsPerYear)

}
