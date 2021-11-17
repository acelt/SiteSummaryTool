# making a few changes to the make map function
MakeMap <- function(EcologicalSiteId, TDat_LMF){
  
  ##Caption to use in your tables and plots
  Caption <- paste0("Cover Summaries for " , Shapefile_Name)
  
  #Clean up
  TDat_LMF <- TDat_LMF %>%
    dplyr::arrange(EcologicalSiteId) %>%
    dplyr::filter(Latitude_NAD83 > 0)
  
  # List of ecological sites for legend
  EcoSiteList <- unique(TDat_LMF$EcologicalSiteId)
  
  #Set color palettes
  # removed palette for ecosite name as Im not using that
  Pal_EcoSite <- leaflet::colorFactor(palette = 'plasma' , domain = TDat_LMF$EcologicalSiteId)
  Pal_Date <- leaflet::colorFactor(palette = 'YlOrRd' , domain = TDat_LMF$Year)
  Year <- TDat_LMF$Year
  
  Map <- leaflet::leaflet(height = 650 , width = 650)
  
  #Convert vector to string to use in caption
  EcoSiteCaption <- toString(EcologicalSiteId)
  
  if(IncludeShapefile) {
    # Reading in Group polygon
    projection <- sf::st_crs("+proj=longlat +datum=NAD83")
    poly <- sf::st_read(dsn = Shapefile_Path, layer = Shapefile_Name)
    poly <- sf::st_transform(poly, crs = projection)
    poly <- methods::as(poly, "Spatial")
    
    # Select only the features of interest
    if(!is.na(Attribute_Name)){
      poly <- poly[poly[[Attribute_Field]] == Attribute_Name,]
    }
    
    if(is.na(Attribute_Name)){
      poly <- poly[!is.na(poly[[Attribute_Field]])]
    }
    
  
    # Making Map with polygon
    Map <- leaflet::addTiles(Map) %>% leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                                                                radius = 3 ,
                                                                fillOpacity = 0.5 ,
                                                                popup = paste("Ecological Site Id: " , TDat_LMF$EcologicalSiteId ,
                                                                              sep = "<br>") ,
                                                                color = ~Pal_EcoSite(TDat_LMF$EcologicalSiteId) , group = TDat_LMF$EcologicalSiteId ,
                                                                data = TDat_LMF) %>%
      leaflet::addLayersControl(overlayGroups = TDat_LMF$EcologicalSiteId,
                                options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
      leaflet::addLegend(pal = Pal_EcoSite , values = TDat_LMF$EcologicalSiteId , opacity = 1 , group = TDat_LMF$EcologicalSiteId) %>% # removing legend for now since it take sup mostof the page when used with ecoregions
      addPolygons(data = poly, fillColor = "transparent",
                  color = "black",
                  weight = 2)
    
    return(Map)
    
  } else {
    
    # Making Map without polygon
    Map <- leaflet::addTiles(Map) %>% leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 , radius = 3 ,
                                                                popup = paste("Ecological Site: " , TDat_LMF$es_name,
                                                                              "Ecolgical Site Id: " , TDat_LMF$EcologicalSiteId,
                                                                              sep = "<br>") ,
                                                                color = ~Pal_Date(Year) ,
                                                                fillOpacity = .5 , group = Year ,
                                                                data = TDat_LMF) %>%
      leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                                radius = 3 ,
                                fillOpacity = 0.5 ,
                                popup = paste("Ecological Site: " ,TDat_LMF$es_name ,
                                              "Ecological Site Id: " , TDat_LMF$EcologicalSiteId ,
                                              sep = "<br>") ,
                                color = ~Pal_EcoSiteID(TDat_LMF$EcologicalSiteId) , group = TDat_LMF$EcologicalSiteId ,
                                data = TDat_LMF) %>%
      leaflet::addLayersControl(overlayGroups = c(TDat_LMF$EcologicalSiteId , Year) ,
                                options = leaflet::layersControlOptions(collapsed = TRUE)) #%>%
      #leaflet::addLegend(pal = Pal_Date , values = TDat_LMF$Year , opacity = 1 , group = Year) %>%
      #leaflet::addLegend(pal = Pal_EcoSiteID , values = EcologicalSiteId , opacity = 1 , group = EcologicalSiteId) # removing legend for now as it takes up most of the page
    
    return(Map)
  }
  
}
