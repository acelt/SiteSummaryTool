Time_Period_Figures <- function(EcologicalSite, SummaryVar, Interactive, TDat_LMF_Attributed, Species_plots_ecosite, SpeciesList, alpha = 0.2)
  {

  ################################################ DATA PREP ##################################################
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
  
  # Merge with species list so we can hover for scientific name
  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
    unique() 
  
  # Filter input datasetc to include only relevant plots within the ecosite of interest
  TDat_LMF_Attributed <- TDat_LMF_Attributed[TDat_LMF_Attributed$EcologicalSiteId == EcologicalSite,] # dataset for ground cover and growth habit
  Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed[Species_plots_ecosite_attributed$EcologicalSiteId == EcologicalSite,] # species dataset for species level indicators
  
  # Remove geometry if it exists
  if (any(class(TDat_LMF_Attributed) == "sf")){
    TDat_LMF_Attributed <- st_drop_geometry(TDat_LMF_Attributed)
  }
    
  # Added in attribute title here instead of allotment name
  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
    dplyr::select(Species, ScientificName, CommonName, PrimaryKey, 
                  PlotID,  AH_SpeciesCover, 
                  AH_SpeciesCover_n, Hgt_Species_Avg, 
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                  Noxious, SG_Group, link, !!attribute_title, Time_Period, EcologicalSiteId) %>%
    dplyr::mutate_if(is.numeric, round , digits = 2) 
  
  # Get Noxious versus Non in Standard Format
  Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
  Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)
  
  # Summarizing Tdat to get sample stats across ecological site
  TDat_summary <- TDat_LMF_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                        TotalFoliarCover , FH_TotalLitterCover , 
                                                        FH_RockCover, !!attribute_title, Time_Period, EcologicalSiteId) %>% # removing un-needed cols
    gather(key = Indicator , value = Percent, BareSoilCover:FH_RockCover) %>% # MAKING LONG
    group_by(Indicator, !!attribute_title, EcologicalSiteId, Time_Period) %>% 
    summarize(AveragePercentCover = mean(Percent),
              StandardDeviation = sd(Percent),
              MinCover = min(Percent),
              MaxCover = max(Percent), n = n()) %>%
    mutate_if(is.numeric, round , digits = 2) %>%
    mutate(se = StandardDeviation/sqrt(n),
           lower_ci = AveragePercentCover - qt(1-(alpha/2), n-1)*se,
           upper_ci = AveragePercentCover + qt(1-(alpha/2), n-1)*se)
  
  ### Summarize species level data (at ecosite level only)
  Species_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
    group_by(Species , GrowthHabit , GrowthHabitSub , 
             Duration , Noxious , ScientificName , 
             CommonName, link, EcologicalSiteId, Time_Period) %>% 
    summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = n()) %>%
    mutate_if(is.numeric, round , digits = 2) %>%
    mutate(se = StandardDeviation/sqrt(n),
           lower_ci = AveragePercentCover - qt(1-(alpha/2), n-1)*se,
           upper_ci = AveragePercentCover + qt(1-(alpha/2), n-1)*se)%>%
  dplyr::select(Species, ScientificName, 
                AveragePercentCover, StandardDeviation,
                MinCover, MaxCover, n, GrowthHabit, 
                GrowthHabitSub, Duration, 
                Noxious, CommonName, link, EcologicalSiteId, Time_Period, lower_ci, upper_ci)
  
  ### Summarize species level data (at growth habit level)
  GH_Species_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
    mutate(Tally = 1) %>%
    group_by(GrowthHabit , GrowthHabitSub , 
             Duration ,
            EcologicalSiteId, Time_Period) %>% 
    summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = n()) %>%
    mutate_if(is.numeric, round , digits = 2) %>%
    mutate(se = StandardDeviation/sqrt(n),
           lower_ci = AveragePercentCover - qt(1-(alpha/2), n-1)*se,
           upper_ci = AveragePercentCover + qt(1-(alpha/2), n-1)*se)%>%
    dplyr::select(GrowthHabit, 
                  GrowthHabitSub, Duration, 
                  EcologicalSiteId, Time_Period, AveragePercentCover, StandardDeviation,
                  MinCover, MaxCover, n,lower_ci, upper_ci)
  
  
  # Noxious Summary
  Noxious_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
    group_by(Noxious , EcologicalSiteId, Time_Period) %>% 
    summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = n()) %>%
    mutate_if(is.numeric, round , digits = 2) %>%
    mutate(se = StandardDeviation/sqrt(n),
           lower_ci = AveragePercentCover - qt(1-(alpha/2), n-1)*se,
           upper_ci = AveragePercentCover + qt(1-(alpha/2), n-1)*se)%>%
    dplyr::select(Noxious,  EcologicalSiteId, Time_Period, AveragePercentCover, StandardDeviation,
                  MinCover, MaxCover, n,lower_ci, upper_ci)
  
  # Gap_summary
  Gap_summary <-  TDat_LMF_Attributed %>% dplyr::select(PlotID, PrimaryKey , 
                                                   GapCover_25_50 , GapCover_51_100 , 
                                                   GapCover_101_200 , GapCover_200_plus , 
                                                   GapCover_25_plus, Time_Period, EcologicalSiteId) %>% 
    gather(key = Gap_Class_cm , 
           value = Percent , GapCover_25_50:GapCover_25_plus) %>%
    group_by(EcologicalSiteId, Time_Period, Gap_Class_cm)%>%
    summarize(AveragePercent = mean(Percent) ,
              StandardDeviation = sd(Percent),
              MinCover = min(Percent) ,
              MaxCover = max(Percent) , n = n()) %>%
    mutate(se = StandardDeviation/sqrt(n),
           lower_ci = AveragePercent - qt(1-(alpha/2), n-1)*se,
           upper_ci = AveragePercent + qt(1-(alpha/2), n-1)*se)%>%
    mutate_if(is.numeric , round, digits = 2)
  
  # Soil_summary
  Soil_summary <- TDat_LMF_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                             SoilStability_All , 
                                                             SoilStability_Protected , 
                                                             SoilStability_Unprotected,
                                                             Time_Period, EcologicalSiteId) %>%
    gather(key = Veg , value = Rating , 
           SoilStability_All:SoilStability_Unprotected) %>%
    mutate_if(is.numeric, round, digits = 2) %>% dplyr::filter(!is.na(Rating))%>%
    group_by(EcologicalSiteId, Time_Period, Veg)%>%
    summarise(AverageRating = mean(Rating) ,
              StandardDeviation = sd(Rating),
              MinRating = min(Rating) ,
              MaxRating = max(Rating) , n = n()) %>%
    mutate(se = StandardDeviation/sqrt(n),
           lower_ci = AverageRating - qt(1-(alpha/2), n-1)*se,
           upper_ci = AverageRating + qt(1-(alpha/2), n-1)*se)%>%
    mutate_if(is.numeric, round , digits = 2)
  
  ############################################## ADDITIONAL VARIABLES ##############################################
  
  NoxNonPal_Fill <- c("grey75"  , "#D55E00")
  NoxNonPal_Dot <- c("grey33" , "#993300")
  
  ## Setting color for attribute title
  Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed[[attribute_title]]))))
  dodge1 <- position_dodge(width = 0.9)
  dodge2 <- position_dodge(width = 0.4)
  
  #################################################### PLOTTING ##################################################
  
  if(SummaryVar == "GrowthHabitSub"){
    if(Interactive){
      
      Plots <-  lapply(X = split(GH_Species_summary, GH_Species_summary[["GrowthHabitSub"]] , 
                                 drop = TRUE),
                       
                       FUN = function(GH_Species_summary){
                         
                         current_plot <- ggplot2::ggplot(GH_Species_summary[!is.na(GH_Species_summary$GrowthHabitSub),], 
                                                         aes(x = GrowthHabitSub, 
                                                             y = AveragePercentCover, 
                                                             text = paste("Percent Cover: " , AveragePercentCover,
                                                                          "Ecological Site: ", EcologicalSiteId,
                                                                          "Time Period: ", Time_Period,
                                                                          "Upper Condfidence Interval: ", round(upper_ci,2),
                                                                          "Lower Condfidence Interval: ", round(lower_ci,2),
                                                                          sep = "<br>"),
                                                             fill = Time_Period)) +
                           
                           geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
                           geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                           theme_light() + 
                           theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                 axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                 axis.line.y = element_blank()) +
                           theme(panel.grid.major.y = element_blank(), axis.title.y = element_blank()) +
                           ggtitle(paste0("Percent Cover by Functional Group: " , 
                                          GH_Species_summary$GrowthHabitSub, ", Ecological Site: ", EcologicalSite)[1]) +
                           coord_flip() +
                           facet_grid(cols = vars(GrowthHabitSub) ,
                                                     rows = vars(Duration) ,
                                                     switch = "y" ,
                                                     scales = "free" , drop = TRUE)
                         
                         return(current_plot)
                       }
      )
    }
    
    if(!Interactive){
      
      Plots <- lapply(X = split(GH_Species_summary, GH_Species_summary[["GrowthHabitSub"]] , drop = TRUE), 
                      FUN = function(GH_Species_summary){
                        
                        current_plot <- ggplot2::ggplot(GH_Species_summary[!is.na(GH_Species_summary$GrowthHabitSub),],
                                                        aes(x = GrowthHabitSub ,
                                                            y = AveragePercentCover,
                                                            fill = Time_Period)) +
                          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
                          geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                          labs(y = "Percent Cover") +
                          theme_light() + 
                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                axis.line.y = element_blank()) +
                          theme(panel.grid.major.y = element_blank(),axis.title.y = element_blank()) +
                          ggtitle(paste("Percent Cover by Functional Group:", 
                                        GH_Species_summary$GrowthHabitSub, ", Ecological Site:", EcologicalSite)[1]) +
                          coord_flip() + facet_grid(cols = vars(GrowthHabitSub),
                                                    rows = vars(Duration) , switch = "y" ,
                                                    scales = "free" , drop = TRUE)
                        
                      
                        return(current_plot)
                      })
    }
  }
  
  if(SummaryVar == "Noxious"){
    if(Interactive){
      Plots <- ggplot2::ggplot(Noxious_summary,(aes(x = Noxious,
                             y = AveragePercentCover,
                             text = paste("Percent Cover: " , AveragePercentCover, 
                                          "Noxious: " , Noxious, 
                                          "Time Period: ", Time_Period, 
                                          sep = "<br>"),
                             fill = Time_Period))) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() ,
              axis.ticks.y = element_blank() , axis.line.y = element_blank() , 
              axis.title.x = element_blank()) +
        theme(panel.grid.major.y = element_blank() , legend.position = "none") +
        coord_flip() + 
        facet_grid(rows = vars(Noxious) , switch = "y" , scales = "free" , 
                   drop = TRUE) 
      return(Plots)
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(Noxious_summary,(aes(x = Noxious , y = AveragePercentCover, fill = Time_Period))) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() , 
              panel.grid.major.y = element_blank()) +
        coord_flip() + 
        facet_grid(rows = vars(Noxious) ,
                   switch = "y" , scales = "free" , drop = TRUE)
      
    }}
  
  if(SummaryVar == "Species"){
    
    if(Interactive){
      Plots <-lapply(X = split(Species_summary, list(Species_summary$GrowthHabitSub , Species_summary$Duration) , drop = TRUE),
                     FUN = function(Species_summary){
                       current_plot <- ggplot2::ggplot(Species_summary , aes(x = Species , y = AveragePercentCover,
                                                                          text = paste("Species: " , 
                                                                                       ScientificName , "Code: " , Species , "Common Name: ", CommonName,
                                                                                       "Percent Cover: " , AveragePercentCover , "Noxious: " , 
                                                                                       Noxious , 
                                                                                       "Time Period: ", Time_Period,
                                                                                       "Ecological Site: ", EcologicalSite,
                                                                                       sep = "<br>"),
                                                                          fill = Time_Period)) +
                         geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
                         geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                         theme_light() +
                         labs(y = "Percent Cover") + 
                         ggtitle(paste("Percent Cover by Species, " , 
                                       Species_summary$GrowthHabitSub, 
                                       Species_summary$Duration ,
                                       EcologicalSite)) + # removed tostring function
                         theme(axis.title.y = element_blank() ,
                               axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(), 
                               axis.title.x = element_blank())+   
                         theme(panel.grid.major.y = element_blank() ,
                               axis.title.y = element_blank()) +
                         coord_flip() +  facet_grid(rows = vars(Species),
                                                    scales = "free" , switch = "y",  drop = TRUE) 
                       return(current_plot)
                     })
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(Species_summary, list(Species_summary$GrowthHabitSub , 
                                                      Species_summary$Duration) , 
                                drop = TRUE),
                      FUN = function(Species_summary){
                        current_plot <- ggplot2::ggplot(Species_summary , aes(x = Species , y = AveragePercentCover, fill = Time_Period)) +
                          geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
                          geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                          theme_light() +
                          labs(y = "Percent Cover") + 
                          ggtitle(paste("Percent Cover by Species, " , 
                                        Species_summary$GrowthHabitSub , 
                                        Species_summary$Duration, 
                                       EcologicalSite, sep = ",")) + 
                          theme(axis.title.y = element_blank()) +
                          coord_flip() +  facet_grid(cols = vars(GrowthHabitSub) , rows = vars(Duration) ,
                                                     switch = "y" , scales = "free" , drop = TRUE) 
                        return(current_plot)
                      })
      
    }
  }
  
  if(SummaryVar == "GroundCover"){
    
    if(Interactive){
      
      
      Plots <-  ggplot2::ggplot(TDat_summary,(aes(x = Indicator , y = AveragePercentCover , 
                             text = paste("Indicator: " , Indicator ,
                                          "Percent Cover: " , AveragePercentCover , 
                                          "Time Period: ", Time_Period,
                                          "Ecological Site: ", EcologicalSite,
                                          sep = "<br>" ),
                             fill = Time_Period))) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(TDat_summary,(aes(x = Indicator , y = AveragePercentCover,
                                                 fill = Time_Period))) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
      
    }
    
    
  }
  
  if(SummaryVar == "Gap"){
    
    
    #Plot prep
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Gap_summary , aes(x = Gap_Class_cm , y = AveragePercent , 
                                                text = paste("Gap Class (cm): " , Gap_Class_cm, 
                                                             "Percent Cover: " , AveragePercent , 
                                                             "Time Period: ", Time_Period,
                                                             "Ecological Site: ", EcologicalSiteId,
                                                             sep = "<br>"),
                                                fill = Time_Period)) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) + coord_flip() + 
        theme_light() + 
        theme(axis.title.x = element_blank() ,
              axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() , 
              axis.title.y = element_blank() , 
              axis.line.y = element_blank(), 
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Gap_summary , aes(x = Gap_Class_cm , y = AveragePercent, fill = Time_Period)) +
        labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
             caption = paste("Percent cover of canopy gap in: ", 
                             EcologicalSite, sep = " ")) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip() + 
        theme_light(base_size = 16) + 
        scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
    }
    
  }
  
  if(SummaryVar == "SoilStability"){
    
    soil_labels <- c("SoilStability_All" = "All" , "SoilStability_Protected" = "Protected" , 
                     "SoilStability_Unprotected" = "Unprotected")
    
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Soil_summary , 
                               aes(x = Veg , y = AverageRating , fill = Time_Period, 
                                   text = paste("Ecological Site: ", EcologicalSiteId,
                                     "Time Period: ", Time_Period, 
                                                "Rating: " , AverageRating ,
                                                sep = "<br>"))) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip(ylim = c(0,6)) +
        theme_light() + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_blank()) +
        facet_grid(rows = vars(Veg) , switch = "y" ,
                   scales = "free_y" , drop = TRUE , 
                   labeller = as_labeller(soil_labels))
    }
    
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Soil_summary, aes(x = Veg , y = AverageRating, fill = Time_Period)) +
        labs(x = "Vegetation cover class", 
             y = "Soil Stability Rating",
             caption = paste("Soil stability ratings in: ", 
                             EcologicalSite)) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
  coord_flip(ylim = c(0,6)) +
  theme_light(base_size = 16) +
  scale_color_manual(values = Attribute_Fill, na.value="#000000") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  facet_grid(rows = vars(Veg),
             switch = "y",
             scales = "free_y", 
             drop = TRUE,
             labeller = as_labeller(soil_labels))
      
    }
    
  }
  
  return(Plots)
  
}
