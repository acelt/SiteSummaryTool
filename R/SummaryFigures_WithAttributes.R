SummaryFigures_WithAttributes <- function(SpeciesList, Species_plots_ecosite, EcologicalSite, 
                                              SummaryVar, Interactive, Attributed_Pks, EcoSitePlots, alpha = 0.2){
  
  #Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
  
  #Merge with species list so we can hover for scientific name
  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
    unique() 
  
  # the input tdat_lmf attributed already has atribute heres, no need to merge
  EcoSitePlots_Attributed <- EcoSitePlots[EcoSitePlots$EcologicalSiteId == EcologicalSite,]
  
  # Added in attribute title here instead of allotment name
  Species_plots_ecosite_attributed <- sp::merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
    dplyr::select(Species, ScientificName, CommonName, PrimaryKey, 
                  PlotID,  AH_SpeciesCover, 
                  AH_SpeciesCover_n, Hgt_Species_Avg, 
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                  Noxious, SG_Group, link, !!Attribute_Field, EcologicalSiteId) %>%
    dplyr::mutate_if(is.numeric, round , digits = 2) 
  
  # Get Noxious versus Non in Standard Format
  
  Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
  Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)
  
    #Ignoring NAs - make disclosure as this may overestimate cover
  #For summarizing across all plots
  
  # I THINK WE WANT TO ADD N AND CONF INT HERE TO REPLACE BOCPLOTS WITH POINTS AND ERROR BARS
  Species_cover_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
    mutate(Tally = 1) %>%
    group_by(Species, GrowthHabit , GrowthHabitSub , 
             Duration , Noxious , ScientificName , 
             CommonName , SG_Group, link, EcologicalSiteId) %>% 
    summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
    mutate_if(is.numeric, round , digits = 2)%>%
    dplyr::select(Species, ScientificName, 
                  AveragePercentCover, StandardDeviation,
                  MinCover, MaxCover, n, GrowthHabit, 
                  GrowthHabitSub, Duration, 
                  Noxious, CommonName, SG_Group, link, EcologicalSiteId)
  
  NoxNonPal_Fill <- c("grey75"  , "#D55E00")
  NoxNonPal_Dot <- c("grey33" , "#993300")
  ## Setting color for attribute title
  ## FIgure out how to not hardcode ALLOT_NAME and instead use attribute_title - DONE 
  Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed[[Attribute_Field]]))))
  
  #Remove NAs for plotting
  Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover), EcologicalSiteId==EcologicalSite)
  
  dodge1 <- position_dodge(width = 0.9)
  dodge2 <- position_dodge(width = 0.4)
  
  if(SummaryVar == "GrowthHabitSub"){
    if(Interactive){
      
      
      Plots <-  lapply(X = split(Species_plots_ecosite_attributed, Species_plots_ecosite_attributed[["GrowthHabitSub"]] , 
                                 drop = TRUE),
                       
                       FUN = function(Species_plots_ecosite_attributed){
                         
                         current_plot <- ggplot2::ggplot(Species_plots_ecosite_attributed[!is.na(Species_plots_ecosite_attributed$GrowthHabitSub)&Species_plots_ecosite_attributed$EcologicalSiteId == EcologicalSite,], 
                                                         aes(x = GrowthHabitSub, 
                                                             y = AH_SpeciesCover, 
                                                             text = paste("Primary Key: " , PrimaryKey , 
                                                                          "Plot ID: " , PlotID , "Species: " , 
                                                                          ScientificName , "Code: " , Species , 
                                                                          "Percent Cover: " , AH_SpeciesCover , 
                                                                          "Noxious: " , Noxious ,
                                                                          sep = "<br>"))) +
                           geom_boxplot(width = 1, outlier.shape = NA) +
                           theme_light() + # remove ylims here
                           theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                 axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                 axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                        axis.title.y = element_blank()) +
                           ggtitle(paste0("Percent Cover by Functional Group: " , 
                                          Species_plots_ecosite_attributed$GrowthHabitSub, ", Ecological Site: ", EcologicalSite)) +
                           coord_flip() + facet_grid(cols = vars(GrowthHabitSub) ,
                                                     rows = vars(Duration) ,
                                                     switch = "y" ,
                                                     scales = "free" , drop = TRUE)
                         
                         return(current_plot)
                       }
      )
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(Species_plots_ecosite_attributed, Species_plots_ecosite_attributed[["GrowthHabitSub"]] , drop = TRUE), 
                      FUN = function(Species_plots_ecosite_attributed){
                        
                        current_plot <- ggplot2::ggplot(Species_plots_ecosite_attributed[Species_plots_ecosite_attributed$EcologicalSiteId == EcologicalSite,], aes(x = GrowthHabitSub , y = AH_SpeciesCover, fill = Noxious)) +
                          geom_boxplot(width = .6 , position = dodge1) +
                          labs(y = "Percent Cover") + # remove ylims
                          theme_light() + 
                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                       axis.title.y = element_blank()) +
                          ggtitle(paste("Percent Cover by Functional Group:", 
                                        Species_plots_ecosite_attributed$GrowthHabitSub, ", Ecological Site:", EcologicalSite, sep = " "
                                        )) +
                          coord_flip() + facet_grid(cols = vars(GrowthHabitSub) ,
                                                    rows = vars(Duration) , switch = "y" ,
                                                    scales = "free" , drop = TRUE)
                        
                      
                        return(current_plot)
                      })
    }
  }
  
  if(SummaryVar == "Noxious"){
    if(Interactive){
      Plots <- Species_plots_ecosite_attributed %>% group_by(Noxious) %>% 
        filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
        ggplot2::ggplot((aes(x = Noxious,
                             y = AH_SpeciesCover,
                             text = paste("Primary Key : " , PrimaryKey, 
                                          "Plot ID: " , PlotID,
                                          "Species: " ,  ScientificName, 
                                          "Code: " , Species, 
                                          "Percent Cover: " , AH_SpeciesCover, 
                                          "Noxious: " , Noxious,  
                                          sep = "<br>")))) +
        geom_boxplot(width = .6 , outlier.shape = NA) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
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
      Plots <- Species_plots_ecosite_attributed %>% group_by(Noxious) %>% 
        filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
        ggplot2::ggplot((aes(x = Noxious , y = AH_SpeciesCover))) +
        geom_boxplot(width = .6 , position = dodge1) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
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
    PercentCover <- Species_plots_ecosite_attributed %>% subset(AH_SpeciesCover > 0.000000)
    # lets find max species cover here to adjust y axis
    maxcover <- max(PercentCover$AH_SpeciesCover)
    
    if(Interactive){
      Plots <-lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , PercentCover$Duration) , drop = TRUE),
                     FUN = function(PercentCover){
                       current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover,
                                                                          text = paste("PrimaryKey: ", PrimaryKey , 
                                                                                       "Plot ID: " , PlotID , "Species: " , 
                                                                                       ScientificName , "Code: " , Species , 
                                                                                       "Percent Cover: " , AH_SpeciesCover , "Noxious: " , 
                                                                                       Noxious , 
                                                                                       sep = "<br>"))) +
                         geom_boxplot(outlier.shape = NA) +
                         theme_light() +
                         labs(y = "Percent Cover") + 
                         ggtitle(paste("Percent Cover by Species, " , 
                                       PercentCover$GrowthHabitSub, 
                                       PercentCover$Duration ,
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
      Plots <- lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , 
                                                   PercentCover$Duration) , 
                                drop = TRUE),
                      FUN = function(PercentCover){
                        current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover)) +
                          geom_boxplot(width = .6 , position = dodge1) +
                          theme_light() +
                          labs(y = "Percent Cover") + 
                          ggtitle(paste("Percent Cover by Species, " , 
                                        PercentCover$GrowthHabitSub , 
                                        PercentCover$Duration, 
                                       EcologicalSite, sep = ",")) + 
                          theme(axis.title.y = element_blank()) +
                          coord_flip() +  facet_grid(cols = vars(GrowthHabitSub) , rows = vars(Duration) ,
                                                     switch = "y" , scales = "free" , drop = TRUE) 
                        return(current_plot)
                      })
      
    }
  }
  
  if(SummaryVar == "GroundCover"){
    
    #Prep
    #BareSoilCover
    #TotalFoliarCover
    #FH_TotalLitterCover
    #FH_RockCover
    
    Ground_Cover_Tall <- EcoSitePlots_Attributed %>% 
      dplyr::select(PrimaryKey, BareSoilCover , 
                    TotalFoliarCover , FH_TotalLitterCover , 
                    FH_RockCover, !!Attribute_Field) %>%
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>% mutate(Tally = 1) 
    if(Interactive){
      
      
      Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
        ggplot2::ggplot((aes(x = Indicator , y = Percent , 
                             text = paste("PlotID: " , PlotID, 
                                          "PrimaryKey: " , PrimaryKey , 
                                          "Indicator: " , Indicator ,
                                          "Percent Cover: " , Percent , 
                                          sep = "<br>" )))) +
        geom_boxplot(width = .6 , outlier.shape = NA) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
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
      Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
        ggplot2::ggplot((aes(x = Indicator , y = Percent))) +
        geom_boxplot(width = .6 , position = dodge1) +
        theme_light(base_size = 16) +
        scale_color_manual(values = Attribute_Fill, na.value="#000000") +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank()) + 
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
      
    }
    
    
  }
  
  if(SummaryVar == "Gap"){
    
    Gap <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey , 
                                                     GapCover_25_50 , GapCover_51_100 , 
                                                     GapCover_101_200 , GapCover_200_plus , 
                                                     GapCover_25_plus) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      mutate_if(is.numeric , round, digits = 2)
    
    #Plot prep
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , y = Percent , 
                                                text = paste("PlotID: " , PlotID , 
                                                             "PrimaryKey: ", PrimaryKey , 
                                                             "Gap Class (cm): " , Gap_Class_cm, 
                                                             "Percent Cover: " , Percent , 
                                                             sep = "<br>"))) +
        geom_boxplot() + coord_flip() + 
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
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , y = Percent)) +
        labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
             caption = paste("Percent cover of canopy gap in: ", 
                             EcologicalSite, sep = " ")) +
        geom_boxplot(position = dodge1) + 
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
    
    SoilStability <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                               SoilStability_All , 
                                                               SoilStability_Protected , 
                                                               SoilStability_Unprotected) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      mutate_if(is.numeric, round, digits = 2) %>% dplyr::filter(!is.na(Rating))
    
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = SoilStability , 
                               aes(x = Veg , y = Rating ,
                                   text = paste("Primary Key: " , PrimaryKey,
                                                "Plot ID: " , PlotID , 
                                                "Rating: " , Rating ,
                                                sep = "<br>"))) +
        geom_boxplot(position = dodge1) + 
        coord_flip() +
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
      Plots <- ggplot2::ggplot(data = SoilStability, aes(x = Veg , y = Rating)) +
        labs(x = "Vegetation cover class", 
             y = "Soil Stability Rating",
             caption = paste("Soil stability ratings in: ", 
                             EcologicalSite)) +
  geom_boxplot(position = dodge1) +
  coord_flip() + 
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
