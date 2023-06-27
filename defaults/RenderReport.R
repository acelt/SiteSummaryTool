### Thank you for your interest in running the Ecological Site Summary Tool!
### Define all of the arguments below. 
### Make sure each command is run in order. 
### Your report may take several minutes to run, depending on your network connection
### The output will be in your WorkingFolder.
### Open the html output in Google Chrome or Mozilla Firefox. 

#Set the path to your R library (make sure it's on local folder not network drive)
#You must define this and run these commands everytime you restart R.
LibraryPath <- "C:/R/R-4.2.1/library"
.libPaths(LibraryPath)

# If you are a BLM employee, set BLM <- TRUE
# If you are an external data sharing partner, set BLM <- FALSE
# For BLM- Reminder- you must be connected to vpn or plugged into the network and have established ODBC connection
# See tutorial for how to establish ODBC connection (you must do this outside of R, once you've done it once, you don't need to do it again)
BLM <- TRUE
#If BLM <- TRUE, set TerrADat_Path to NA or NULL
#If BLM <- FALSE, set TerrADat_Path to the location of your TerrADat.gdb (i.e "C:/Users/name/Documents/TerrADat.gdb")
TerrADat_Path <- NA

# Where did you download  and unzip this reporting tool and supporting documents?
# Remember to keep all supporting file names and locations as they are in this folder structure
# Set this on your LOCAL drive. You will encounter errors if running this on a network drive. 
WorkingFolder <- "C:\\Users\\alaurencetraynor\\Documents\\Tools\\Shiny App\\SiteSummaryTool"
#Set your 2 letter state abbreviation, or a string of states if you are aggregating data across state lines
State <- "NM"

#Set your full EcologicalSiteId, or a string of EcologicalSiteIds if you are grouping
EcologicalSiteId <- "R036XB006NM"

#Are you including an allotment shapefile to append to summaries? 
#This currently functions for BLM Allotments only
#If so, set IncludeShape <- TRUE and fill out lines 38, 43, and 45
# If not, set IncludeShape <- FALSE and set lines 38, 43, and 45 to NA or NULL
IncludeAllotmentShapefile <- FALSE
# Right now this only works for BLM grazing allotments. An update to generalize this is in the works.
#Set shapefile name 
#Either the shapefile (without extension) if the shapefile lives in a folder
#Or layer name if it is a feature class in a geodatabase
shapefile_name <-  "Allotments"
#Path to either the shapefile folder or gdb (if gdb, include full .gdb extension)
shapefile_path <- "C:/Users/..."


## That's it! Now run the rest of the lines, but no more changes are necessary. 


Rmd <- paste0(WorkingFolder, "/defaults/", "ESS_Tool.Rmd", sep = "")

rmarkdown::render(Rmd, 
                  output_file = paste0(WorkingFolder, "/", 
                  (toString(EcologicalSiteId)), "_", Sys.Date(), sep = ""))

## If you encounter pandoc error, go into your library folder and manually delete the folder titled rmarkdown
## You may need to do this in multiple locations if .libPaths() returns more than 1 file location
## Then reinstall rmarkdown from within R studio (Tools -> Install Packages)

## If you encounter errors due to certain packages not being installed, manually install each one into your specified library

