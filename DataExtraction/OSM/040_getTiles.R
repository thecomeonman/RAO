library(XML)
library(data.table)

cAtherGitRepo = Sys.getenv('AtherGitRepo')
source(
    paste0(
        cAtherGitRepo,
        '/RAO/DataExtraction/OSM/000_Parameters.R'
    )
)


# Getting bounding box
# ==============================================================================

if ( file.exists(cRefKMLFilePath) ) {

   # Parsing the KML file and getting the coordinates we want data within.
   dtBoundCoordinates = fGetServiceableAreaFromKML(cRefKMLFilePath)

   # A 0.001 buffer shifts the boundaries of the polygon by 100m
   # Querying a sliiiiiiiightly bigger polygon than the service area they gave us
   # just to make sure all roads are captured
   vnBox = c(
      min(dtBoundCoordinates$Longitude_deg) - 0.0005,
      min(dtBoundCoordinates$Latitude_deg) - 0.0005,
      max(dtBoundCoordinates$Longitude_deg) + 0.0005,
      max(dtBoundCoordinates$Latitude_deg) + 0.0005
   )

}

# get the map data
ggMapTiles = get_map(
   location = make_bbox(
      c( vnBox[1], vnBox[3]),
      c( vnBox[2], vnBox[4]),
      f = 0.05
   ),
   zoom = 13,
   scale = 2,
   maptype = 'toner',
   source = 'stamen'
)

# ggmap(ggMapTiles)
setwd(cAtherDataLocation)
save(
   list = 'ggMapTiles', 
   file = paste0(cAtherDataLocation,'/Processed/RAO/',cCityName,'/ggMapTiles.Rdata')
)