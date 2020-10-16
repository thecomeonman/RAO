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

# Querying OSM
# ==============================================================================

dir.create(
   gsub(
      x = cMapXMLFilePath,
      pattern = '(.*)/.*',
      replacement = '\\1'
   ),
   showWarnings = F,
   recursive = T
)

system(
   sprintf(
      'curl -g -o %s -X GET "http://www.overpass-api.de/api/xapi?way[bbox=%f,%f,%f,%f][highway=*]"',
      cMapXMLFilePath,
      vnBox[1],
      vnBox[2],
      vnBox[3],
      vnBox[4]
   )
)