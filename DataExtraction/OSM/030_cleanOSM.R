rm(list = ls())

library(ggmap)
library(data.table)
library(igraph)
library(bit64)
library(ggplot2)
library(RAO)
library(geosphere)
theme_set(theme_bw(12))

cAtherGitRepo = Sys.getenv('AtherGitRepo')
source(
    paste0(
        cAtherGitRepo,
        '/RAO/DataExtraction/OSM/000_Parameters.R'
    )
)











# Service area bounds
# ==============================================================================

# hard coded bounds, in case the KML doesn't exist.
# Default is to pick up from KML

if ( file.exists(cRefKMLFilePath) ) {

   # Parsing the KML file and getting the coordinates we want data within.
   dtBoundCoordinates = fGetServiceableAreaFromKML(cRefKMLFilePath)


} else {

   cBound = c(
      paste(vnBox[1], vnBox[2], 0, sep = ','),
      paste(vnBox[3], vnBox[2], 0, sep = ','),
      paste(vnBox[3], vnBox[4], 0, sep = ','),
      paste(vnBox[1], vnBox[4], 0, sep = ',')
   )

   dtBoundCoordinates = fGetBoundCoordinatesFromKMLString(cBound)

}












# Setting up input data
# ==============================================================================

dir.create(paste0(cAtherDataLocation, '/Raw/RAO/',cCityName,'/'), recursive = T, showWarnings = F)
dir.create(paste0(cAtherDataLocation, '/Processed/RAO/',cCityName,'/'), recursive = T, showWarnings = F)

dtNodes = fread(
   paste0(cAtherDataLocation, '/Raw/RAO/',cCityName,'/OSMData/nodedetails.txt'),
   colClasses = c('character','numeric','numeric')
)

dtWays = fread(
   paste0(cAtherDataLocation, '/Raw/RAO/',cCityName,'/OSMData/ways.txt'),
   colClasses = c('character','character','character','character','character')
)

dtWays = dtWays[SourceID != DestinationID]
dtWays = dtWays[
   Type %in% c(
      'motorway',
      'motorway_link',
      'trunk',
      'trunk_link',
      'primary',
      'primary_link',
      'road',
      'secondary',
      'secondary_link',
      'service',
      'tertiary',
      'tertiary_link',
      'residential',
      'unclassified',
      'living_street'
   )
]

# Keeping only nodes which are in a path
dtWays = merge(
   dtNodes[,
      list(
         DestinationID = NodeID,
         Latitude_deg2 = Latitude_deg,
         Longitude_deg2 = Longitude_deg
      )
   ],
   merge(
      dtNodes[,
         list(
            SourceID = NodeID,
            Latitude_deg1 = Latitude_deg,
            Longitude_deg1 = Longitude_deg
         )
      ],
      dtWays,
      'SourceID'
   ),
   'DestinationID'
)

# dtWays[OneWay == 'no' & DestinationID > SourceID, c('DestinationID','SourceID') := list(SourceID, DestinationID)]
setkey(dtWays, SourceID, DestinationID)
dtWays = unique(dtWays)

# adding a distance column
# dtWays[, Distance_m := sqrt(((Latitude_deg2 - Latitude_deg1)^2) + ((Longitude_deg2 - Longitude_deg1)^2)) * 40000 * 1000 / 360]
dtWays[, Distance_m := distHaversine(cbind(Longitude_deg1, Latitude_deg1), cbind(Longitude_deg2, Latitude_deg2))]

# Assigning a serial number to the way
dtWays[, WayID := .I]















# Keeping only the nodes actually connected to the rest of the city
# ==============================================================================
# Removes paths inside apartments, etc. which you can't really access
# from the rest of the city

if ( F ) {

   # keeping only paths which are connected to at least one other path
   # Two paths connected to each but not connected to anything else
   # can sneak through.
   # Replacing this logic with a more robust and faster logic.
   vcValidPaths = sapply(
      dtWays[, unique(PathID)] ,
      function(cPathID) {

         cNodeIDs = dtWays[
            PathID == cPathID,
            c(SourceID, DestinationID)
         ]

         if (
            dtWays[
               SourceID %in% cNodeIDs |
                  DestinationID %in% cNodeIDs,
               length(unique(PathID)) > 1
            ]
         ) {
            return (cPathID)
         } else {
            return ( NULL )
         }
      }
   )

   dtWays = dtWays[PathID %in% unlist(vcValidPaths)]

}

dtNodes = dtNodes[
   NodeID %in% dtWays[, c(SourceID, DestinationID)]
]

vnDisconnectedPaths = fGetDisconnectedPaths(
   dtNodes,
   dtWays
)

dtWays = dtWays[
  !PathID %in% vnDisconnectedPaths
]

dtNodes = dtNodes[
   NodeID %in% dtWays[,
      c(SourceID, DestinationID)
   ]
]

rm(vnDisconnectedPaths)














# If there are some corrections to be made in OSM
# ==============================================================================

lOSMObjects = fEditOSMObjects(
   dtWays,
   dtNodes,
   vcMapEdits
)

dtWays = lOSMObjects$dtWays
dtNodes = lOSMObjects$dtNodes
rm(lOSMObjects)

# Debugging missing nodes, etc.
if ( F ) {

   load(paste0('./Processed/RAO/',cCityName,'/ggMapTiles.Rdata'))

   # vcNodesWeCareAbout = c(4017395225)
   vcNodesWeCareAbout = c(446892459)

   vnLongitudeRange = dtNodes[NodeID %in% vcNodesWeCareAbout, range(Longitude_deg)] + c(-0.001, 0.001)
   vnLatitudeRange = dtNodes[NodeID %in% vcNodesWeCareAbout, range(Latitude_deg)] + c(-0.001, 0.001)

   ggplotCorrectionsMap = ggmap(ggMapTiles)  +
      geom_segment(
         data = dtWays,
         aes(
            x = Longitude_deg1,
            y = Latitude_deg1,
            xend = Longitude_deg2,
            yend = Latitude_deg2,
            color = Type
         ),
         size = 1
      ) +
      geom_point(
         data = dtNodes,
         aes(
            x = Longitude_deg,
            y = Latitude_deg
         ),
         size = 1,
         shape = 21,
         fill = 'green',
         color = 'black'
      ) +
      coord_cartesian(xlim = vnLongitudeRange, ylim = vnLatitudeRange)

   ggplotCorrectionsMap

}











# Adding travel time
# ==============================================================================
# Not really adding. That logic should come here eventually when it does.


# Was planning to do this by travel time earlier but not doing it. Placeholder
# value for travel time so that the other code around this doesn't break.
# WorstTrafficOSM.Rdata is where the actual travel times should go, if you
# add it.
if ( !'TravelTime_s' %in% colnames(dtWays) ) {
   dtWays[, TravelTime_s := 0]
}

# saving for later use
# save(
#    list = c('dtWays','dtNodes'),
#    file = paste0(cAtherDataLocation, '/Raw/RAO/',cCityName,'/OSMData/TrimmedOSMData.Rdata')
# )

dtNodes = dtNodes[, list(NodeID, Latitude_deg, Longitude_deg)]

# Saving this object for later use
# save(
#    list = c('dtWays','dtNodes'),
#    # file = paste0('/Raw/RAO/',cCityName,'/WorstTrafficOSM.Rdata')
#    file = paste0(cAtherDataLocation, '/Raw/RAO/',cCityName,'/OSM.Rdata')
# )

#











# Keeping only paths and nodes that are within the serviceable area
# ==============================================================================
# Why aren't we doing this earlier? Because those operations aren't costly
# but checking the coverage of each location is actually expensive. Could
# have done earlier also if we wanted to.

mBound <- as.matrix(
   dtBoundCoordinates[,
      list(
         Latitude_deg = as.numeric(Latitude_deg),
         Longitude_deg = as.numeric(Longitude_deg)
      )
   ]
)
rm(dtBoundCoordinates)

mNodes <- as.matrix(cbind(dtNodes$Latitude_deg, dtNodes$Longitude_deg))
# mNodes = mNodes[complete.cases(mNodes),]
vbInside <- in.out(mBound, mNodes)
# plot(mBound,type="n")
# polygon(mBound)
# points(mNodes,col=as.numeric(vbInside)+1)
rm(mNodes)
rm(mBound)

dtNodes = dtNodes[vbInside]
rm(vbInside)

dtWays = dtWays[SourceID %in% dtNodes[,NodeID] & DestinationID %in% dtNodes[,NodeID]]

setwd(cAtherDataLocation)
save(
   list = c('dtWays','dtNodes'),
   file = cOSMServiceableAreaFile
)
