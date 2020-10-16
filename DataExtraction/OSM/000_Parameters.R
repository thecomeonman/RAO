library(data.table)
library(bit64)

# Point to a location on your disk under which intermediate files, etc. will
# get stored

cAtherDataLocation = Sys.getenv('AtherDataLocation')

# Input parms
# ==============================================================================

# This is just a label which you will associate with this area and which will keep
# all the artifacts related to this area linked together. It can be the name
# of a city, an area, or the name of the person who is responsible for the area,
# whatever - just a name. 
cCityName = 'RAOExample'

# 40k kms = 360 degrees
nEarthCircumference_m = 40000000

# Input file name
# This is just to get the coordinates within which we want to get the OSM data
# If you don't have a KML, you can leave that path as is and create the vnBox 
# variable instead
cRefKMLFilePath = paste0(
   cAtherDataLocation,
   "/Raw/RAO/",
   cCityName, "/",
   cCityName, "ServiceArea.kml"
)

# bounding box around the area you're interested in
# This will get ignored if the kml file pointed to by cRefKMLFilePath exists
# The larger the span, the more RAM you need
vnBox = c(
   77.54, # bottom left long
   12.92, # bottom left lat
   77.65, # top right long
   13.00  # top right lat
)


# Will have the raw downloaded OSM data after get_XML
cMapXMLFilePath = paste0(
   cAtherDataLocation,
   "/Raw/RAO/",
   cCityName, "/",
   "RawOSMData.xml"
)

# The nodes and paths for the serviceable area are extracted, processed
# and stored in this after cleanOSM
cOSMServiceableAreaFile = paste0(
   cAtherDataLocation,
   '/Processed/RAO/',
   cCityName,
   '/OSMServiceable.Rdata'
)

# If there are some nodes or ways that need addition or removal, this string
# is the channel to do it.
# Refer to fEditOSMObjects to understand syntax for this
vcMapEdits = c()


# When checking coverage from a candidate location, this is the upper limit
# of the distance that coverage will be checked for
# The larger the value, the more RAM you need
nDistanceCutoff_m = 6000

# To calculate coverage, we find a polygon of OSM nodes around each location.
# The location istelf may not be located exactly on a node. Therefore we
# use the vertices of the polygon to calculate the distance from any other
# node to the polygon as a proxy. The algorithm tries to find the closest set of
# points to make the polygon but in some cases that may not be possible, in
# which case, the algorithm finds the best polygon such thta no vertex is
# more than nMaxVertexDistanceFromPoint_m distane away from the location
nMaxVertexDistanceFromPoint_m = nDistanceCutoff_m * 0.05



# LocationIDs that you don't want processed can be mentioned here
vcStationsToExclude = c()
# vcStationsToExclude = setdiff(c(1:9999999), 1608) # process only 1608
# vcStationsToExclude = c('21', '166', '354', '422', '451', '543', '544', '591', '661', '766', '803', '918', '922') # process everything else except the ones here
vcStationsToExclude = as.integer64(vcStationsToExclude)

# If T, deletes the existing coverage data for the locations which
# are in the current locations file, and replaces it with the recalculated
# coverage. Keeps the coverage data for locations not in this locations file.
# If F, doesn't do the coverage calculation for stations already in the
# coverage data and calculates coverage only for new locations
# If you've changed nMaxVertexDistanceFromPoint_m or nDistanceCutoff_m then
# let this be T
bOverwriteExistingCoverageCalculation = T


# the file which will have details of the coverage of each location
cCoverageFileName = paste0(
   cAtherDataLocation,
   "/Processed/RAO/",
   cCityName, "/",
   '/dtPolygonCoveredEdgesServiceable.Rdata'
)

# if you're running GenerateRandomLocations then this is th enumber of location
# that will randomly be chosen
# The larger the value, the more RAM you need
iRandomStationsToGenerate = 50

# Rhis is where the random locations generated in GenerateRandomLocations will
# be saved. This is what needs to be uploaded to the upload scenario button
# in the dashboard.
cLocationsFile = paste0(
   cAtherDataLocation,
   '/Raw/RAO/',
   cCityName,
   '/Locations.csv'
)
