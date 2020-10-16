#' @import data.table
#' @export
fGetBoundCoordinatesFromKMLString = function ( cBound ) {

   # Bounding OSM data, etc. by only the serviceable area
   dtBoundCoordinates = data.table(
      Coordinates = unlist(
         strsplit(
            cBound,
            ' '
         )
      )
   )

   dtBoundCoordinates[,
      Longitude_deg :=
      gsub(x = Coordinates, pattern = ',.*,.*$', replacement = ''),
      Coordinates
   ]


   dtBoundCoordinates[,
      Latitude_deg :=
      gsub(x = Coordinates, pattern = '.*,(.*),.*$', replacement = '\\1'),
      Coordinates
   ]

   dtBoundCoordinates = dtBoundCoordinates[Coordinates != '']

   return ( dtBoundCoordinates )

}