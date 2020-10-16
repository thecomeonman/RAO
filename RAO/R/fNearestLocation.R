#' A function to find the nearest location from a certain x, y
#' It returns an integer to pick the position in the vector of locations
#' 
#' @param nRadius_m radius within which to search around x, y
#' @import data.table
#' @import geosphere
#' @export
fNearestLocation = function(
   nLongitude_deg = 0,
   nLatitude_deg = 0,
   nRadius_m = 1000,
   vnLongitude_deg = 0,
   vnLatitude_deg = 0
) {

   # Building locations table
   dtLocations = data.table(
      Longitude_deg = vnLongitude_deg,
      Latitude_deg = vnLatitude_deg
   )[, 
      SNO := .I
   ]

   # calculate distance from x, y
#    dtLocations[,
#       Distance_deg :=
#       (((nLongitude_deg - Longitude_deg) ^ 2) + ((nLatitude_deg - Latitude_deg) ^ 2)) ^ 0.5
#    ]
   dtLocations[,
      Distance_deg := distHaversine(
         cbind(Longitude_deg, Latitude_deg),
         cbind(nLongitude_deg, nLatitude_deg)
      ) * 360 / 40000000
   ]

   dtLocationsClick = dtLocations[!is.na(Distance_deg)]

   if ( nrow(dtLocationsClick) == 0 ) {
      return ( 0 )
   }

   # Keep the point closest from x,y
   dtLocationsClick = dtLocationsClick[
      which.min(Distance_deg)
   ]

   if ( nrow(dtLocationsClick) == 0 ) {
      return ( 0 )
   }

   # Keep the point only if it's within nRadius_m
   dtLocationsClick = dtLocationsClick[
      Distance_deg < (nRadius_m * 360/40000000)
   ]

   if ( nrow(dtLocationsClick) == 0 ) {
      return ( 0 )
   }

   return ( dtLocationsClick[, SNO] )

}