
#' Find the enclosing node polygon. That's what the edge span is calculated on.
#' @param vcPointsID Location IDs of the points
#' @param vnPointsLatitude_deg Respective Lats of the location IDs
#' @param vnPointsLongitude_deg Respective Longs of the location IDs
#' @param vNodeID Node IDs of the points
#' @param vnNodeLongitude_deg Respective Lats of the Node IDs
#' @param vnNodeLatitude_deg Respective Longs of the Node IDs
#' @param nMaxVertexDistanceFromPoint_m If the polygon nodes are going beyond
#' this then take the closest point instead.
#' @import data.table
#' @import geosphere
#' @export
fEnclosingNodePolygon = function (
   vcPointsID,
   vnPointsLatitude_deg,
   vnPointsLongitude_deg,
   vNodeID,
   vnNodeLongitude_deg,
   vnNodeLatitude_deg,
   nMaxVertexDistanceFromPoint_m = 250
) {

   # Preparing the location table
   dtLocations = data.table(
      LocationID = vcPointsID,
      Longitude_deg = vnPointsLongitude_deg,
      Latitude_deg = vnPointsLatitude_deg
   )
   rm(vcPointsID)
   rm(vnPointsLongitude_deg)
   rm(vnPointsLatitude_deg)

   # Preparing the nodes table
   dtNodes = data.table(
      NodeID = vNodeID,
      Longitude_deg = vnNodeLongitude_deg,
      Latitude_deg = vnNodeLatitude_deg
   )
   rm(vNodeID)
   rm(vnNodeLongitude_deg)
   rm(vnNodeLatitude_deg)

   # Looping through each location to find the polygon
   dtPolygonNodes = rbindlist(
      lapply(
         dtLocations[, LocationID],
         function( iLocationID ) {

            # isolating attributes of the location
            dtLocation = dtLocations[LocationID == iLocationID]

            # The distance will provide a sorting order to find a
            # polygon
            # dtNodes[,
            #    Distance_deg := sqrt(
            #       ((dtLocation[, Longitude_deg] - Longitude_deg) ^ 2) +
            #       ((dtLocation[, Latitude_deg] - Latitude_deg) ^ 2)
            #    )
            # ]
            dtNodes[,
               Distance_deg := distHaversine(
                  cbind(dtLocation[, Longitude_deg], dtLocation[, Latitude_deg]),
                  cbind(Longitude_deg, Latitude_deg)
               ) * 360 / 40000000
            ]

            if ( dtNodes[, any(Distance_deg == 0)] ) {

               dtHull = dtNodes[Distance_deg == 0]

            } else {
                  

               setkey(dtNodes, Distance_deg)

               # We will try and find a polygon starting with the closest three points
               viPolygon = 1:3

               repeat {

                  # Finding the convex hull around the selected points
                  # This means some points close to the location might
                  # get kicked out for points farther away but the
                  # distance of 250 m is too small to significantly affect
                  # results
                  dtHull = dtNodes[
                     dtNodes[
                        viPolygon,
                        chull(Longitude_deg, Latitude_deg)
                     ]
                  ]

                  # Calculating angle subtended on to location by vertices of hull
                  # Multiples of 360 deg means location is surrounded
                  nSubtendedAngle = sum(
                     sapply(
                        1:dtHull[,.N],
                        function (iHullRow1) {

                           iHullRow2 = iHullRow1 + 1
                           if (iHullRow1 == dtHull[,.N]) {
                              iHullRow2 = 1
                           }
                           P12 = sqrt(
                              ((dtHull[iHullRow1 , Latitude_deg] - dtLocation[, Latitude_deg]) ^ 2) +
                              ((dtHull[iHullRow1, Longitude_deg] - dtLocation[, Longitude_deg]) ^ 2)
                           )

                           P13 = sqrt(
                              ((dtHull[iHullRow2, Latitude_deg] - dtLocation[, Latitude_deg]) ^ 2) +
                              ((dtHull[iHullRow2, Longitude_deg] - dtLocation[, Longitude_deg]) ^ 2)
                           )

                           P23 = sqrt(
                              ((dtHull[iHullRow2, Latitude_deg] - dtHull[iHullRow1, Latitude_deg]) ^ 2) +
                              ((dtHull[iHullRow2, Longitude_deg] - dtHull[iHullRow1, Longitude_deg]) ^ 2)
                           )

                           nAngle_rad = acos(((P12^2) + (P13^2) - (P23^2)) / (2 * P12 * P13))

                           nAngle_rad


                        }
                     )
                  ) / ( 2 * pi )

                  if ( abs(nSubtendedAngle - 1) < 0.000001 ) {

                     break

                  } else if (dtHull[, max(Distance_deg) >  100 * 360 / nEarthCircumference_m]) {

                     # If distance has crossed 100m then just pick the closest point
                     dtHull = dtHull[which.min(Distance_deg)]
                     break

                  } else {

                     # add the next closest point to the polygon
                     viPolygon = c(viPolygon, max(viPolygon)+1)

                  }


                  # This was just for diagnostics. Ignore.
                  if ( F ) {

                     print(
                        ggplot() +
                           geom_point(data = dtHull, aes(x = Longitude_deg, y = Latitude_deg)) +
                           geom_point(data = dtLocation, aes(x = Longitude_deg, y = Latitude_deg), color = 'red') +
                           coord_fixed()
                     )

                     Sys.sleep(.5)

                  }

               }

            }

            dtHull[, LocationID := iLocationID]

            return ( dtHull )

         }
      )
   )

   # Converting to metre distance
   dtPolygonNodes[, Distance_m := Distance_deg * nEarthCircumference_m / 360]
   dtPolygonNodes[, Distance_deg := NULL]

   # Flashing warning if any point exceeds this distance
   if ( any(dtPolygonNodes[,  Distance_m > nMaxVertexDistanceFromPoint_m ] ) ) {

      # dtPolygonNodes[,
      #    hist(Distance_deg  * nEarthCircumference_m / 360 , breaks = 100)
      # ]

      warning('Some polygon nodes are greater than nMaxVertexDistanceFromPoint_m')

   }

   return ( dtPolygonNodes )

}
