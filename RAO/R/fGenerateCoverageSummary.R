
#' Get the road length covered within various bucketed radii
#' @import data.table
#' @export
fGenerateCoverageSummary = function (
   dtLocations,
   dtWays,
   dtPolygonCoveredEdges,
   nMaxRadiusUserWillUse_m = nMaxRadiusUserWillUse_m
) {

   # Keeping only relevant locations
   dtPolygonCoveredEdges = dtPolygonCoveredEdges[
      LocationID %in% dtLocations[, LocationID]
   ]

   if ( nrow(dtPolygonCoveredEdges) == 0 ) {
      return ( NULL )
   }

   # Getting road lengths from dtWays
   dtPolygonCoveredEdges = merge(
      dtPolygonCoveredEdges,
      dtWays[, list(WayID, Distance_m)],
      'WayID'
   )

   # Was too slow. Replaced it with the else logic
   if ( F ) {
      
      # Calculating incremental distance in each distance bracket
      # Each bracket: 2 * ceiling(DistanceFromDestination_m / 2000)
      dtCoverageSummary = dtPolygonCoveredEdges[
         # DistanceFromDestination_m <= 6000
         ,
         list(Coverage_km = sum(Distance_m) / 1000),
         list(
            LocationID,
            DistanceBracket_km = paste0(
               'C/',
               formatC(
                  2 * ceiling(DistanceFromDestination_m / 2000), 
                  width = 2,
                  flag = '0'
               ), 
               'kms'
            )
         )
      ][
         DistanceBracket_km != 'C/00kms'
      ]

   } else {

      dtPolygonCoveredEdges[,
         DistanceBracket1_km := 
         # paste0(
            # 'C/',
            # formatC(
               # ( nMaxRadiusUserWillUse_m / 1000 ) * round(DistanceFromDestination_m / ( 0.33 * nMaxRadiusUserWillUse_m ), 1)
               round( 
                  ( nMaxRadiusUserWillUse_m / 3000 ) * ceiling( DistanceFromDestination_m / ( nMaxRadiusUserWillUse_m / 3 ) ),
                  1
               )
            #    width = 2,
            #    flag = '0'
            # )
            # 'kms'
         # )
      ]

      # this is funky to cover cases where there are some buckets with
      # decimals, some without
      dtPolygonCoveredEdges[,
         DistanceBracket_km := 
         paste0(
            'C/',
            formatC(
               DistanceBracket1_km %/% 1.0,
               width = dtPolygonCoveredEdges[
                  DistanceFromDestination_m < nMaxRadiusUserWillUse_m, 
                  max(nchar(unique(DistanceBracket1_km) %/% 1.0))
               ],
               flag = '0'
            ),
            '.',
            ( 10 * DistanceBracket1_km) %% 10,
            'kms'
         ),
         DistanceBracket1_km
      ]

      dtPolygonCoveredEdges[, DistanceBracket1_km := NULL]

      dtCoverageSummary = dtPolygonCoveredEdges[
         # DistanceFromDestination_m <= 6000
         # DistanceBracket_km != 'C/00kms'
         DistanceFromDestination_m < nMaxRadiusUserWillUse_m
         ,
         list(Coverage_km = sum(Distance_m) / 1000),
         list(
            LocationID,
            DistanceBracket_km
         )
      ]

   }

   # Adding lesser radius coverage to the higher radius
   setkey(dtCoverageSummary, LocationID, DistanceBracket_km)
   dtCoverageSummary[, Coverage_km := round(cumsum(Coverage_km), 0), LocationID]

   # pivotting it for easier viewing
   dtCoverageSummary = dcast(
      dtCoverageSummary,
      LocationID~DistanceBracket_km
   )

   dtCoverageSummary

}

