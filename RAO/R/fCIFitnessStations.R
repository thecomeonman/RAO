
#' Fitness function to minimise the number of stations 
#' @param viSelected A vector of 0s and 1s to mark out which of the
#'  status = ? stations are selected in this solution
#' @param nRadiusUpperLimit_m Radius to be consiered to calculate coverage
#' @param nMinCoverage_pct Coverage min constraint imposed below which soln is invalid
#' @import data.table
#' @export
fCIFitnessStations = function(
   viSelected,
   nRadiusUpperLimit_m = 6000,
   nMinCoverage_pct = 100,
   dtLocations = NULL,
   dtWays = NULL,
   dtPolygonCoveredEdges = NULL
) {

   gc()

   # Selected stations
   vcLocationIDs = c(
      dtLocations[Status == 'T', LocationID],
      dtLocations[Status == '?'][viSelected == 1, LocationID]
   )

   # adding road distance information
   dtPolygonCoveredEdges = merge(
      dtPolygonCoveredEdges[LocationID %in% vcLocationIDs],
      dtWays[, list(WayID, Distance_m)],
      'WayID'
   )

   # Roads covered by the stations
   dtCoverage = dtPolygonCoveredEdges[,
      # DistanceFromDestination_m < nRadiusUpperLimit_m,
      list(
         # LocationID = LocationID[which.min(DistanceFromDestination_m)][1]
         1
      ),
      list(
         WayID,
         Distance_m
      )
   ]

   # dtCoverage = merge(
   #    dtCoverage,
   #    dtWays,
   #    c('WayID')
   # )

   # Calculating coverage
   nCoverage_pct = dtCoverage[, sum(Distance_m)] / dtWays[, sum(Distance_m)]

   if ( (nMinCoverage_pct) > nCoverage_pct ) {

      # invalid solution because min coverage not met
      # the negative sign is how we check for invalid solutions at the end
      # the 10 is just to, hopefully, make it even less valid
      return ( 10 * (nCoverage_pct - nMinCoverage_pct) )

   } else {

      # valid solution
      return ( 1/length(vcLocationIDs) )

   }

   # what factor to add to the score?

   # return ( composite of score factor and coverage )


}