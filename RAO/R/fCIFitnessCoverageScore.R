#' Fitness function to maximise the coverage
#' @param viSelected A vector of 0s and 1s to mark out which of the
#'  status = ? stations are selected in this solution
#' @param nRadiusUpperLimit_m Radius to be consiered to calculate coverage
#' @param iMaxStations The max stations constraints imposed above which soln is invalid
#' @import data.table
#' @export
fCIFitnessCoverageScore = function(
   viSelected,
   nRadiusUpperLimit_m = 6000,
   iMaxStations = Inf,
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

   # Adding distance information for each of the ways
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

   if ( length(vcLocationIDs) > iMaxStations ) {

      # invalid solution because max number of permissible stations exceeded
      # the negative sign is how we check for invalid solutions at the end
      return ( -length(vcLocationIDs) )

   } else {

      # valid solution
      return ( nCoverage_pct )   

   }

   # what factor to add to the score?

   # return ( composite of score factor and coverage )

   

}