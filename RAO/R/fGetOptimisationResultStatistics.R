#' @import data.table
#' @export
fGetOptimisationResultStatistics = function (
   dtPolygonCoveredEdges,
   vcRawSolution,
   dtOptimisedLocations,
   dtWays,
   nRadiusUpperLimit_m
) {

   lOptimisationResult = list()

   vcSolution = rep('F', length(vcRawSolution[1,]))
   vcSolution[vcRawSolution[1,] == 1] = 'T'

   dtOptimisedLocations[Status != '?', Chosen := Status]
   dtOptimisedLocations[Status == '?', Chosen := vcSolution]
   dtOptimisedLocations[, Chosen := as.character(Chosen)]

   # Getting the necessary coverage details and storing them for later use
   dtCoveredEdges = dtPolygonCoveredEdges[
      LocationID %in% dtOptimisedLocations[ Chosen == 'T', LocationID ] &
      DistanceFromDestination_m <= nRadiusUpperLimit_m
   ]

   if ( nrow(dtCoveredEdges) == 0 ) {

      return ( NULL )

   }

   dtCoveredEdges = dtCoveredEdges[,
      list(
         LocationID,
         WayID,
         DistanceFromDestination_m
         # Longitude_deg1,
         # Latitude_deg1,
         # Longitude_deg2,
         # Latitude_deg2,
         # Distance_m
      )
   ]

   lOptimisationResult$dtCoveredEdges = dtCoveredEdges

   # Adding distance of roads
   dtCoveredEdges = merge(
      dtCoveredEdges,
      dtWays[, list(WayID, Distance_m)],
      'WayID'
   )

   # Counting how many statoins each road is covered by
   # pivotting this into the format that the datatable expects
   dtCoveredEdgesummary = merge(
      dtCoveredEdges[, list(Coverage = paste0('C/',pmin(.N, 2))), list(WayID)],
      dtCoveredEdges,
      all = T,
      c('WayID')
   )
   
   # dtCoveredEdgesummary[Coverage == 'C/2', Coverage := 'C/2+']
   # dtCoveredEdgesummary = dtCoveredEdgesummary[Coverage != 'C/2']
   
   dtOptimisationCoverageSummmary = dcast(
      dtCoveredEdgesummary,
      LocationID~Coverage,
      value.var = 'Distance_m',
      fun.aggregate = function(x) round(sum(x, na.rm = T)/1000, 0)
   )

   dtOptimisationCoverageSummmary = merge(
      dtCoveredEdgesummary[, list(ByAny = round(sum(Distance_m)/1000, 0)), LocationID],
      dtOptimisationCoverageSummmary,
      'LocationID'
   )

   dtOptimisationCoverageSummmary[, 'C/2' := NULL]

   setnames(
      dtOptimisationCoverageSummmary,
      'ByAny',
      'C'
   )

   lOptimisationResult$dtOptimisationCoverageSummmary = dtOptimisationCoverageSummmary

   # Calculating other basic stats about the solution to show to the user
   lOptimisationResult$nCoverage_pct = dtCoveredEdges[, list(Distance_m = Distance_m[1]), WayID][, sum(Distance_m)] / dtWays[, sum(Distance_m)]

   # This ensures that even stations that haven't been initiated but were part of the 
   # the solution get accommodated in this number
   lOptimisationResult$iStationsUsed = dtOptimisedLocations[ Chosen == 'T', length(unique(LocationID))]
   # iStationsUsed = dtCoveredEdges[, length(unique(LocationID))]
   lOptimisationResult$nMeanScore = dtOptimisedLocations[ Chosen == 'T', mean(Score)]

   return ( lOptimisationResult )
   
}