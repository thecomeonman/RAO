#' Mutating the organisms
#' @param object: The GA object thing
#' @param parent: GA also insists on another argument, which is the specific organism
#' @import data.table
#' @export
fCustomMutation = function (
   object,
   parent,
   dtLocations,
   dtWays,
   dtPolygonCoveredEdges
) {

   # Get the population 
   population = object@population
   
   # Pick a particular organism
   # The 1st corresponds to status = '?' locations which have been selected
   viSelected <- as.vector(object@population[parent, ])

   # Retrieve actual locations + status = T locations
   vcProposedLocationsIDs = dtLocations[Status == '?'][viSelected == 1, LocationID]
   vcAllChosenLocationIDs = c(
      dtLocations[Status == 'T', LocationID],
      dtLocations[Status == '?'][viSelected == 1, LocationID]
   )

   # Get road distance data
   dtPolygonCoveredEdges = merge(
      dtPolygonCoveredEdges,
      dtWays[, list(WayID, Distance_m)],
      'WayID'
   )

   # Roads covered by the stations
   dtAllChosenCoveredEdges = dtPolygonCoveredEdges[
      LocationID %in% vcAllChosenLocationIDs
   ]

   # Assigning each way as being closest to a particular location
   dtAllChosenCoverage = dtAllChosenCoveredEdges[,
      list(
         LocationID = LocationID[which.min(DistanceFromDestination_m)][1]
      ),
      list(
         WayID,
         Distance_m
      )
   ]
   
   # And summing up the covered nearest road length
   dtAllChosenCoverage = dtAllChosenCoverage[, 
      list(
         Coverage_m = sum(Distance_m) + 0.00001
      ), 
      LocationID
   ]

   # Keeping only selected locations
   dtProposedCoverage = dtAllChosenCoverage[
      LocationID %in% vcProposedLocationsIDs
   ]
   
   # Deciding how many selections to flip
   nPMutation = 0.2
   iToMutate = ceiling(nPMutation * length(vcProposedLocationsIDs))

   # If there is at least one station to be flipped then -
   if ( iToMutate >= 1 ) {

      vcToMute = c()

      if ( nrow(dtProposedCoverage) > 0 ) {

         # Pick stations to turn from 1 to 0
         # Probability of selection for this is proportional to 1/Coverage_m
         # Also stations with 0 coverage are muted since they are pointless
         vcToMute = sample(
            dtProposedCoverage[, LocationID],
            size = min(iToMutate, nrow(dtProposedCoverage)),
            prob = dtProposedCoverage[, 1/Coverage_m / sum(1/Coverage_m)]
         )
         
         vcToMute = unique(
            vcToMute, 
            dtAllChosenCoverage[Coverage_m == 0, LocationID]
         )

      }


      # To pick stations to turn from 0 to 1 we will re-evaluate coverage_m
      # after removing the stations to mute 
      dtToActivateCoverage = dtPolygonCoveredEdges[
         !LocationID %in% setdiff(vcAllChosenLocationIDs, vcToMute) &
         !WayID %in% dtAllChosenCoveredEdges[!LocationID %in% vcToMute, WayID],
         list(
            LocationID = LocationID[which.min(DistanceFromDestination_m)][1]
         ),
         list(
            WayID,
            Distance_m
         )
      ]

      vcToActivate = c()

      if ( nrow(dtToActivateCoverage) > 0 ) {

         dtToActivateCoverage = dtToActivateCoverage[, 
            list(
               Coverage_m = sum(Distance_m)
            ), 
            LocationID
         ]
         
         # Deciding how many stations to flip from 0 to 1
         iToMutate = min(
            round(iToMutate * ((runif(1) * 2)), 0), 
            nrow(dtToActivateCoverage)
         )
         
         # Probability of seleciton proportoinal to Coverage_m
         vcToActivate = sample(
            dtToActivateCoverage[, LocationID],
            size = min(iToMutate, nrow(dtToActivateCoverage)),
            prob = dtToActivateCoverage[, Coverage_m / sum(Coverage_m)]
         )

      }

      # Deciding new location list
      vcProposedLocationsIDs = setdiff(
         vcProposedLocationsIDs,
         vcToMute
      )
      
      vcProposedLocationsIDs = c(
         vcProposedLocationsIDs,
         vcToActivate
      )

      viSelected = as.integer(dtLocations[Status == '?'][, LocationID %in% vcProposedLocationsIDs])
      
   }
   
   return ( viSelected )

}