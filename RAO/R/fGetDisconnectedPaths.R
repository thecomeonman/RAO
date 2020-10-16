#' dtRandomGrid is a set of random points from across the area
#' We check if each path in the data can reach points in dtRandomGrid
#' Paths which can't reach more than a certain number of points are excluded
#' The brute force way would be to check each path against each path but
#' that's too time consuming. We'll live with this heuristic.
#' @import igraph
#' @import data.table
#' @export
fGetDisconnectedPaths = function (
   dtNodes,
   dtWays
) {
   igraphLinks = graph_from_data_frame(
      d = rbind(
         dtWays[,
            list(
               from = SourceID,
               to = DestinationID,
               weight = Distance_m
            )
         ],
         dtWays[
            OneWay == 'no',
            list(
               from = DestinationID,
               to = SourceID,
               weight = Distance_m
            )
         ]
      ),
      directed = T
   )

   # Dividing the serviceable area into an x X y grid as per below (x,y)
   vnGridDimensions = c(3,3)
   # and randomly sapmlting a point from within each block in the grid
   dtRandomGrid = rbindlist(
      lapply(
         seq(vnGridDimensions[1]),
         function(x) {
            
            rbindlist(
               lapply(
                  seq(vnGridDimensions[2]),
                  function(y) {
                     
                     nRangeLatitude_deg = diff(dtNodes[, range(Latitude_deg)])
                     nRangeLongitude_deg = diff(dtNodes[, range(Longitude_deg)])
                     
                     # getting the nodes in the grid
                     dtNodesInGridBlock = dtNodes[
                        Latitude_deg >= ((nRangeLatitude_deg * ( y - 1 )/vnGridDimensions[2]) + dtNodes[, min(Latitude_deg)]) &
                        Latitude_deg <= ((nRangeLatitude_deg * ( y     )/vnGridDimensions[2]) + dtNodes[, min(Latitude_deg)]) &
                        Longitude_deg >= ((nRangeLongitude_deg * ( x - 1 )/vnGridDimensions[1]) + dtNodes[, min(Longitude_deg)]) &
                        Longitude_deg <= ((nRangeLongitude_deg * ( x     )/vnGridDimensions[1]) + dtNodes[, min(Longitude_deg)])
                     ]
                     
                     # randomly picking one
                     dtNodesInGridBlock = dtNodesInGridBlock[which.max(runif(nrow(dtNodesInGridBlock)))]
                     
                  }
               )
            )
            
         }
      )
   )

   mSampledDistanceMatrix = shortest.paths(
      graph = igraphLinks,
      v = dtRandomGrid[, NodeID],
      to = dtWays[, min(SourceID), PathID][, unique(V1)]
   )

   # Checking how many of the x*y distances are infinite
   # and if they are more than a certain number then assuming path isn't reachable
   vnInfiniteDistancesNodes = apply(
      mSampledDistanceMatrix, 
      2,
      function(x){sum(is.infinite(x))}
   )

   # No reason behind 5.
   # The 5 isn't 1 because there might be small sub-areas within which the path
   # is accessible but in general across the rest of the city it isn't. That whole
   # area should get removed as a result. That area is probably some weird island
   # disconnected from the rest of the city.
   vnInfiniteDistancesNodes = vnInfiniteDistancesNodes[
      vnInfiniteDistancesNodes > prod(vnGridDimensions) / 5
   ]

   # Filtering out thosep aths
   vnInfiniteDistancesPaths = dtWays[
      (
         SourceID %in% names(vnInfiniteDistancesNodes) |
         DestinationID %in% names(vnInfiniteDistancesNodes)
      ),
      unique(PathID)
   ]

   vnInfiniteDistancesPaths

}