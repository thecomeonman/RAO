#' Function to calculate what edges fall under what node's radius
#' @param igraphLinks Graph from dtWays. Look through server.r to find creation
#' @param cDestinationNodeID The node ID around which we need to find the coverage for.
#'    Note that that is the destination and not the source.
#' @import data.table
#' @import geosphere
#' @export
fEdgeSpan = function(
   igraphLinks,
   cDestinationNodeID,
   dtNodes,
   dtWays,
   nEarthCircumference_m = 40000000,
   nDistanceCutoff_m = 6000
) {

   # Isolating attributes of only that node.
   dtNode = dtNodes[cDestinationNodeID == NodeID]

   # dtNodes = dtNodes[
   #    NodeID %in% dtWays[, SourceID]
   # ]

   # Getting as-bird-flies distance of each node as a crude intial filter
#    dtNodes[, Distance_m := sqrt(
#       ((Longitude_deg - dtNode[, Longitude_deg]) ^ 2) +
#          ((Latitude_deg - dtNode[, Latitude_deg]) ^ 2)
#       ) * nEarthCircumference_m / 360
#    ]
   dtNodes[, 
      Distance_m := distHaversine(
         cbind(Longitude_deg, Latitude_deg), 
         cbind(dtNode[, Longitude_deg], dtNode[, Latitude_deg])
      )
   ]

   # mode in means the graph is actually traced from to to v, rather than v to to for the default mode out
   # shortest.paths(
   #       igraphLinks,
   #       to = c('1454644892','1383248107'),
   #       v = '1000852002',
   #       mode = 'in'
   #    )
   # dtWays[SourceID %in% 1000852002 | DestinationID %in% 1000852002 ]

   # Getting graph distance of all the nodes from destination
   mDistanceMatrix = shortest.paths(
      igraphLinks,
      to = dtNodes[Distance_m < nDistanceCutoff_m, NodeID],
      v = dtNode[, NodeID],
      mode = 'in'
   )

   # Debugging a particular node
   if ( F )  {

      # vcNodesToReach = dtNodes[Latitude_deg < 13.045 & Latitude_deg > 13.04 & Longitude_deg < 77.5950 & Longitude_deg > 77.5910, NodeID]
      vcNodesToReach = dtWays[WayID %in% vnWays, unique(c(SourceID, DestinationID))]
      vnxlim = c(77.5923, 77.6)
      vnylim = c(13.047, 13.1)

      qwe = get.shortest.paths(
         graph = igraphLinks,
         to = vcNodesToReach,
         from = dtPolygonNodes[, NodeID],
         mode = 'in'
      )


      ggplot() + 
         geom_point(data = dtNodes[Latitude_deg > vnylim[1] & Latitude_deg < vnylim[2] & Longitude_deg > vnxlim[1] & Longitude_deg < vnxlim[2]], aes(x = Longitude_deg, y = Latitude_deg), color = 'grey50') + 
         geom_point(data = dtNodes[NodeID %in% names(c(qwe$vpath[[1]]))], aes(x = Longitude_deg, y = Latitude_deg)) + 
         geom_segment(data = dtWays, aes(x = Longitude_deg1, y = Latitude_deg1, xend = Longitude_deg2, yend = Latitude_deg2), color = 'black') +
         geom_segment(data = dtWays[SourceID %in% names(c(qwe$vpath[[1]])) & DestinationID %in% names(c(qwe$vpath[[1]]))], aes(x = Longitude_deg1, y = Latitude_deg1, xend = Longitude_deg2, yend = Latitude_deg2), color = 'red') +
         geom_segment(data = dtWays[SourceID %in% names(c(qwe$vpath[[1]])) & DestinationID %in% names(c(qwe$vpath[[1]]))][OneWay == 'no'], aes(x = Longitude_deg1, y = Latitude_deg1, xend = Longitude_deg2, yend = Latitude_deg2), color = 'green') +
         geom_point(data = dtNode, aes(x = Longitude_deg, y = Latitude_deg), color = 'blue', size = 5) +
         geom_point(data = dtNodes[NodeID %in% vcNodesToReach[1]], aes(x = Longitude_deg, y = Latitude_deg), color = 'yellow', size = 5) +
         coord_cartesian(
               # xlim = c(77.585, 77.595),
               # ylim = c(13.02, 13.06)
               xlim = vnxlim,
               ylim = vnylim
         )

   }
   
   # Filtering the actual distance for the nDistanceCutoff_m
   vnDistanceMatrix = mDistanceMatrix[1,(mDistanceMatrix[1,] < nDistanceCutoff_m)]
   rm(mDistanceMatrix)

   # Getting the edges which have both nodes within the distance cutoff
   # We will lose edges which are partially within the boundary but that's okay
   dtSpan = dtWays[
         SourceID %in% c(names(vnDistanceMatrix), dtNode[, NodeID]) &
         DestinationID %in% c(names(vnDistanceMatrix), dtNode[, NodeID]),
      list(
         WayID,
         SourceID,
         DestinationID,
         # Latitude_deg1,
         # Longitude_deg1,
         # Latitude_deg2,
         # Longitude_deg2,
         Distance_m
      )
   ]

   # Taking the midpoint of the edge as the actual distance
   dtSpan[,
      DistanceFromDestination_m := pmin(
         vnDistanceMatrix[dtSpan[, SourceID]],
         vnDistanceMatrix[dtSpan[, DestinationID]]
      ) + (Distance_m/2)
   ][,
      c('SourceID','DestinationID','Distance_m') := NULL
   ]

   # Removing extra columns
   dtNodes[, Distance_m := NULL]

   return ( dtSpan )

}