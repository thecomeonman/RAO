#' @import data.table
#' @import igraph
#' @export
fUpdateCoverageData = function (
   igraphLinks = NULL,
   dtWays,
   cCoverageFileName,
   bOverwriteExistingCoverageCalculation,
   dtLocationsToBeProcessed,
   nMaxVertexDistanceFromPoint_m = 250,
   nDistanceCutoff_m = 6000
) {

   if ( is.null(igraphLinks) ) {

      rm(igraphLinks)

   }

   if ( !exists('igraphLinks') ) {

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

   }

   if ( file.exists(cCoverageFileName) ) {

      load(cCoverageFileName)

      if ( bOverwriteExistingCoverageCalculation ) {

         dtPolygonCoveredEdges = dtPolygonCoveredEdges[
            !LocationID %in% dtLocationsToBeProcessed[, LocationID]
         ]
         
      } else {

         dtLocationsToBeProcessed = dtLocationsToBeProcessed[
            !LocationID %in% dtPolygonCoveredEdges[, LocationID]
         ]

      }

   } else {

      dtPolygonCoveredEdges = data.table()

   }



   # Calculating the road stretch covered. 
   # 10 locations at a time for sake of RAM
   viLocationIndicesToCover = 1:10
   iMaxWayID = dtWays[, max(WayID)]

   repeat {

      if ( min(viLocationIndicesToCover) > nrow(dtLocationsToBeProcessed) ) {

         break

      }
      
      viLocationIndicesToCover = viLocationIndicesToCover[viLocationIndicesToCover <= nrow(dtLocationsToBeProcessed)]

      # dtLocationsToBeProcessed = dtLocations[LocationID %in% '3043787000000262039']
      dtPolygonNodes = fEnclosingNodePolygon(
         vcPointsID = unlist(dtLocationsToBeProcessed[viLocationIndicesToCover, LocationID]),
         vnPointsLatitude_deg = unlist(dtLocationsToBeProcessed[viLocationIndicesToCover, Latitude_deg]),
         vnPointsLongitude_deg = unlist(dtLocationsToBeProcessed[viLocationIndicesToCover, Longitude_deg]),
         vNodeID = unlist(dtNodes[, NodeID]),
         vnNodeLongitude_deg = unlist(dtNodes[, Longitude_deg]),
         vnNodeLatitude_deg = unlist(dtNodes[, Latitude_deg]),
         nMaxVertexDistanceFromPoint_m = 250
      )

      # Probably remove these locations
      if ( F ) {

         ggplot() + 
            geom_point(data = dtNodes, aes(x = Longitude_deg, y = Latitude_deg)) + 
            geom_point(data = dtNodes[NodeID %in% dtPolygonNodes[Distance_m > 250 ,NodeID]], aes(x = Longitude_deg, y= Latitude_deg), size = 3, color = 'green') +
            geom_point(data= dtLocations[!LocationID %in% 661][LocationID %in% dtPolygonNodes[Distance_m > 250, LocationID]], aes(x = Longitude_deg, y= Latitude_deg), size = 3, color = 'red') +
            geom_text(data = dtLocations[!LocationID %in% 661][LocationID %in% dtPolygonNodes[Distance_m > 250, LocationID]], aes(x = Longitude_deg, y = Latitude_deg, label = LocationName), color = 'red') +
            # geom_segment(data = dtWays, aes(x = Longitude_deg1, y = Latitude_deg1, xend = Longitude_deg2, yend = Latitude_deg2)) +
            coord_fixed()

      }


      vcNodesToCover = dtPolygonNodes[, unique(NodeID)]

      dtPolygonCoveredEdgesIncrement = rbindlist(
         lapply(
            vcNodesToCover,
            function (iNodeID) {

               dtSpan = fEdgeSpan(
                  igraphLinks,
                  cDestinationNodeID = iNodeID,
                  dtNodes = dtNodes,
                  dtWays = dtWays,
                  nDistanceCutoff_m = nDistanceCutoff_m
               )

               dtSpan[, NodeID := iNodeID]

            }
         )
      )


      dtPolygonNodes = dtPolygonNodes[, list(NodeID, LocationID, Distance_m)]

      setkey(
         dtPolygonCoveredEdgesIncrement,
         WayID
      )

      dtPolygonCoveredEdgesIncrement = rbindlist(
         lapply(
            dtPolygonNodes[, unique( LocationID )],
            function(cLocationID) {

               gc()

               print('Processing location ID:')
               print(cLocationID)

               dtTemp = rbindlist(
                  lapply(
                     seq(ceiling(iMaxWayID / 1000)),
                     function( iWayGroup ) {

                        # print('iWayGroup:')
                        # print(iWayGroup)

                        dtTemp = merge(
                           dtPolygonNodes[
                              LocationID == cLocationID, 
                              list(NodeID,LocationID, DistanceLocationToNode_m = Distance_m)
                           ],
                           dtPolygonCoveredEdgesIncrement[
                              WayID > ((iWayGroup - 1) * 1000) &
                              WayID <= min((iWayGroup) * 1000, iMaxWayID)
                           ][,
                              list(NodeID, WayID, DistanceFromDestination_m)
                           ],
                           'NodeID',
                           allow.cartesian = T
                        )

                        if ( nrow(dtTemp) > 0 ) {

                           dtTemp[,
                              list(
                                 DistanceFromDestination_m = min(DistanceFromDestination_m + DistanceLocationToNode_m)
                              ),
                              list(
                                 LocationID,
                                 WayID
                                 # SourceID,
                                 # DestinationID,
                                 # Latitude_deg1,
                                 # Longitude_deg1,
                                 # Latitude_deg2,
                                 # Longitude_deg2,
                                 # Distance_m
                              )
                           ]

                        } else {

                           dtTemp = data.table()

                        }

                        dtTemp                        

                     }

                  ),
                  fill = T
               )

               dtTemp

            }
         ),
         fill = T
      )

      dtPolygonCoveredEdges = rbind(
         dtPolygonCoveredEdges,
         dtPolygonCoveredEdgesIncrement,
         fill = T
      )

      rm(dtPolygonCoveredEdgesIncrement)
      rm(dtPolygonNodes)

      viLocationIndicesToCover = viLocationIndicesToCover + length(viLocationIndicesToCover)

   }

   # rm(dtLocationsToBeProcessed)
   # rm(dtNodes)
   # rm(dtWays)
   # rm(vcNodesToCover)
   # rm(igraphLinks)


   if ( F ) {

      dtPolygonCoveredEdges = merge(
         dtPolygonCoveredEdges,
         dtWays[, 
            list(
               WayID,
               SourceID,
               DestinationID,
               Latitude_deg1,
               Longitude_deg1,
               Latitude_deg2,
               Longitude_deg2,
               Distance_m

            )
         ],
         by = 'WayID'
      )

      ggplot() + 
         geom_segment(data = dtWays, aes(x = Longitude_deg1, xend = Longitude_deg2, y = Latitude_deg1, yend = Latitude_deg2)) + 
         geom_segment(data = dtPolygonCoveredEdges, aes(x = Longitude_deg1, xend = Longitude_deg2, y = Latitude_deg1, yend = Latitude_deg2), color = 'green') + 
         geom_point(data = dtLocations, aes(x = Longitude_deg, y = Latitude_deg)) +
         coord_fixed(
               xlim = dtPolygonCoveredEdges[, range(c(Longitude_deg1, Longitude_deg2))],
               ylim = dtPolygonCoveredEdges[, range(c(Latitude_deg1, Latitude_deg2))]
         )

   }

   dtPolygonCoveredEdges[, LocationID := as.character(LocationID)]
   setDT(dtPolygonCoveredEdges)

   if ( file.exists(cCoverageFileName) ) {

      # For back up, renaming the current covered edges dataset
      # Doing it here so that this happens only when the complete function
      # has run. If it errored out and we renamed the file anyway then
      # other things might fail.
      
      file.rename(
         cCoverageFileName,
         paste0(
            gsub(
               x = cCoverageFileName,
               pattern = '\\.Rdata',
               replacement = ''
            ),
            strftime(Sys.time(), '%Y%m%d%H%M'),
            '.Rdata'
         )
      )

   }

   save(
      list = 'dtPolygonCoveredEdges',
      file = cCoverageFileName
   )

   return ( NULL )

}
