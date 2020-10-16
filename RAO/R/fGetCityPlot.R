#' Makes a ggMap plot based on the selected scenario
#' @import data.table
#' @import ggmap
#' @import ggplot2
#' @export
fGetCityPlot = function(
   ggMapTiles = NULL,
   selectizeInputPlotScenario = NULL,
   dtOptimisedLocations = NULL,
   dtLocations = NULL,
   dtWays = NULL,
   dtOptimisationCoverageSummmary = NULL,
   dtCoveredEdges = NULL,
   sliderLocationSize = 1,
   sliderCoverageSize = 1
) {

   if ( !is.null(ggMapTiles) ) {

      ggplotCity = ggmap(ggMapTiles)

      if ( selectizeInputPlotScenario == 'Only City' ) {

         # do nothing effectively.
         # ggplotCity = ggplotCity

      }

      if ( !is.null(dtLocations) ) {

         vnLongitudeRange_deg = range(dtLocations[, Longitude_deg], na.rm = T)
         vnLatitudeRange_deg = range(dtLocations[, Latitude_deg], na.rm = T)

         nLongitudeSpan_deg = diff(vnLongitudeRange_deg)
         nLatitudeSpan_deg = diff(vnLatitudeRange_deg)
         nBiggestSpan_deg = pmax(nLatitudeSpan_deg, nLongitudeSpan_deg)

         if ( nLongitudeSpan_deg < nBiggestSpan_deg ) {

            nExtension_deg = (nBiggestSpan_deg - nLongitudeSpan_deg) / 2
            vnLongitudeRange_deg[1] = vnLongitudeRange_deg[1] - nExtension_deg
            vnLongitudeRange_deg[2] = vnLongitudeRange_deg[2] + nExtension_deg

         } else {

            nExtension_deg = (nBiggestSpan_deg - nLatitudeSpan_deg) / 2
            vnLatitudeRange_deg[1] = vnLatitudeRange_deg[1] - nExtension_deg
            vnLatitudeRange_deg[2] = vnLatitudeRange_deg[2] + nExtension_deg

         }


         if ( selectizeInputPlotScenario == 'Locations by Status' ) {

            ggplotCity = ggplotCity +
               geom_point(
                  data = dtLocations,
                  aes(
                     x = Longitude_deg,
                     y = Latitude_deg,
                     color = Status
                  ),
                  size = sliderLocationSize
               ) +
               scale_colour_manual(
                  breaks = c('T', 'F', '?'),
                  values = c('#ff7f00', '#7570b3', '#b15928')
               ) +
               coord_cartesian(
                  xlim = c(
                     vnLongitudeRange_deg[1] - 0.01,
                     vnLongitudeRange_deg[2] + 0.01
                  ),
                  ylim = c(
                     vnLatitudeRange_deg[1] - 0.01,
                     vnLatitudeRange_deg[2] + 0.01
                  )
                  # xlim = c(77.550607, 77.650607),
                  # ylim = c(13.053455,13.153455)
               ) +
               guides(
                  colour = guide_legend(
                     override.aes = list(size=2)
                  )
               )

         } else if ( selectizeInputPlotScenario == 'Locations by Chosen' ) {

            ggplotCity = ggplotCity +
               geom_point(
                  data = dtLocations,
                  aes(
                     x = Longitude_deg,
                     y = Latitude_deg,
                     color = Chosen
                  ),
                  size = sliderLocationSize
               ) +
               scale_colour_manual(
                  breaks = c('T', 'F', '?'),
                  values = c('#ff7f00', '#7570b3', '#b15928')
               ) +
               coord_cartesian(
                  xlim = c(
                     vnLongitudeRange_deg[1] - 0.01,
                     vnLongitudeRange_deg[2] + 0.01
                  ),
                  ylim = c(
                     vnLatitudeRange_deg[1] - 0.01,
                     vnLatitudeRange_deg[2] + 0.01
                  )
                  # xlim = c(77.550607, 77.650607),
                  # ylim = c(13.053455,13.153455)
               ) +
               guides(
                  colour = guide_legend(
                     override.aes = list(size=2)
                  )
               )

         } else if ( selectizeInputPlotScenario == 'Chosen locations and coverage' ) {

            if ( !is.null(dtCoveredEdges) ) {

               dtCoveredEdges = merge(
                  dtCoveredEdges,
                  dtWays[,
                     list(
                        WayID,
                        Longitude_deg1,
                        Latitude_deg1,
                        Longitude_deg2,
                        Latitude_deg2
                     )
                  ],
                  'WayID'
               )

               dtCoveredEdges = dtCoveredEdges[,
                  list(
                     DistanceFromDestination_m = min(DistanceFromDestination_m)
                  ),
                  list(
                     WayID,
                     Longitude_deg1,
                     Latitude_deg1,
                     Longitude_deg2,
                     Latitude_deg2
                  )
               ]

               ggplotCity = ggplotCity +
                  geom_segment(
                     data = dtWays[!WayID %in% dtCoveredEdges[,WayID]],
                     aes(
                        x = Longitude_deg1,
                        y = Latitude_deg1,
                        xend = Longitude_deg2,
                        yend = Latitude_deg2
                     ),
                     size = sliderCoverageSize,
                     color = 'red'
                  ) +
                  geom_segment(
                     data = dtCoveredEdges,
                     aes(
                        x = Longitude_deg1,
                        y = Latitude_deg1,
                        xend = Longitude_deg2,
                        yend = Latitude_deg2,
                        color = DistanceFromDestination_m
                     ),
                     size = sliderCoverageSize
                  ) +
                  geom_point(
                     data = dtLocations,
                     aes(
                        x = Longitude_deg,
                        y = Latitude_deg,
                        fill = Chosen
                     ),
                     shape = 21,
                     size = sliderLocationSize
                  ) +
                  scale_fill_manual(
                     breaks = c('T', 'F', '?'),
                     values = c('#ff7f00', '#7570b3', '#b15928')
                  ) +
                  scale_colour_continuous(
                     # low = '#31a354', high = '#e5f5e0',
                     low = '#005500', high = '#00FF00',
                     name = 'Distance (m)\nfrom nearest\nstation'
                  ) +
                  coord_cartesian(
                     xlim = c(
                        vnLongitudeRange_deg[1] - 0.01,
                        vnLongitudeRange_deg[2] + 0.01
                     ),
                     ylim = c(
                        vnLatitudeRange_deg[1] - 0.01,
                        vnLatitudeRange_deg[2] + 0.01
                     )
                     # xlim = c(77.550607, 77.650607),
                     # ylim = c(13.053455,13.153455)
                  ) +
                  guides(
                     colour = guide_legend(
                        override.aes = list(size=2)
                     ),
                     fill = guide_legend(
                        override.aes = list(size=2)
                     )
                  )

               }

         } else if ( selectizeInputPlotScenario == 'Chosen locations and redundancy' ) {

            if ( !is.null ( dtCoveredEdges ) ) {

               dtCoveredEdges = merge(
                  dtCoveredEdges,
                  dtWays[,
                     list(
                        WayID,
                        Longitude_deg1,
                        Latitude_deg1,
                        Longitude_deg2,
                        Latitude_deg2
                     )
                  ],
                  'WayID'
               )

               dtCoveredEdges = dtCoveredEdges[,
                  list(
                     .N
                  ),
                  list(
                     WayID,
                     Longitude_deg1,
                     Latitude_deg1,
                     Longitude_deg2,
                     Latitude_deg2
                  )
               ]

               ggplotCity = ggplotCity +
                  geom_segment(
                     data = dtWays[!WayID %in% dtCoveredEdges[,WayID]],
                     aes(
                        x = Longitude_deg1,
                        y = Latitude_deg1,
                        xend = Longitude_deg2,
                        yend = Latitude_deg2
                     ),
                     size = sliderCoverageSize,
                     color = 'red'
                  ) +
                  geom_segment(
                     data = dtCoveredEdges,
                     aes(
                        x = Longitude_deg1,
                        y = Latitude_deg1,
                        xend = Longitude_deg2,
                        yend = Latitude_deg2,
                        color = N
                     ),
                     size = sliderCoverageSize
                  ) +
                  geom_point(
                     data = dtLocations,
                     aes(
                        x = Longitude_deg,
                        y = Latitude_deg,
                        fill = Chosen
                     ),
                     shape = 21,
                     size = sliderLocationSize
                  ) +
                  scale_fill_manual(
                     breaks = c('T', 'F', '?'),
                     values = c('#ff7f00', '#7570b3', '#b15928')
                  ) +
                  scale_colour_continuous(
                     low = '#e5f5e0', high = '#31a354',
                     name = 'Station\nwithin\nradius'
                  ) +
                  coord_cartesian(
                     xlim = c(
                        vnLongitudeRange_deg[1] - 0.01,
                        vnLongitudeRange_deg[2] + 0.01
                     ),
                     ylim = c(
                        vnLatitudeRange_deg[1] - 0.01,
                        vnLatitudeRange_deg[2] + 0.01
                     )
                     # xlim = c(77.550607, 77.650607),
                     # ylim = c(13.053455,13.153455)
                  ) +
                  guides(
                     colour = guide_legend(
                        override.aes = list(size=2)
                     ),
                     fill = guide_legend(
                        override.aes = list(size=2)
                     )
                  )

            }

         }

      } else {

         ggplotCity = ggplot() +
            geom_text(
               aes(
                  x = 0,
                  y = 0,
                  label = 'Need to upload locations data / scenario. Or recalculate.'
               )
            )

      }

   } else {

      ggplotCity = ggplot() +
         geom_text(
            aes(
               x = 0,
               y = 0,
               label = 'Need to upload city data.'
            )
         ) +
         coord_cartesian()

   }

   return ( ggplotCity )

}
