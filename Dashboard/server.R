function(input, output, session) {


   # initialising reactive values list
   lReactiveValues = reactiveValues(
      iTableRenderNumber = 0,
      ggplotMapWidth = 100,
      ggplotMapHeight = 100
   )

   plotCityBlank = ggplot() +
      geom_text(aes(x=0,y=0,label='No city data plotted yet')) +
      theme(
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         line = element_blank(),
         title = element_blank(),
         text = element_blank()
      )


   # --------------------------------------------------------------------------

   output$selectizeInputCities = renderUI({
      selectizeInput(
         inputId = 'selectizeInputCities',
         label = 'Pick City',
         choices = list.files(
            paste0(
               cAtherDataLocation,
               '/Processed/RAO'
            )
         ),
         selected = NULL,
         multiple = F
      )
   })

   # updating data directory to point to the respective city
	cDataDirectory = reactive({

		cDirectory = paste0(
         cAtherDataLocation,
         '/Processed/RAO/',
         input$selectizeInputCities
		)

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Changing working directory to ',
            cDirectory,
            'as per city choice\n'
         )
      )

      cDirectory

	})


   # uploading the locations file
   observeEvent(
      input$actionButtonUploadLocations,
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Uploading location data\n'
            )
         )

         progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Loading location data'
         )

         # reading in the file and some basic processing
         inFile <- input$fileInputUploadLocations

         if (is.null(inFile))
            return(NULL)


         # Getting coverage data for some other processing
         dtPolygonCoveredEdges = lReactiveValues$dtPolygonCoveredEdges
         if ( is.null(dtPolygonCoveredEdges) ) {

            showModal(modalDialog(
               title = "Error!",
               "No city data uploaded! Upload it and run this again.",
               easyClose = TRUE,
               footer = NULL
            ))

            return ( NULL )

         }


         tryCatch(
            {

               dtLocations = fread(inFile$datapath, stringsAsFactors = F)
               setkey(dtLocations, LocationID)
               dtLocations[, LocationID := as.character(LocationID)]
               dtLocations[, Longitude_deg := as.numeric(as.character(Longitude_deg))]
               dtLocations[, Latitude_deg := as.numeric(as.character(Latitude_deg))]
               dtLocations[, LocationName := as.character(LocationName)]
               dtLocations[, Score := as.numeric(Score)]

            },
            error = function(e) {

               showModal(modalDialog(
                  title = "Error! The app expects a file with 5 + 2 columns: LocationID, a character column for LocationName, numeric columns for Score, Latitude_deg, and Longitude_deg, and optionally columns for Status and Chosen.",
                  easyClose = TRUE,
                  footer = NULL
               ))

               return ( NULL )

            }
         )

         if ( dtLocations[,length(unique(LocationID))] != nrow(dtLocations) ) {

            vcDuplicates = dtLocations[,.N,LocationID][N>1][, LocationID]

            showModal(modalDialog(
               title = paste("Error! Duplicate locations IDs:", paste(vcDuplicates, collapse = ', ')),
               easyClose = TRUE,
               footer = NULL
            ))

         }



         # Fixing status column
         if ( !'Status' %in% colnames(dtLocations) ) {

            dtLocations[, Status := '?']

         } else {

            dtLocations[, Status := as.character(Status)]
            dtLocations[is.na(Status), Status := '?']

         }

         # Fixing chosen column
         if ( !'Chosen' %in% colnames(dtLocations) ) {

            dtLocations[, Chosen := 'F']

         } else {

            dtLocations[, Chosen := as.character(Chosen)]
            dtLocations[is.na(Chosen), Chosen := 'F']
            dtLocations[Chosen == 'TRUE', Chosen := 'T']
            dtLocations[Chosen == 'FALSE', Chosen := 'F']

         }

         # Sanity check for location IDs, etc.
         if ( !all(complete.cases(dtLocations)) ) {

            showModal(modalDialog(
               title = paste(
                  "Error! Some of the locations have missing values for the ID, name, or coordinates (",
                  paste(
                     dtLocations[!complete.cases(dtLocations), paste(LocationID,':', LocationName)],
                     collapse = ', '
                     # collapse  <br/> didn't work and \n didn't either
                  ),
                  ') Please remove / rectify. For now, these locations shall be excluded.'
               ),
               easyClose = TRUE,
               footer = NULL
            ))

         }

         dtLocations = dtLocations[complete.cases(dtLocations)]

         # Since data.tables are by reference, we need to trace updates to
         # location with posixctLastLocationUpload

         lReactiveValues$dtLocations = dtLocations
         lReactiveValues$posixctLastLocationUpload = Sys.time()



         # adding locations to the actionable locations selection box
         vcLocations = unlist(dtLocations[, LocationID])
         names(vcLocations) = dtLocations[, LocationName]
         selectizeInputToActionLocations = input$selectizeInputToActionLocations

         updateSelectizeInput(
            session = session,
            inputId = 'selectizeInputToActionLocations',
            choices = vcLocations,
            selected = intersect(vcLocations, selectizeInputToActionLocations)
         )


         dtWays = lReactiveValues$dtWays

         # Checking for locations not in coverage data
         progress$set(
            message = 'Checking for locations not in coverage data'
         )

         vcNewLocations = setdiff(
            dtLocations[, LocationID],
            dtPolygonCoveredEdges[, unique(LocationID)]
         )

         if ( length(vcNewLocations) > 0 ) {

            names(vcNewLocations) = dtLocations[
               LocationID %in% vcNewLocations,
               LocationName
            ]

         }

         updateSelectizeInput(
            session = session,
            inputId = 'selectizeInputNewLocations',
            choices = vcNewLocations,
            selected = vcNewLocations
         )

         # Calculating location coverage details
         progress$set(
            message = 'Calculating location coverage details'
         )

         lReactiveValues$dtCoverageSummary = fGenerateCoverageSummary(
            dtLocations = dtLocations,
            dtWays = dtWays,
            dtPolygonCoveredEdges = dtPolygonCoveredEdges,
            nMaxRadiusUserWillUse_m
         )


      }
   )


   # creating a datatable with checkboxes, etc. for the user to manipulate
   output$datatableLocations = renderDataTable(
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Creating the datatable of locations\n'
            )
         )

         progress = Progress$new(session)
         # on.exit(progress$close())

         progress$set(
            message = 'Preparing table'
         )

         iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber) + 1

         lReactiveValues$posixctLastLocationUpload

         # creatinga a copy becase we don't want to modify the original one
         dtLocations = copy(isolate(lReactiveValues$dtLocations))

         if ( is.null(dtLocations) ) {
            progress$close()
            return ( data.table(NODATA = NA) )
         }

         # Adding overall coverage summary details
         dtCoverageSummary = lReactiveValues$dtCoverageSummary
         if ( is.null(dtCoverageSummary) ) {
            progress$close()
            return ( data.table(NODATA = NA) )
         }

         print('unbinding has happened')

         # removing the previous radio button elements
         # http://stackoverflow.com/questions/40020600/r-shiny-selectedinput-inside-renderdatatable-cells/40027726#40027726
         session$sendCustomMessage('unbind-DT', 'datatableLocations')

         setkey(dtLocations, LocationID)

         # creating the radio button options
         dtLocations[,
            Status := fActOnShinyInput(
               cFunctionName = 'radioButtons',
               id = paste0('radioStatus', iTableRenderNumber),
               vcSuffixes = LocationID,
               choices = c('T','F','?'),
               selected = Status,
               inline = T,
               label = NULL
            ),
            Status
         ]

         dtLocations[,
            Chosen := fActOnShinyInput(
               cFunctionName = 'radioButtons',
               id = paste0('radioChosen', iTableRenderNumber),
               vcSuffixes = LocationID,
               choices = c('T','F'),
               selected = Chosen,
               inline = T,
               label = NULL
            ),
            Chosen
         ]

         # creating the radio button options
         dtLocations[,
            X := fActOnShinyInput(
               cFunctionName = 'checkboxInput',
               id = paste0('checkBoxActive', iTableRenderNumber),
               vcSuffixes = LocationID,
               value = F,
               label = NULL
            ),
            Chosen
         ]

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'dtLocations done\n'
            )
         )

         dtLocations = merge(
            dtLocations[, list(X, LocationID, LocationName, Score = round(Score,2), Status, Chosen)] ,
            dtCoverageSummary,
            'LocationID',
            all.x = T
         )

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'dtCoverageSummary done\n'
            )
         )


         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Performance fixes\n'
            )
         )

         # Adding optimisation coverage summary details
         dtOptimisationCoverageSummmary = lReactiveValues$lOptimisationResult$lEditedResults$dtOptimisationCoverageSummmary
         if ( !is.null(dtOptimisationCoverageSummmary) ) {

            dtLocations = merge(
               dtLocations,
               dtOptimisationCoverageSummmary,
               'LocationID',
               all.x = T
            )

            setkey(
               dtLocations,
               'C/1'
            )

            dtLocations = dtLocations[.N:1]

         } else {

            dtLocations[, c('V1','V2') := NA]
            setnames(
               dtLocations,
               c('V1','V2'),
               c('C','C/1')
            )

         }

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'dtOptimisationCoverageSummmary done\n'
            )
         )

         dtLocations[, LocationID := NULL]
         dtLocations[, Score := NULL]

         lReactiveValues$iTableRenderNumber = iTableRenderNumber

         progress$set(
            message = 'The table has been created. It may take a few seconds
            to load in the browser though. This pop up won\t disappear
            automatically so once the table is loaded, please close it
            yourself.'
         )

         return ( dtLocations )

      },
      server = TRUE,
      escape = FALSE,
      selection = 'none',
      filter = 'top',
      # extensions = c('FixedColumns',"FixedHeader"),
      # extensions = c('FixedHeader'),
      # rownames = FALSE,
      options = list(
         scrollY = '400px',
         autoWidth = TRUE,
         columnDefs = list(
            list(
               width = '40px',
               targets = c(1, 5, 6, 7, 8, 9)
            ),
            list(
               width = '80px',
               targets = 4
            ),
            list(
               width = '120px',
               targets = 3
            )
         ),
         # scrollX = F,
         paging = FALSE,
         preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
         drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
         # fixedHeader =TRUE
         # fixedColumns = list(leftColumns = 3, rightColumns = 0),
      )
   )



   # not used anywhere except when debugging is needed
   # observe({
   output$debugging = renderPrint({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Debugging chunk\n'
         )
      )

      dtLocations = (lReactiveValues$dtLocations)
      print(Sys.time())
      # qwe1 = (fRetrieveShinyValue(paste0('radioStatus', iTableRenderNumber), dtLocations[, LocationID]))
      # qwe2 = fRetrieveShinyValue(paste0('radioChosen', iTableRenderNumber), unlist(dtLocations[, LocationID]))
      qwe3 = fRetrieveShinyValue(
         paste0('checkBoxActive', iTableRenderNumber),
         unlist(dtLocations[, LocationID]),
         input
      )
      # print(str(qwe1))
      # print(str(qwe2))
      print(str(qwe3))


   })


   # loading the map, etc. data about the city
   observeEvent(
      input$actionButtonLoadCityData,
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Uploading city data, etc.\n'
            )
         )

      progress = Progress$new(session)
      on.exit(progress$close())

      # OSM data about ways and nodes
      progress$set(
         message = 'Loading map data'
      )

      load(
         paste0(
            cDataDirectory(),
            '/OSMServiceable.Rdata'
         )
      )

      setDT(dtNodes)
      setDT(dtWays)

      dtNodes = dtNodes[
         NodeID %in% dtWays[, SourceID] |
         NodeID %in% dtWays[, DestinationID]
      ]

      lReactiveValues$dtNodes = dtNodes
      lReactiveValues$dtWays = dtWays

      # the image tiles for the map plot
      progress$set(
         message = 'Loading map image'
      )

      load(
         paste0(
            cDataDirectory(),
            '/ggMapTiles.Rdata'
         )
      )

      lReactiveValues$ggMapTiles = ggMapTiles


      # data for the coverage of ways by each nodes
      progress$set(
         message = 'Loading coverage data'
      )


      load(
         paste0(
            cDataDirectory(),
            '/dtPolygonCoveredEdgesServiceable.Rdata'
         )
      )

      setDT(dtPolygonCoveredEdges)
      dtPolygonCoveredEdges = dtPolygonCoveredEdges[
         DistanceFromDestination_m <= nMaxRadiusUserWillUse_m,
         list(LocationID, WayID, DistanceFromDestination_m)
      ]
      # dtPolygonCoveredEdges[, setdiff(colnames(dtPolygonCoveredEdges), c('LocationID','WayID', 'DistanceFromDestination_m')) := NULL]
      # save(list = 'dtPolygonCoveredEdges', file = 'dtPolygonCoveredEdgesServiceable.Rdata')
      gc()
      setkey(dtPolygonCoveredEdges, LocationID)
      dtPolygonCoveredEdges[, LocationID := as.character(LocationID)]
      setkey(dtPolygonCoveredEdges, LocationID)
      lReactiveValues$dtPolygonCoveredEdges = dtPolygonCoveredEdges


      # checking if all loctions are present in the coverage data. Else need to pre-process
      dtLocations = isolate(lReactiveValues$dtLocations)

      if ( !is.null(dtLocations) ) {

         progress$set(
            message = 'Checking for locations not in coverage data'
         )

         vcNewLocations = setdiff(
            dtLocations[, LocationID],
            dtPolygonCoveredEdges[, LocationID]
         )

         if ( length(vcNewLocations) > 0 ) {

            names(vcNewLocations) = dtLocations[
               LocationID %in% vcNewLocations,
               LocationName
            ]

         }

         updateSelectizeInput(
            session = session,
            inputId = 'selectizeInputNewLocations',
            choices = vcNewLocations,
            selected = vcNewLocations
         )

         # Generating coverage summary
         progress$set(
            message = 'Generating location details'
         )

         dtPolygonCoveredEdges = lReactiveValues$dtPolygonCoveredEdges
         if ( is.null(dtPolygonCoveredEdges) ) {
            dtLocations = dtLocations[, list(X, LocationName, Status, Chosen)]
            return ( dtLocations )
         }

         lReactiveValues$dtCoverageSummary = fGenerateCoverageSummary(
            dtLocations = dtLocations,
            dtWays = dtWays,
            dtPolygonCoveredEdges = dtPolygonCoveredEdges,
            nMaxRadiusUserWillUse_m = nMaxRadiusUserWillUse_m
         )

      }


   })



   # map plotting with the locations, etc.
	output[['ggplotMap']] = renderPlot({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Plotting map\n'
         )
      )

      sliderPlotWidth = input[['sliderPlotWidth']]
      ggplotMapLatitudeRange_deg = lReactiveValues$ggplotMapLatitudeRange_deg

      if ( is.null(ggplotMapLatitudeRange_deg) ) {

         nHeight = 100
         nWidth = sliderPlotWidth

      } else {

         ggplotMapLongitudeRange_deg = lReactiveValues$ggplotMapLongitudeRange_deg

         if ( diff(ggplotMapLatitudeRange_deg) < diff(ggplotMapLongitudeRange_deg) ) {

            nWidth = sliderPlotWidth
            nHeight = sliderPlotWidth * diff(ggplotMapLatitudeRange_deg) / diff(ggplotMapLongitudeRange_deg)

         } else {

            nWidth = sliderPlotWidth * diff(ggplotMapLongitudeRange_deg) / diff(ggplotMapLatitudeRange_deg)
            nHeight = sliderPlotWidth

         }

      }

      lReactiveValues[['ggplotMapWidth']] = nWidth
      lReactiveValues[['ggplotMapHeight']] = nHeight

      lReactiveValues[['ggplotMap']]

   },
      width = function () { lReactiveValues[['ggplotMapWidth']] },
      height = function () { lReactiveValues[['ggplotMapHeight']] }
   )

   # Updating the plot as per user request
   observeEvent(
      input$actionButtonUpdatePlot,
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Updating plot\n'
            )
         )

         selectizeInputPlotScenario = input$selectizeInputPlotScenario
         dtLocations = isolate( lReactiveValues$dtLocations )
         dtOptimisedLocations = lReactiveValues$lOptimisationResult$lEditedResults$dtOptimisedLocations
         dtWays = lReactiveValues$dtWays
         ggMapTiles = lReactiveValues$ggMapTiles
         sliderLocationSize = input$sliderLocationSize
         sliderCoverageSize = input$sliderCoverageSize
         dtCoveredEdges = lReactiveValues$lOptimisationResult$lEditedResults$dtCoveredEdges
         dtOptimisationCoverageSummmary = lReactiveValues$lOptimisationResult$lEditedResults$dtOptimisationCoverageSummmary

         ggplotCity = fGetCityPlot(
            ggMapTiles = ggMapTiles,
            selectizeInputPlotScenario = selectizeInputPlotScenario,
            dtOptimisedLocations = dtOptimisedLocations,
            dtLocations = dtLocations,
            dtWays = dtWays,
            dtOptimisationCoverageSummmary = dtOptimisationCoverageSummmary,
            dtCoveredEdges = dtCoveredEdges,
            sliderLocationSize = sliderLocationSize,
            sliderCoverageSize = sliderCoverageSize
         )

         lReactiveValues[['ggplotMap']] = ggplotCity
         lReactiveValues[['ggplotMapLongitudeRange_deg']] = dtLocations[, range(Longitude_deg)]
         lReactiveValues[['ggplotMapLatitudeRange_deg']] = dtLocations[, range(Latitude_deg)]


      }
   )

   # zooming in on brush
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Implementing brush\n'
         )
      )


      ggplotMapBrush = input$ggplotMapBrush

      if ( is.null(ggplotMapBrush) ) {
         return ( NULL )
      }

      ggplotMap = isolate(lReactiveValues[['ggplotMap']])

      if ( is.null(ggplotMap) ) {
         return ( NULL )
      }

      lReactiveValues[['ggplotMap']] = ggplotMap + coord_cartesian(
         xlim = c(
            ggplotMapBrush$xmin,
            ggplotMapBrush$xmax
         ),
         ylim = c(
            ggplotMapBrush$ymin,
            ggplotMapBrush$ymax
         )
      )


      lReactiveValues[['ggplotMapLongitudeRange_deg']] = c(
         ggplotMapBrush$xmin,
         ggplotMapBrush$xmax
      )

      lReactiveValues[['ggplotMapLatitudeRange_deg']] = c(
         ggplotMapBrush$ymin,
         ggplotMapBrush$ymax
      )

   })


   # resetting the zoom on double click
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Implementing double click\n'
         )
      )

      if ( is.null(input$ggplotMapDblClick) ) {
         return ( NULL )
      }

      ggplotMap = isolate( lReactiveValues[['ggplotMap']] )

      if ( is.null(ggplotMap) ) {
         return ( NULL )
      }

      dtLocations = isolate( lReactiveValues$dtLocations )

      # Since coord_fixed causes problems, I  have to use coord_cartesian.
      # I tried to do this to try and get a square-ish plot but that was fail
      # This code is still here even though, I think, it doesn't do anything
      # useful anyway.
      # It gets the coordinates correctly but since the final image isn't a
      # square itself, it fails
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

         ggplotMap = ggplotMap +
            coord_cartesian(
               xlim = c(
                  vnLongitudeRange_deg[1] - 0.01,
                  vnLongitudeRange_deg[2] + 0.01
               ),
               ylim = c(
                  vnLatitudeRange_deg[1] - 0.01,
                  vnLatitudeRange_deg[2] + 0.01
               )
            )


      } else {

         ggplotMap = ggplotMap + coord_cartesian()

      }

      lReactiveValues[['ggplotMap']] = ggplotMap

      lReactiveValues[['ggplotMapLongitudeRange_deg']] = dtLocations[, range(Longitude_deg)]
      lReactiveValues[['ggplotMapLatitudeRange_deg']] = dtLocations[, range(Latitude_deg)]

   })


   # based on insertion or deletions in the check box, changing the checkboxes in the datatable
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Updating check boxes based on selectize box of action locations\n'
         )
      )

      selectizeInputToActionLocations = input$selectizeInputToActionLocations
      iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber)

      lReactiveValues$posixctLastLocationUpload
      dtLocations = isolate(lReactiveValues$dtLocations)
      if ( is.null(dtLocations) ) {
         return ( NULL )
      }

      isolate({

         for ( cSuffix in intersect(dtLocations[,LocationID], selectizeInputToActionLocations) ) {

            updateCheckboxInput(
               inputId = paste0('checkBoxActive', iTableRenderNumber, cSuffix),
               value = T,
               session = session
            )

         }

         for ( cSuffix in setdiff(dtLocations[,LocationID], selectizeInputToActionLocations) ) {

            updateCheckboxInput(
               inputId = paste0('checkBoxActive', iTableRenderNumber, cSuffix),
               value = F,
               session = session
            )

         }


      })

   })


   # checking / unchecking on the datatable should result in addition/deletion in the checkbox
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Adding checked items to selectize action locations  \n'
         )
      )

      # this can't be in isolate because the launch of the dashboard triggers this once
      # null, nothing happpens. and then the reactivity just ignores it since thet
      # checkbox inputs didn't register with the reactivity.

      lReactiveValues$posixctLastLocationUpload
      dtLocations = isolate(lReactiveValues$dtLocations)
      iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber)

      if ( is.null(dtLocations) ) {
         return ( NULL )
      }

      vbSelectedLocations = fRetrieveShinyValue(
         paste0('checkBoxActive', iTableRenderNumber),
         dtLocations[, LocationID],
         input
      )

      if ( any(is.null(vbSelectedLocations) ) | any(is.na(vbSelectedLocations) ) ) {
         return ( NULL )
      }

      dtLocationsActive = dtLocations[vbSelectedLocations]
      selectizeInputToActionLocations = dtLocationsActive[, LocationID]
      names(selectizeInputToActionLocations) = dtLocationsActive[, LocationName]

      isolate({updateSelectizeInput(
         session = session,
         inputId = 'selectizeInputToActionLocations',
         selected = selectizeInputToActionLocations
      )})

   })


   # translating click to an addition/deletion in  the checkbox
   # and also carrying over to the checkboxes i nthe datatable
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Adding location on click\n'
         )
      )

      ggplotMapClick = input$ggplotMapClick
      iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber)

      if ( is.null(ggplotMapClick)) {
         return ( NULL )
      }

      dtLocations = copy(isolate(lReactiveValues$dtLocations))
      selectizeInputToActionLocations = isolate(input$selectizeInputToActionLocations)

      if ( is.null(dtLocations) ) {
         return ( NULL )
      }

      # Identifying the nearest location to the click
      dtLocationsClick = dtLocations[
         fNearestLocation(
            nLongitude_deg = ggplotMapClick$x,
            nLatitude_deg = ggplotMapClick$y,
            nRadius_m = 1000,
            vnLongitude_deg = dtLocations$Longitude_deg,
            vnLatitude_deg = dtLocations$Latitude_deg
         )
      ]

      if ( nrow(dtLocationsClick) == 0 ) {
         return ( data.table() )
      }

      # Updating the selectize Box based on the various scenarios
      if ( is.null(selectizeInputToActionLocations) ) {

         selectizeInputToActionLocations = c(
            dtLocationsClick[, LocationID]
         )

         names(selectizeInputToActionLocations) = dtLocationsClick[, LocationName]

         isolate({updateCheckboxInput(
            session = session,
            inputId = paste0(paste0('checkBoxActive', iTableRenderNumber), unlist(dtLocationsClick[, LocationID])),
            value = T
         )})

      } else if ( dtLocationsClick[, LocationID] %in% selectizeInputToActionLocations ) {

         selectizeInputToActionLocations = setdiff(
            selectizeInputToActionLocations,
            dtLocationsClick[, LocationID]
         )

         isolate({updateCheckboxInput(
            session = session,
            inputId = paste0(paste0('checkBoxActive', iTableRenderNumber), unlist(dtLocationsClick[, LocationID])),
            value = F
         )})

      } else {

         selectizeInputToActionLocations = c(
            selectizeInputToActionLocations,
            dtLocationsClick[, LocationID]
         )

         names(selectizeInputToActionLocations) = c(
            names(selectizeInputToActionLocations),
            dtLocationsClick[, LocationName]
         )

         isolate({updateCheckboxInput(
            session = session,
            inputId = paste0(paste0('checkBoxActive', iTableRenderNumber), dtLocationsClick[, LocationID]),
            value = T
         )})


      }

      isolate({updateSelectizeInput(
         session = session,
         inputId = 'selectizeInputToActionLocations',
         selected = selectizeInputToActionLocations
      )})


   })

   # Optimisation
   observeEvent(
      input$actionButtonOptimise,
      {

         progress = Progress$new(session)
         on.exit(progress$close())

         # OSM data about ways and nodes
         progress$set(
            message = 'Optimising'
         )

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Optimising\n'
            )
         )

         radioOptimisationType = input$radioOptimisationType
         checkboxOptimiseMore = input$checkboxOptimiseMore
         sliderCoverageRadius = input$sliderCoverageRadius
         sliderCoverageScoreTradeoff = input$sliderCoverageScoreTradeoff
         sliderMaxStations = input$sliderMaxStations
         sliderMinScore = input$sliderMinScore
         sliderMinCoverage = input$sliderMinCoverage
         sliderMaxIterations = input$sliderMaxIterations
         sliderPopulation = input$sliderPopulation

         dtLocations = copy(lReactiveValues$dtLocations)

         if ( is.null(dtLocations) ) {

            showModal(modalDialog(
               title = "Error!",
               "No locations data uploaded!",
               easyClose = TRUE,
               footer = NULL
            ))

            return ( NULL )

         }

         setkey(dtLocations, LocationID)
         dtWays = lReactiveValues$dtWays
         dtPolygonCoveredEdges = lReactiveValues$dtPolygonCoveredEdges[DistanceFromDestination_m < sliderCoverageRadius]


         if ( is.null(dtPolygonCoveredEdges) ) {

            showModal(modalDialog(
               title = "Error!",
               "City data not loaded!",
               easyClose = TRUE,
               footer = NULL
            ))

            return ( NULL )

         }

         dtLocationsToIgnore = dtLocations[ !LocationID %in% dtPolygonCoveredEdges[, LocationID] ]

         # Checking for invalid max stations constraint
         if ( nrow(dtLocationsToIgnore) > 0 ) {

            showModal(modalDialog(
               title = "Warning!",
               paste(
                  "Coverage for some stations hasn't been calculated, forcing them as status = F:",
                  dtLocationsToIgnore[, paste0(LocationName, collapse = ', ' )]
               ),
               easyClose = TRUE,
               footer = NULL
            ))

         }

         dtLocations[ LocationID %in% dtLocationsToIgnore[, LocationID], Status := 'F' ]

         rm(dtLocationsToIgnore)


         # The stations that need to be decided
         iToDecide = dtLocations[, sum(Status == '?')]
         # The stations that are already chosen
         iChosen = dtLocations[, sum(Status == 'T')]

         # Trying to parallelise
         # sfInit(parallel = T, cpus = 1)
         # sfLibrary(data.table)
         # sfLibrary(GA)
         # sfExport('radioOptimisationType')
         # sfExport('numericTrials')
         # sfExport('sliderPopulation')
         # sfExport('iToDecide')
         # sfExport('sliderMaxStations')
         # sfExport('iChosen')
         # sfExport('sliderCoverageRadius')
         # sfExport('dtLocations')
         # sfExport('dtWays')
         # sfExport('dtPolygonCoveredEdges')
         # sfExport('sliderMaxIterations')
         # sfExport('sliderMinCoverage')
         # sfExport('fCIFitnessStations')
         # sfExport('fCIFitnessCoverageScore')

         # Creating empty objects to fill iterations in
         lGAResults = list()
         dtGASummary = data.frame()
         posixctStartTime = Sys.time()
         setkey(
            dtLocations,
            LocationID
         )

         # If the user wishes to resume from last run then load last
         # result as initial solution
         if ( !is.null(checkboxOptimiseMore) ) {

            if ( checkboxOptimiseMore == T )  {

               lOptimisationResult = lReactiveValues$lOptimisationResult
               gaResult = lOptimisationResult$lGAResults$Result
               dtGASummary = lOptimisationResult$lGAResults$dtGASummary
               dtOptimisedLocations = lOptimisationResult$lGAResults$dtOptimisedLocations
               lOptimisationResultStatistics = lOptimisationResult$lOptimisationResultStatistics
               rm(lOptimisationResult)

               setkey(
                  dtOptimisedLocations,
                  LocationID
               )

            }
         }

         if ( !exists('lOptimisationResultStatistics')) {
            lOptimisationResultStatistics = list()
         }

         # How many generation should the GA 'pause' to update the
         # progress message
         iUpdateAfterEvery = 5

         # So many iterations of iUpdateAfterEvery generations need to be run
         for ( i in 0:floor(sliderMaxIterations/iUpdateAfterEvery)) {

            # Poorly named variable. This is actually the number of
            # generations that the GA should run for
            iIterations = sliderMaxIterations - (i * iUpdateAfterEvery)

            if ( iIterations <= 0 ) {
               break
            }

            # GA can't propogate for only one generation so if remaining
            # generations after this iteration is only one then just
            # include that in this one
            if ( iIterations != (iUpdateAfterEvery+1) ) {
               iIterations = pmin(
                  iIterations,
                  iUpdateAfterEvery
               )
            }


            if ( radioOptimisationType == 'Maximise coverage' ) {

               # Checking for invalid max stations constraint
               if ( sliderMaxStations - iChosen < 0 ) {

                  showModal(modalDialog(
                     title = "Error!",
                     "Max stations allowed < stations already selected in status column",
                     easyClose = TRUE,
                     footer = NULL
                  ))

                  return ( NULL )

               }

               # Preparing initial solution for GA to start from
               if ( exists('gaResult') ) {

                  # Solution from previous iteration / previous run
                  if ( !is.null(gaResult@population) ) {

                     # todo - updating status from T to F or vv and resuming
                     # from previous optimisation causes it to crash.
                     mSuggestions = rbind(
                        gaResult@solution[1,],
                        gaResult@population[
                           sample(
                              nrow(gaResult@population),
                              min(nrow(gaResult@population), sliderPopulation) - 1
                           ),
                        ]
                     )

                     if ( exists('dtOptimisedLocations') ) {

                        mSuggestions = fUpdateSuggestionsForStatusChange(
                           dtLocations,
                           dtOptimisedLocations,
                           mSuggestions
                        )

                     }

                  }

               } else {

                  # randomly select combinations of the highest number of stations
                  mSuggestions = Reduce(
                     rbind,
                     lapply(
                        1:sliderPopulation,
                        function(x) {

                           viSuggestion = rep(0, iToDecide)
                           viFlip = sample(iToDecide, min(floor(runif(1) * (sliderMaxStations - iChosen)), iToDecide))
                           viSuggestion[viFlip] = 1
                           viSuggestion

                        }
                     )
                  )
               }

               gaResult <- ga(
                  type = "binary",
                  fitness = function(x) fCIFitnessCoverageScore(
                     viSelected = x,
                     nRadiusUpperLimit_m = sliderCoverageRadius,
                     iMaxStations = sliderMaxStations,
                     dtLocations = dtLocations,
                     dtWays = dtWays,
                     dtPolygonCoveredEdges = dtPolygonCoveredEdges
                  ),
                  suggestions = mSuggestions,
                  nBits = iToDecide,
                  popSize = sliderPopulation,
                  maxiter = iIterations,
                  mutation = function(x,y) fCustomMutation(
                     object = x,
                     parent = y,
                     dtLocations = dtLocations,
                     dtWays = dtWays,
                     dtPolygonCoveredEdges = dtPolygonCoveredEdges
                  ),
                  pmutation = function(x, y) fCustomPMutation(
                     object = x,
                     iUpdateAfterEvery = iUpdateAfterEvery,
                     i = i,
                     iMaxIterations = sliderMaxIterations
                  )
               )

               progress$set(
                  message = paste(
                     Sys.time(),
                     'After',
                     (iUpdateAfterEvery*i) + iIterations,
                     'iterations, coveragescore achieved = ',
                     pmax(0, round(gaResult@fitnessValue,2)),
                     '. Optimisation continues. Approximately',
                     ceiling((
                        (
                           sliderMaxIterations - (
                              iIterations + (i * iUpdateAfterEvery)
                           )
                        ) *
                        as.numeric(Sys.time() - posixctStartTime) / (iIterations + (i * iUpdateAfterEvery)))/60
                     ),
                     'minute(s) left'
                  )
               )

               dtGASummaryNew = data.frame(gaResult@summary)

            } else if ( radioOptimisationType == 'Minimise stations' ) {

               # Preparing initial solution for GA to start from
               if ( exists('gaResult') ) {

                  # Solution from previous iteration / previous run

                  if ( !is.null(gaResult@population) ) {

                     mSuggestions = rbind(
                        gaResult@solution[1,],
                        gaResult@population[
                           sample(
                              nrow(gaResult@population),
                              min(nrow(gaResult@population), sliderPopulation) - 1
                           ),
                        ]
                     )

                     if ( exists('dtOptimisedLocations') ) {

                        mSuggestions = fUpdateSuggestionsForStatusChange(
                           dtLocations,
                           dtOptimisedLocations,
                           mSuggestions
                        )

                     }

                  }

               } else {

                  # Taking random combinations of stations in proportion to coverage area constraint
                  mSuggestions = Reduce(
                     rbind,
                     lapply(
                        1:sliderPopulation,
                        function(x) {

                           viSuggestion = rep(0, iToDecide)
                           viFlip = sample(iToDecide, floor(iToDecide * min(1, (sliderMinCoverage/100) + runif(1))))
                           viSuggestion[viFlip] = 1
                           viSuggestion

                        }
                     )
                  )

               }

               # mSuggestions = matrix(rep(1, iToDecide), nrow = 1, ncol = iToDecide)

               gaResult <- ga(
                  type = "binary",
                  fitness = function(x) fCIFitnessStations(
                     viSelected = x,
                     nRadiusUpperLimit_m = sliderCoverageRadius,
                     nMinCoverage_pct = sliderMinCoverage,
                     dtLocations = dtLocations,
                     dtWays = dtWays,
                     dtPolygonCoveredEdges = dtPolygonCoveredEdges
                  ),
                  suggestions = mSuggestions,
                  nBits = iToDecide,
                  popSize = sliderPopulation,
                  maxiter = iIterations,
                  mutation = function(x,y) fCustomMutation(
                     object = x,
                     parent = y,
                     dtLocations = dtLocations,
                     dtWays = dtWays,
                     dtPolygonCoveredEdges = dtPolygonCoveredEdges
                  ),
                  keepBest = T,
                  pmutation = function(x, y) fCustomPMutation(
                     object = x,
                     iUpdateAfterEvery = iUpdateAfterEvery,
                     i = i,
                     iMaxIterations = sliderMaxIterations
                  )
               )

               # cat(sliderMaxIterations)
               # cat(iIterations)
               # cat(i)
               # cat(iUpdateAfterEvery)
               # cat(posixctStartTime)
               # cat(Sys.time())

               progress$set(
                  message = paste(
                     Sys.time(),
                     'After',
                     (iUpdateAfterEvery*i) + iIterations,
                     'iterations, number of stations selected = ',
                     pmax(0, round(1/gaResult@fitnessValue, 0)),
                     '. Optimisation continues. Approximately',
                     ceiling((
                        (
                           sliderMaxIterations - (
                              iIterations + (i * iUpdateAfterEvery)
                           )
                        ) *
                        as.numeric(Sys.time() - posixctStartTime) / (iIterations + (i * iUpdateAfterEvery)))/60
                     ),
                     'minutes left.'
                  )
               )

               dtGASummaryNew = data.frame(1/gaResult@summary)

            }

            # This thing will work but look weird if the sort
            # of optimisation is changed between runs
            dtGASummary = rbind.fill(
               dtGASummary,
               dtGASummaryNew
            )


            if ( length(lOptimisationResultStatistics) > 0 ) {

               vbStoredDetailElements = sapply(
                  lOptimisationResultStatistics,
                  function ( lOptimisationResultStatisticsIncremental ) {

                     is.null(lOptimisationResultStatisticsIncremental$dtCoveredEdges)

                  }
               )

               if ( sum(vbStoredDetailElements) > 1 ) {

                  for ( iElement in which(vbStoredDetailElements)[-1] ) {

                     lOptimisationResultStatistics[[iElement]]$dtCoveredEdges = NULL
                     lOptimisationResultStatistics[[iElement]]$dtOptimisationCoverageSummmary = NULL

                  }

               }

            }

            lOptimisationResultStatistics = append(
               lOptimisationResultStatistics,
               lapply(
                  seq(length(gaResult@bestSol)),
                  function ( iSolutionIndex ) {

                     vcRawSolution = gaResult@bestSol[[iSolutionIndex]]

                     lOptimisationResultStatisticsIncremental = fGetOptimisationResultStatistics(
                        dtPolygonCoveredEdges,
                        vcRawSolution,
                        dtLocations,
                        dtWays,
                        sliderCoverageRadius
                     )

                     if ( iSolutionIndex < length(gaResult@bestSol) ) {

                        lOptimisationResultStatisticsIncremental$dtCoveredEdges = NULL
                        lOptimisationResultStatisticsIncremental$dtOptimisationCoverageSummmary = NULL

                     }

                     return ( lOptimisationResultStatisticsIncremental )

                  }

               )
            )

         }

         setDT(dtGASummary)
         dtGASummary[, iter := .I]

         # gaResult = lGAResults[[which.max(sapply(lGAResults, function(x) x@fitness[1]))]]
         rm(lGAResults)

         if (

            gaResult@fitness[1] < 0 ) {

            showModal(modalDialog(
               title = "Error!",
               "FYI: the best solution achieved wasn't a valid solution. Try relaxing some of the constraints. If you like, you can restore the last solution from the button below.",
               easyClose = TRUE,
               footer = NULL
            ))

         }

         # needs the [1,] because sometime multiple solutions are there
         # means discarding the other solutions. Shouldn't matter.
         vcSolution = rep('F', length(gaResult@solution[1,]))
         vcSolution[gaResult@solution[1,] == 1] = 'T'

         # Updating locations table with the solution
         dtLocations[Status != '?', Chosen := Status]
         dtLocations[Status == '?', Chosen := vcSolution]
         dtLocations[, Chosen := as.character(Chosen)]
         # A lot of the other updating doesn't happen here but happens
         # when computing cResultSummary because that chunk will also
         # respond to recalculations whereas this chunk won't

         # Big bulky object with all optimisation results
         # The lEditedResults is where the recalculated versions
         # will be stored. The optimisation solution is epxensive
         # to compute which is why we don't overwrite it.
         lOptimisationResult = list(
            lGAResults = list(
               cOptimisationType = radioOptimisationType,
               Result = gaResult,
               nRadiusUpperLimit_m = sliderCoverageRadius,
               iMaxStations = sliderMaxStations,
               nMinCoverage_pct = sliderMinCoverage,
               dtOptimisedLocations = dtLocations,
               dtGASummary = dtGASummary
            ),
            lOptimisationResultStatistics = lOptimisationResultStatistics,
            lEditedResults = list(
               nRadiusUpperLimit_m = sliderCoverageRadius,
               dtOptimisedLocations = dtLocations
            )
         )

         lReactiveValues$lPreviousOptimisationResult = lReactiveValues$lOptimisationResult
         lReactiveValues$lOptimisationResult = lOptimisationResult

      }
   )

   # Preparing the text snippet that goes in the results
   # Almost all of the updates that follow an optimisation are
   # here because this also responds to recalculations which the
   # optimisation chunk does not.
   output$cResultSummary = renderUI({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Updating optimisation result to dtLocations\n'
         )
      )

      lOptimisationResult = lReactiveValues$lOptimisationResult
      lPreviousOptimisationResult = lReactiveValues$lPreviousOptimisationResult

      dtWays = lReactiveValues$dtWays

      if ( is.null(lOptimisationResult) )  {
         return ( NULL )
      }
      dtOptimisedLocations = lOptimisationResult$lEditedResults$dtOptimisedLocations
      dtPolygonCoveredEdges = isolate(lReactiveValues$dtPolygonCoveredEdges)
      lReactiveValues$dtLocations = copy(dtOptimisedLocations)
      iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber)

      if ( is.null(dtPolygonCoveredEdges) ) {
         return ( NULL )
      }

      # Updating the datatable's radio buttons based on results
      isolate({

         for ( cSuffix in dtOptimisedLocations[Chosen == 'F', LocationID] ) {

            updateRadioButtons(
               inputId = paste0('radioChosen', iTableRenderNumber, cSuffix),
               selected = F,
               session = session
            )

         }

         for ( cSuffix in dtOptimisedLocations[Chosen == 'T', LocationID] ) {

            updateRadioButtons(
               inputId = paste0('radioChosen', iTableRenderNumber, cSuffix),
               selected = T,
               session = session
            )

         }

      })

      # save(
      #    list = 'lOptimisationResultStatistics',
      #    file = '/tmp/lOptimisationResultStatistics.Rdata'
      # )

      cReturn = paste(
         '<br/>Results of latest optimisation / calculation: Objective value of <b>',
         round(lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$nCoverage_pct, 4),
         '</b> with <b>',
         lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$iStationsUsed,
         'stations</b>',
         # ' of average score <b>',
         # round(nMeanScore, 4),
         # '</b>',
         ''
      )

      dtIterationSummary = rbindlist(
         lapply(
            lOptimisationResult$lOptimisationResultStatistics,
            function(x) {
               data.table(StationsUsed = x$iStationsUsed, Coverage_pct = x$nCoverage_pct)
            }
         )
      )
      dtIterationSummary[,  Iteration := .I]

      isolate({lReactiveValues$lOptimisationResult$lEditedResults$dtOptimisationCoverageSummmary = lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$dtOptimisationCoverageSummmary})
      isolate({lReactiveValues$lOptimisationResult$lEditedResults$dtCoveredEdges = lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$dtCoveredEdges})
      isolate({lReactiveValues$lOptimisationResult$lEditedResults$iStationsUsed = lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$iStationsUsed})
      isolate({lReactiveValues$lOptimisationResult$lEditedResults$nCoverage_pct = lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$nCoverage_pct})
      isolate({lReactiveValues$lOptimisationResult$lEditedResults$nMeanScore = lOptimisationResult$lOptimisationResultStatistics[[length(lOptimisationResult$lOptimisationResultStatistics)]]$nMeanScore})
      isolate({lReactiveValues$lOptimisationResult$lEditedResults$dtIterationSummary = dtIterationSummary})

      # Appending results of previous optimisation if there exists one
      if ( !is.null(lPreviousOptimisationResult) ) {

         cReturn = paste(
            cReturn,
            '<br/>Results of previous optimisation / calculation: Objective value of <b>',
            round(lPreviousOptimisationResult$lEditedResults$nCoverage_pct, 4),
            '</b> with <b>',
            lPreviousOptimisationResult$lEditedResults$iStationsUsed,
            # 'stations</b> of average score <b>',
            # round(lPreviousOptimisationResult$lEditedResults$nMeanScore, 4),
            # '</b>',
            ''
         )

      }

      lReactiveValues[['ggplotMap']] = plotCityBlank
      lReactiveValues[['ggplotMapLatitudeRange_deg']] = NULL
      lReactiveValues[['ggplotMapLongitudeRange_deg']] = NULL

      return ( HTML(cReturn) )

   })

   # Discards current optimisation and loads the previous one
   observeEvent(
      input$actionButtonLoadPreviousOptimisaton,
      {

         lReactiveValues$lOptimisationResult = lReactiveValues$lPreviousOptimisationResult
         lReactiveValues$lPreviousOptimisationResult = NULL
         lReactiveValues[['ggplotMap']] = plotCityBlank
         lReactiveValues[['ggplotMapLatitudeRange_deg']] = NULL
         lReactiveValues[['ggplotMapLongitudeRange_deg']] = NULL

      }
   )


   # Checks for the existence of an optimisation solution and if so
   # then allows user the option to continue optimisation from that solution
   output[['uiOptimiseMore']] = renderUI({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Checkbox to resume from previous optimisation\n'
         )
      )

      # The other element, lEditedResults, gets updated on a recalc also
      # So then the object won't show up as null
      if ( is.null(lReactiveValues$lOptimisationResult$lGAResults)) {

         return (
            h6('')
         )

      } else {

         checkboxOptimiseMore = input$checkboxOptimiseMore
         if ( is.null(checkboxOptimiseMore) ) {

            checkboxInput(
               inputId = 'checkboxOptimiseMore',
               label = 'Resume from latest run',
               value = F
            )

         } else {

            checkboxInput(
               inputId = 'checkboxOptimiseMore',
               label = 'Resume from latest run',
               value = checkboxOptimiseMore
            )

         }

      }

   })



   # Depending on the existence of a previous optimisation result,
   # gives controls to the user to load it instead of current result
   output[['uiLoadPreviousOptimisationResult']] = renderUI({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Adding more optimisation option \n'
         )
      )

      if ( is.null(lReactiveValues$lPreviousOptimisationResult)) {

         return (
            h6('')
         )

      } else {

         actionButton(
            inputId = 'actionButtonLoadPreviousOptimisaton',
            label = 'Load prev result ( discard current )'
         )

      }

   })


   # Letting user save the edits to the chosen and status radio buttons
   observeEvent(
      input$actionButtonSave,
      {

         # status radio button being updated back to the dataset
         # should I just dynamically retrieve this when the dataset is needed?

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Saving changes \n'
            )
         )

         cat(
            file = stderr(),
            paste(
               'Updating status radio button change to dataset \n'
            )
         )

         # this can't be in isolate because the launch of the dashboard triggers this once
         # null, nothing happpens. and then the reactivity just ignores it since thet
         # checkbox inputs didn't register with the reactivity.

         dtLocations = isolate(lReactiveValues$dtLocations)
         iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber)

         if ( is.null(dtLocations) ) {
            return ( NULL )
         }

         vbStatus = fRetrieveShinyValue(
            paste0('radioStatus', iTableRenderNumber),
            unlist(dtLocations[, LocationID]),
            input
         )

         vcStatusFlip = paste(
            dtLocations[Status != vbStatus, LocationName],
            vbStatus[dtLocations[,Status] != vbStatus]
         )

         dtLocations[, Status := as.character(vbStatus)]

         # chosen radio button being updated back to the dataset
         # should I just dynamically retrieve this when the dataset is needed?

         cat(
            file = stderr(),
            paste(
               'Updating chosen radio button change to dataset\n'
            )
         )

         # this can't be in isolate because the launch of the dashboard triggers this once
         # null, nothing happpens. and then the reactivity just ignores it since thet
         # checkbox inputs didn't register with the reactivity.

         vbChosen = fRetrieveShinyValue(
            paste0('radioChosen', iTableRenderNumber),
            unlist(dtLocations[, LocationID]),
            input
         )
         vcChosenFlip = paste(
            dtLocations[Chosen != vbChosen, LocationName],
            vbChosen[dtLocations[,Chosen] != vbChosen]
         )

         dtLocations[, Chosen := as.character(vbChosen)]

         isolate({lReactiveValues$dtLocations = dtLocations})

         # Some acknowledgement messages
         cModalString = paste(
            paste(
               'Status changed for:',
               paste(
                  vcStatusFlip,
                  collapse = ', '
               )
            ),
            '<br/>',
            paste(
               'Chosen changed for:',
               paste(
                  vcChosenFlip,
                  collapse = ', '
               )
            ),
            '<br/>',
            'For debugging, table number:',
            lReactiveValues$iTableRenderNumber - 1
         )

         showModal(
            modalDialog(
               title = "Confirmation!",
               HTML(cModalString),
               easyClose = TRUE,
               footer = NULL
            )
         )

      }
   )

   # Recalculate coverage, etc based on the latest available status and chosen
   observeEvent(
      input$actionButtonRecalculate,
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Recalculating\n'
            )
         )

         sliderEditedCoverageRadius = input$sliderEditedCoverageRadius
         dtLocations = lReactiveValues$dtLocations

         # This should trigger the cResultSummary chunk which will do
         # all the coverage, etc. calculations
         lReactiveValues$lOptimisationResult = list(
            lGAResults = lReactiveValues$lOptimisationResult$lGAResults,
            lEditedResults = list(
               nRadiusUpperLimit_m = sliderEditedCoverageRadius,
               dtOptimisedLocations = dtLocations
            )
         )

      }
   )


   # Downloading a scenario - analogous to the locations.csv file
   output$downloadScenario = downloadHandler(
      filename = function() {

         lOptimisationResult = lReactiveValues$lOptimisationResult
         selectizeInputCities = input$selectizeInputCities

         paste0(
            "Scenario-",
            selectizeInputCities,'-',
            lOptimisationResult$lEditedResults$nRadiusUpperLimit_m,'-',
            # input$selectizeInputCities,
            # lReactiveValues$lOptimisationResult$lEditedResults$sliderEditedCoverageRadius,
            strftime(Sys.time(), '%Y%m%d%H%M'),
            ".csv"
         )

      },
      content = function(file) {

         dtOptimisedLocations = lReactiveValues$lOptimisationResult$lEditedResults$dtOptimisedLocations

         if ( !is.null(dtOptimisedLocations) ) {

            write.csv(
               dtOptimisedLocations,
               file,
               row.names = F,
               quote = T,
               na = ''
            )

         } else {

            showModal(
               modalDialog(
                  title = "Error!",
                  "At least one optimisation / recalculation should have happened to save anything.",
                  easyClose = TRUE,
                  footer = NULL
               )
            )

         }


      }
   )


   # Downloading an optimisation result - analogous to the lOptimisationResult
   output$downloadResult = downloadHandler(
      filename = function() {

         lOptimisationResult = lReactiveValues$lOptimisationResult
         selectizeInputCities = input$selectizeInputCities

         paste0(
            "Result-",
            selectizeInputCities,'-',
            lOptimisationResult$lEditedResults$nRadiusUpperLimit_m,'-',
            # input$selectizeInputCities,
            # lReactiveValues$lOptimisationResult$lEditedResults$sliderEditedCoverageRadius,
            strftime(Sys.time(), '%Y%m%d%H%M'),
            ".Rdata"
         )

      },
      content = function(file) {

         lOptimisationResult = lReactiveValues$lOptimisationResult

         if ( !is.null(lOptimisationResult) ) {

            save(
               list = 'lOptimisationResult',
               file = file
            )

         } else {

            showModal(
               modalDialog(
                  title = "Error!",
                  "At least one optimisation / recalculation should have happened to save",
                  easyClose = TRUE,
                  footer = NULL
               )
            )

         }

      }
   )



   # Uploading the result of an optimisation
   observeEvent(
      input$actionButtonUploadScenario,
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Uploading scenario data\n'
            )
         )

         progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Loading scenario data'
         )

         # reading in the file in
         inFile <- input$fileInputUploadResult

         if (is.null(inFile)) {
            return(NULL)
         }

         load(inFile$datapath)

         # Some basic preprocessing
         setkey(lOptimisationResult$lEditedResults$dtOptimisedLocations, LocationID)
         dtLocations = lOptimisationResult$lEditedResults$dtOptimisedLocations
         lReactiveValues$dtLocations = dtLocations
         lReactiveValues$posixctLastLocationUpload = Sys.time()

         lReactiveValues$lOptimisationResult = lOptimisationResult

         rm(lOptimisationResult)


         # adding locations to the actionable locations selection box
         vcLocations = unlist(dtLocations[, LocationID])
         names(vcLocations) = dtLocations[, LocationName]
         selectizeInputToActionLocations = input$selectizeInputToActionLocations

         updateSelectizeInput(
            session = session,
            inputId = 'selectizeInputToActionLocations',
            choices = vcLocations,
            selected = intersect(vcLocations, selectizeInputToActionLocations)
         )

         # checking if all loctions are present in the coverage data. Else need to pre-process
         dtPolygonCoveredEdges = lReactiveValues$dtPolygonCoveredEdges

         if ( !is.null(dtPolygonCoveredEdges) ) {

            progress$set(
               message = 'Checking for locations not in coverage data'
            )

            vcNewLocations = setdiff(
               dtLocations[, LocationID],
               dtPolygonCoveredEdges[, LocationID]
            )

            if ( length(vcNewLocations) > 0 ) {

               names(vcNewLocations) = dtLocations[
                  LocationID %in% vcNewLocations,
                  LocationName
               ]

            }

            updateSelectizeInput(
               session = session,
               inputId = 'selectizeInputNewLocations',
               choices = vcNewLocations,
               selected = vcNewLocations
            )

            progress$set(
               message = 'Calculating location coverage details'
            )

            lReactiveValues$dtCoverageSummary = fGenerateCoverageSummary(
               dtLocations = dtLocations,
               dtWays = lReactiveValues$dtWays,
               dtPolygonCoveredEdges = dtPolygonCoveredEdges,
               nMaxRadiusUserWillUse_m = nMaxRadiusUserWillUse_m
            )

         }


   })


   # Calculating the hover text on ggplot
   output[['htmlHoverText']] = renderUI({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Implementing hover text\n'
         )
      )

      # getting the hover action
      ggplotMapHover <- input[['ggplotMapHover']]
      ggplotMapLatitudeRange_deg = isolate(lReactiveValues$ggplotMapLatitudeRange_deg)


      # if there is no hover then return a null
      if ( is.null(ggplotMapHover) ) {

         return (NULL)

      }

      # if there is only a blank plot
      if ( any(is.null(ggplotMapLatitudeRange_deg)) ) {

         return (NULL)

      # else if there is hover then calculate the display
      }


      dtLocations = lReactiveValues$dtLocations


      if ( is.null(dtLocations) ) {
         return ( NULL )
      }

      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (ggplotMapHover$x - ggplotMapHover$domain$left) / (ggplotMapHover$domain$right - ggplotMapHover$domain$left)
      top_pct <- (ggplotMapHover$domain$top - ggplotMapHover$y) / (ggplotMapHover$domain$top - ggplotMapHover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- ggplotMapHover$range$left + left_pct * (ggplotMapHover$range$right - ggplotMapHover$range$left)
      top_px <- ggplotMapHover$range$top + top_pct * (ggplotMapHover$range$bottom - ggplotMapHover$range$top)

      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0(
         "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
         "left:",
         left_px + 2,
         "px; top:",
         top_px + 2,
         "px;"
      )

      dtLocationsClick = dtLocations[
         fNearestLocation(
            nLongitude_deg = ggplotMapHover$x,
            nLatitude_deg = ggplotMapHover$y,
            nRadius_m = 1000,
            vnLongitude_deg = dtLocations$Longitude_deg,
            vnLatitude_deg = dtLocations$Latitude_deg
         )
      ]

      if ( nrow(dtLocationsClick) == 0 ) {

         return (NULL)

      }


      # Sending this to reactive values for it to be reused in the output object
      return (
         wellPanel(
            style = style,
            p(HTML(
               dtLocationsClick[, LocationName]
            ))
         )
      )

   })

   # If there are new locations in the locations file which
   # haven't been processed yet then processing them.
   # The logic is very similar to what's in the intialising city script
   observeEvent(
      input$actionButtonProcessNewLocations,
      {

         progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Processing new locations'
         )

         selectizeInputNewLocations = input[['selectizeInputNewLocations']]
         dtLocations = lReactiveValues$dtLocations
         dtNodes = lReactiveValues$dtNodes
         dtWays = lReactiveValues$dtWays
         cCoverageFileName = paste0(
            cDataDirectory(),
            '/Analysis/OSMServiceable.Rdata'
         )

         # Getting locations data only for new location
         dtLocationsToBeProcessed = dtLocations[LocationID %in% selectizeInputNewLocations]


         progress$set(
            message = 'Calculating nearest map nodes to locations'
         )

         fUpdateCoverageData (
            igraphLinks = NULL,
            dtWays,
            cCoverageFileName,
            bOverwriteExistingCoverageCalculation,
            dtLocationsToBeProcessed
         )

         load(cCoverageFileName)

         setkey(dtPolygonCoveredEdges, LocationID)
         lReactiveValues$dtPolygonCoveredEdges = dtPolygonCoveredEdges

         progress$set(
            message = 'Generating location details'
         )

         # Updating the coverage summary
         lReactiveValues$dtCoverageSummary = fGenerateCoverageSummary(
            dtLocations = dtLocations,
            dtWays = dtWays,
            dtPolygonCoveredEdges = dtPolygonCoveredEdges,
            nMaxRadiusUserWillUse_m = nMaxRadiusUserWillUse_m
         )

      }
   )

   # Basic plot to show GA iteration details
   output[['plotConvergence']] = renderPlot({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Convergence plot\n'
         )
      )

      # dtGASummary = lReactiveValues$lOptimisationResult$lGAResults$dtGASummary
      dtIterationSummary = lReactiveValues$lOptimisationResult$lEditedResults$dtIterationSummary

      if ( is.null(dtIterationSummary) ) {
         return (
            ggplot() +
               geom_text(aes(x=0,y=0,label='No optimisations run yet')) +
               theme(
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  line = element_blank(),
                  title = element_blank(),
                  text = element_blank()
               )
         )
      }

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'There is sometimes a very long delay after this chunk. Timestamp
            to indicate this chunk ended and some other unidentified thing
            is running.\n'
         )
      )


      grid.arrange(
         ggplot(dtIterationSummary) + geom_line(aes(x = Iteration, y = Coverage_pct)),
         ggplot(dtIterationSummary) + geom_line(aes(x = Iteration, y = StationsUsed)),
         ncol = 1
      )

   })


   # Updating the max coverage possible with current radius
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Calculating max possible coverage\n'
         )
      )

      progress = Progress$new(session)
      on.exit(progress$close())

      # OSM data about ways and nodes
      progress$set(
         message = 'Calculating max possible coverage'
      )


      sliderCoverageRadius = input$sliderCoverageRadius
      dtWays = lReactiveValues$dtWays
      dtPolygonCoveredEdges = lReactiveValues$dtPolygonCoveredEdges

      if ( is.null(sliderCoverageRadius) ) {
         return ( NULL )
      }

      if ( is.null(dtWays) ) {
         return ( NULL )
      }

      if ( is.null(dtPolygonCoveredEdges) ) {
         return ( NULL )
      }

      dtPolygonCoveredEdges = merge(
         dtPolygonCoveredEdges,
         dtWays[, list(WayID, Distance_m)],
         'WayID'
      )


      dtPolygonCoveredEdges = dtPolygonCoveredEdges[
         DistanceFromDestination_m < sliderCoverageRadius
      ]

      # Roads covered by the stations
      dtCoverage = dtPolygonCoveredEdges[,
         list(
            1
         ),
         list(
            WayID,
            Distance_m
         )
      ]


      # Calculating coverage
      nCoverage_pct = dtCoverage[, sum(Distance_m)] / dtWays[, sum(Distance_m)]

      nMaxValue_pct = floor(
         100*nCoverage_pct
      ) / 100

      updateSliderInput(
         inputId = 'sliderMinCoverage',
         session = session,
         max = nMaxValue_pct,
         value = pmin(nMaxValue_pct, isolate(input$sliderMinCoverage))
      )

   })

   # This priority ensures the running order of the chunks is correct
   outputOptions(output, "cResultSummary", priority = 500)
   outputOptions(output, "datatableLocations", priority = 400)

































   output[['ggplotCorrectionsMap']] = renderPlot({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Plotting corrections map\n'
         )
      )

      lReactiveValues$ggplotCorrectionsMap

   })

   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Implementing brush on corrections map\n'
         )
      )

      progress = Progress$new(session)
      on.exit(progress$close())
      progress$set(
         message = 'Zooming in on corrections map'
      )

      ggplotCorrectionsMap = isolate(lReactiveValues$ggplotCorrectionsMap)
      ggplotCorrectionsMapBrush = input$ggplotCorrectionsMapBrush

      if ( is.null(ggplotCorrectionsMap) ) {

         return ( NULL )

      }

      if ( !is.null(ggplotCorrectionsMapBrush) ) {

         ggplotCorrectionsMap = ggplotCorrectionsMap +
         coord_cartesian(
            xlim = c(
               ggplotCorrectionsMapBrush$xmin,
               ggplotCorrectionsMapBrush$xmax
            ),
            ylim = c(
               ggplotCorrectionsMapBrush$ymin,
               ggplotCorrectionsMapBrush$ymax
            )
         )


      }

      lReactiveValues$ggplotCorrectionsMap = ggplotCorrectionsMap

   })



   # resetting the zoom on double click
   observe({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Implementing double click on corrections map\n'
         )
      )

      progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Resetting corrections map'
         )

      ggplotCorrectionsMapDblClick = input$ggplotCorrectionsMapDblClick

      if ( is.null(input$ggplotCorrectionsMapDblClick) ) {
         return ( NULL )
      }

      ggplotCorrectionsMap = isolate(lReactiveValues$ggplotCorrectionsMap)

      lReactiveValues$ggplotCorrectionsMap = ggplotCorrectionsMap +
         coord_cartesian()

   })

   observe({

      progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Making map for corrections tab'
         )

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Creating corrections map\n'
         )
      )

      ggMapTiles = lReactiveValues$ggMapTiles
      dtWays = lReactiveValues$dtWays
      dtNodes = lReactiveValues$dtNodes

      input$actionButtonResetCorrectionsMap

      if ( is.null(ggMapTiles) ) {

         return ( NULL )

      }

      ggplotCorrectionsMap = ggmap(ggMapTiles)  +
         geom_segment(
            data = dtWays,
            aes(
               x = Longitude_deg1,
               y = Latitude_deg1,
               xend = Longitude_deg2,
               yend = Latitude_deg2,
               color = OneWay
            ),
            size = 1
         ) +
         geom_point(
            data = dtNodes,
            aes(
               x = Longitude_deg,
               y = Latitude_deg
            ),
            size = 1,
            shape = 21,
            fill = 'green',
            color = 'black'
         ) +
         coord_cartesian()

      lReactiveValues$ggplotCorrectionsMap = ggplotCorrectionsMap

   })



   output[['textPathDetails']] = eventReactive(
      input$actionButtonDrawRoute,
      {

         progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Getting path details'
         )

         ggplotCorrectionsMap = isolate(lReactiveValues$ggplotCorrectionsMap)
         dtWays = isolate(lReactiveValues$dtWays)
         dtNodes = isolate(lReactiveValues$dtNodes)
         textInputFromCorrectionMap = isolate(input$textInputFromCorrectionMap)
         textInputToCorrectionMap = isolate(input$textInputToCorrectionMap)

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

         lPath = get.shortest.paths(
            graph = igraphLinks,
            # from = '423744834',
            # to = '736207519'
            from = textInputFromCorrectionMap,
            to = textInputToCorrectionMap
         )

         vcNodesOnPath = names(lPath$vpath[[1]])

         dtCorrectionMapPath = rbind(
            merge(
               data.table(SourceID = head(vcNodesOnPath, -1), DestinationID = tail(vcNodesOnPath, -1)),
               dtWays,
               c('SourceID','DestinationID')
            ),
            merge(
               data.table(DestinationID = head(vcNodesOnPath, -1), SourceID = tail(vcNodesOnPath, -1)),
               dtWays,
               c('SourceID','DestinationID')
            )
         )

         setDT(dtCorrectionMapPath)

         ggplotCorrectionsMap = ggplotCorrectionsMap  +
            geom_segment(
               data = dtCorrectionMapPath,
               aes(
                  x = Longitude_deg1,
                  y = Latitude_deg1,
                  xend = Longitude_deg2,
                  yend = Latitude_deg2
               ),
               size = 2,
               color = '#FF4500'
            ) +
            geom_point(
               data = dtNodes[NodeID %in% vcNodesOnPath],
               aes(
                  x = Longitude_deg,
                  y = Latitude_deg
               ),
               size = 2,
               shape = 21,
               fill = '#FF4500',
               color = 'black'
            )

         lReactiveValues$ggplotCorrectionsMap = ggplotCorrectionsMap

         # print(head(dtCorrectionMapPath))

         paste(
            'Distance between',
            head(vcNodesOnPath, 1),
            'and',
            tail(vcNodesOnPath, 1),
            ':',
            dtCorrectionMapPath[, round(sum(Distance_m) / 1000, 2)],
            'km'
         )

      }
   )


   output[['textClickedNode']] = renderText({

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'Name of node which had click\n'
         )
      )

      ggplotCorrectionsMapClick = input$ggplotCorrectionsMapClick
      iTableRenderNumber = isolate(lReactiveValues$iTableRenderNumber)
      dtNodes = lReactiveValues$dtNodes

      if ( is.null(ggplotCorrectionsMapClick)) {
         return ( NULL )
      }

      # Identifying the nearest location to the click
      cCorrectionMapLocationsClick = dtNodes[
         fNearestLocation(
            nLongitude_deg = ggplotCorrectionsMapClick$x,
            nLatitude_deg = ggplotCorrectionsMapClick$y,
            nRadius_m = 1000,
            vnLongitude_deg = dtNodes$Longitude_deg,
            vnLatitude_deg = dtNodes$Latitude_deg
         ),
         NodeID
      ]

      lReactiveValues$cCorrectionMapLocationsClick = cCorrectionMapLocationsClick

      paste(
         'NodeID:',
         cCorrectionMapLocationsClick
      )


   })


}


# config file will have city, radius,
