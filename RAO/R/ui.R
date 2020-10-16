#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import gridExtra
#' @import ggmap
#' @import data.table
#' @import plyr
#' @import bit64
#' @import DT
#' @import GA
#' @import snow
#' @import snowfall
#' @import rlecuyer
#' @import igraph
fDashboardUI = function() {
   
   header <- dashboardHeader(
      title = "Pikachu"
   )

   body <- dashboardBody(
      tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
            Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
         })")),
      # verbatimTextOutput('debugging'),
      fluidRow(
         tabBox(
            width = 12,
            height = NULL,
            selected = "tabAppConfig",
            tabPanel(
               value = "tabAppConfig",
               title = "App Config",
               fluidRow(
                  width = 12,
                  column(
                     width = 12,
                     fluidRow(
                        width = 12,
                        column(
                           width = 12,
                           # selectizeInput(
                           #    inputId = 'selectizeInputCities',
                           #    label = 'Pick City',
                           #    choices = c('Bangalore','Chennai','Pune','Hyderabad'),
                           #    selected = NULL,
                           #    multiple = F
                           # ),
                           htmlOutput("selectizeInputCities"),
                           actionButton(
                              inputId = 'actionButtonLoadCityData',
                              label = 'Load City Data!'
                           )
                        )
                     ),
                     tags$hr(),
                     fluidRow(
                        width = 12,
                        column(
                           width = 6,
                           fileInput(
                              inputId = 'fileInputUploadLocations',
                              label = 'Upload Scenario!'
                           ),
                           actionButton(
                              inputId = 'actionButtonUploadLocations',
                              label = 'Read!'
                           )
                        ),
                        column(
                           width = 6,
                           fileInput(
                              inputId = 'fileInputUploadResult',
                              label = 'Upload Previous Result!'
                           ),
                           actionButton(
                              inputId = 'actionButtonUploadScenario',
                              label = 'Read!'
                           )
                        )
                     ),
                     tags$hr(),
                     fluidRow(
                        width = 12,
                        column(
                           width = 12,
                           selectizeInput(
                              inputId = 'selectizeInputNewLocations',
                              label = 'New Locations Detected',
                              choices = c(),
                              selected = NULL,
                              multiple = T,
                              width = '100%'
                           ),
                           actionButton(
                              inputId = 'actionButtonProcessNewLocations',
                              label = 'Process New Locations!'
                           )
                        )
                     )
                  )
               )
            ),
            tabPanel(
               value = "tabAnalysis",
               title = "Analysis",
               fluidRow(
                  width = 12,
                  column(
                     width = 2,
                     box(
                        title = 'Actions',
                        collapsible = T,
                        width = 12,
                        solidHeader = T,
                        status = 'primary',
                        selectizeInput(
                           inputId = 'selectizeInputToActionLocations',
                           label = 'Locations To Action',
                           choices = c(),
                           selected = NULL,
                           multiple = T
                        # ),
                        # actionButton(
                        #    inputId = 'actionButtonResetToOptimal',
                        #    label = 'Select Only Optimal!'
                        ),
                        actionButton(
                           'actionButtonDoesNothing',
                           'Does Nothing. Add.'
                        )
                     )
                  ),
                  column(
                     width = 10,
                     box(
                        title = 'Optimisation',
                        collapsible = T,
                        width = 12,
                        solidHeader = T,
                        status = 'primary',
                        fluidRow(
                           width = 12,
                           column(
                              width = 3,
                              radioButtons(
                                 inputId = 'radioOptimisationType',
                                 label = 'Optimisation Type',
                                 choices = c('Minimise stations','Maximise coverage'),
                                 selected =  'Minimise stations'
                              )
                           ),
                           column(
                              width = 3,
                              conditionalPanel(
                                 condition = "input.radioOptimisationType == 'Minimise stations'",
                                 # sliderInput(
                                 numericInput(
                                    inputId = 'sliderMinCoverage',
                                    label = 'Minimum coverage expected',
                                    min = 0,
                                    max = 1,
                                    value = 0.8,
                                    step = 0.01
                                 )
                                 # sliderInput(
                                 #    inputId = 'sliderMinScore',
                                 #    label = 'Minimum score expected',
                                 #    min = 0,
                                 #    max = 10,
                                 #    value = 7.5
                                 # )
                              ),
                              conditionalPanel(
                                 condition = "input.radioOptimisationType == 'Maximise coverage'",
                                 numericInput(
                                    inputId = 'sliderMaxStations',
                                    label = 'Maximum stations possible',
                                    min = 0,
                                    max = Inf,
                                    value = 75
                                 )
                              ),
                              numericInput(
                                 inputId = 'sliderCoverageRadius',
                                 label = 'Coverage Radius',
                                 min = 0,
                                 max = 6000,
                                 value = 6000,
                                 step = 100
                              )
                           ),
                           # column(
                              # width = 4,
                              # sliderInput(
                              
                              # sliderInput(
                              #    inputId = 'sliderCoverageScoreTradeoff',
                              #    label = 'Coverage / Score trade off',
                              #    min = 0,
                              #    max = 10,
                              #    value = 5
                              # )
                              # minimise stations / maximise coverage
                              # adjust coverage radius
                              # nMinStations nMaxStations
                              # Coverage vs. score (lambda)
                              # reoptimise
                              #
                              # recalculate coverage post edits
                                 # currently chosen stations, new radius, new lambda
                           # ),
                           column(
                              width = 3,
                              # h5('Advanced settings'),
                              # numericInput(
                              #    inputId = 'numericTrials',
                              #    label = 'Number of trials',
                              #    min = 0,
                              #    max = Inf,
                              #    value = 1,
                              #    step = 1
                              # ),
                              sliderInput(
                                 inputId = 'sliderMaxIterations',
                                 label = 'Maximum iterations',
                                 min = 2,
                                 max = 200,
                                 value = 20
                              ),
                              sliderInput(
                                 inputId = 'sliderPopulation',
                                 label = 'Population',
                                 min = 2,
                                 max = 200,
                                 value = 25
                              )
                           ),
                           column(
                              width = 3,
                              actionButton(
                                 inputId = 'actionButtonOptimise',
                                 label = 'Optimise!'
                              ),
                              uiOutput('uiOptimiseMore'),
                              uiOutput('uiLoadPreviousOptimisationResult')
                           )
                        ),
                        fluidRow(
                           width = 12,
                           column(
                              width = 12,
                              htmlOutput('cResultSummary'),
                              plotOutput('plotConvergence')
                           )
                        )
                     )
                  )
               ),
               fluidRow(
                  width = 12,
                  box(
                     title = 'Results',
                     collapsible = T,
                     width = 12,
                     solidHeader = T,
                     status = 'primary',
                     height = NULL,
                     fluidRow(
                        width = 12,
                        column(
                           width = 4,
                           selectizeInput(
                              inputId = 'selectizeInputPlotScenario',
                              label = 'Plot Scenario',
                              choices = c('Only city', 'Locations by Status', 'Locations by Chosen','Chosen locations and coverage','Chosen locations and redundancy'),
                              selected = c('Locations by Status'),
                              multiple = F
                           )
                        ),
                        column(
                           width = 2,
                           actionButton(
                              inputId = 'actionButtonUpdatePlot',
                              label = 'Update Plot!'
                           )
                        ),
                        column(
                           width = 2,
                           sliderInput(
                              inputId = 'sliderPlotWidth',
                              label = 'Max Plot Width',
                              min = 100,
                              max = 2400,
                              value = 600,
                              step = 100
                           )
                        ),
                        column(
                           width = 2,
                           sliderInput(
                              inputId = 'sliderLocationSize',
                              label = 'Location Marker Size',
                              min = 0,
                              max = 5,
                              value = 3,
                              step = 0.1
                           )
                        ),
                        column(
                           width = 2,
                           sliderInput(
                              inputId = 'sliderCoverageSize',
                              label = 'Coverage Marker Size',
                              min = 0,
                              max = 5,
                              value = 1,
                              step = 0.1
                           )
                        )
                     ),
                     div(
                        style = "position:relative",
                        plotOutput(
                           outputId = 'ggplotMap',
                           # width = '100%',
                           # width = 600,
                           # height = 1000,
                           width = 'auto',
                           height = 'auto',
                           click = "ggplotMapClick",
                           dblclick = "ggplotMapDblClick",
                           hover = hoverOpts(id = "ggplotMapHover", delayType = 'debounce'),
                           brush = brushOpts(id = "ggplotMapBrush", resetOnNew = T, delayType = 'debounce')
                        ),
                        uiOutput('htmlHoverText')
                     ),
                     tags$hr(),
                     fluidRow(
                        width = 12,
                        column(
                           width = 12,
                     # wellPanel(
                        # id = "tPanel",
                        # style = "overflow-x:scroll;",
                     #    max-height: 500px",
                           dataTableOutput('datatableLocations')
                        )
                     ),
                     tags$hr(),
                     fluidRow(
                        column(
                           width = 3,
                           downloadButton(
                              outputId = 'downloadScenario',
                              label = "Download CSV"
                           ),
                           downloadButton(
                              outputId = 'downloadResult',
                              label = "Download result"
                           )
                        ),
                        column(
                           width = 3,
                           # sliderInput(
                           #    inputId = 'sliderEditedCoverageRadius',
                           #    label = 'Edit coverage radius',
                           #    min = 1,
                           #    # max = 10000,
                           #    max = 6000,
                           #    value = 6000
                           # )
                           numericInput(
                              inputId = 'sliderEditedCoverageRadius',
                              label = 'Coverage Radius for Recalc',
                              min = 0,
                              max = 6000,
                              value = 6000,
                              step = 100
                           )
                        ),
                        # column(
                        #    width = 3,
                        #    sliderInput(
                        #       inputId = 'sliderCoverageScoreTradeoff',
                        #       label = 'Coverage / Score trade off',
                        #       min = 0,
                        #       max = 10,
                        #       value = 5
                        #    )
                        # ),
                        column(
                           width = 3,
                           actionButton(
                              inputId = 'actionButtonSave',
                              label = 'Save changes!'
                           )
                        ),
                        column(
                           width = 3,
                           actionButton(
                              inputId = 'actionButtonRecalculate',
                              label = 'Recalculate (not optimisation)!'
                           )
                        )
                     )
                  )
               )
            ),
            tabPanel(
               value = "tabMapCorrections",
               title = "Map Corrections",
               fluidRow(
                  column(
                     width = 3,
                     textOutput('textClickedNode')
                  ),
                  column(
                     width = 9,
                     fluidRow(
                        width = 12,
                        column(
                           width = 6,
                           textInput(
                              inputId = 'textInputFromCorrectionMap',
                              label = 'From',
                              placeholder = 'Node ID'
                           )
                        ),
                        column(
                           width = 6,
                           textInput(
                              inputId = 'textInputToCorrectionMap',
                              label = 'To',
                              placeholder = 'Node ID'
                           )
                        )
                     ),
                     fluidRow(
                        width = 12,
                        column(
                           width = 3,
                           actionButton(
                              inputId = 'actionButtonDrawRoute',
                              label = 'Draw route'
                           )
                        ),
                        column(
                           width = 6,
                           textOutput('textPathDetails')
                        ),
                        column(
                           width = 3,
                           actionButton(
                              inputId = 'actionButtonResetCorrectionsMap',
                              label = 'Reset'
                           )
                        )
                     )
                  )
               ),
               fluidRow(
                  width = 12,
                  column(
                     width = 12,
                     plotOutput(
                        outputId = 'ggplotCorrectionsMap',
                        # width = '100%',
                        # width = 600,
                        height = 1000,
                        dblclick = "ggplotCorrectionsMapDblClick",
                        click = "ggplotCorrectionsMapClick",
                        brush = brushOpts(id = "ggplotCorrectionsMapBrush", resetOnNew = T, delayType = 'debounce')
                     )
                  )
               )


            )

         )
      )

   )

   dashboardPage(
      header,
      dashboardSidebar(disable = TRUE),
      body
   )

}