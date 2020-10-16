
launch_app2 = function() {

   cat(
      file = stderr(),
      paste(
         Sys.time(),
         'Launching app'
      )
   )

   library(shiny)
   library(shinydashboard)
   library(ggplot2)
   library(gridExtra)
   library(ggmap)
   library(data.table)
   library(plyr)
   library(bit64)
   library(DT)
   library(GA)
   library(snow)
   library(snowfall)
   library(rlecuyer)
   library(igraph)
   library(Pikachu)

   theme_set(theme_bw(12))

   # app options
   # ------------------------------------------------------------------------------

   # preventing scientific notation when printing numbers
   options(scipen=50)

   # allows users to upload files of up to 100MB
   # the optimisation result could reach that size if it's a large city with
   # lots of locations
   options(shiny.maxRequestSize=100*1024^2)

   # Directory locations for app, etc.
   # ------------------------------------------------------------------------------

   cRootDirectory <- getShinyOption("cRootDirectory", Sys.getenv('AtherGitRepo'))
   cAtherDataLocation = getShinyOption("cAtherDataLocation", Sys.getenv('AtherDataLocation'))
   cRootDirectory = if ( cRootDirectory == '' ) {
      '/mnt/disks/vi-data/AtherGit'
   } else {
      cRootDirectory
   }

   cAtherDataLocation = if ( cAtherDataLocation == '' ) {
      '/mnt/disks/vi-data/Data'
   } else {
      cAtherDataLocation
   }

   # Other input parametrs
   # ------------------------------------------------------------------------------

   nEarthCircumference_m = getShinyOption('nEarthCircumference_m', 40000000)

   nMaxRadiusUserWillUse_m =  getShinyOption('nMaxRadiusUserWillUse_m', 6000)


   shinyApp(
      ui = fDashboardUI,
      server = fDashboardServer
   )

}