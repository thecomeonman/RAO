#' @import shiny
#' @export
launch_app = function(
   cRootDirectory = '/mnt/disks/vi-data/AtherGit',
   cAtherDataLocation = '/mnt/disks/vi-data/Data',
   nEarthCircumference_m = 40000000,
   nMaxRadiusUserWillUse_m = 6000
){
  shinyOptions(cRootDirectory = cRootDirectory)
  shinyOptions(cAtherDataLocation = cAtherDataLocation)
  shinyOptions(nEarthCircumference_m = nEarthCircumference_m)
  shinyOptions(nMaxRadiusUserWillUse_m = nMaxRadiusUserWillUse_m)
  # source(system.file("app.R", package = "Pikachu", local = TRUE, chdir = TRUE))$value


   shinyApp(
      ui = fDashboardUI,
      server = fDashboardServer
   )

}
