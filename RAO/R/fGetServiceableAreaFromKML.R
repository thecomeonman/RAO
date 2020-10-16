#' @import XML
#' @import data.table
#' @export
fGetServiceableAreaFromKML = function (
   cRefKMLFilePath
) {

   # Parsing the KML file and getting the coordinates we want data within.
   kml = xmlTreeParse(cRefKMLFilePath)
   kml = xmlRoot(kml)

   dtBoundCoordinates = rbindlist(
      lapply(
         strsplit(
            xmlValue(xmlElementsByTagName(kml,"coordinates",recursive = T)[[1]]),
            split = ' '
         )[[1]],
         function(x){

            dtBoundCoordinates = data.table()

            if ( nchar(x) > 0 ) {

               loc = as.numeric(strsplit(x,split = ",")[[1]])[1:2]
               dtBoundCoordinates = data.table(Latitude_deg = loc[2], Longitude_deg =  loc[1])

            }

            dtBoundCoordinates


         }
      ),
      fill = T
   )

   dtBoundCoordinates

}