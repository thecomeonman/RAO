# https://github.com/rstudio/DT/issues/93#issuecomment-111001538
# Function to retrieve multiple UI elements values from basic argument constructs
#' @import shiny
#' @export
fRetrieveShinyValue = function(
   id,
   vcSuffixes,
   input
) {

   unlist(
      lapply(
         seq(length(vcSuffixes)),
         function( i ) {

            value = input[[
               paste0(id, vcSuffixes[i])
            ]]

            if (is.null(value)) NA else value
         }
      )
   )

}