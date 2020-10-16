# https://github.com/rstudio/DT/issues/93#issuecomment-111001538
# Function to add multiple UI elements from basic argument constructs
#' @export
fActOnShinyInput = function(
   cFunctionName,
   id,
   vcSuffixes,
   ...
) {
   
   # The generic function, in the last else condition, takes too long because
   # of the sapply. I use radioButtons and checkBoxes so overriding the sapply
   # with a vectorised implementation of those two. This also means that if
   # the shiny function itself changes in some way then this function may
   # break.
   # This is problematic though, because the usage of the  extra optional 
   # arguments passed will need to be made transparent so that the end user
   # knows how to specify it exactly
   # As a result, I'm going back to the original mode of working with the
   # the generic way of creating these shiny inputs.
   if ( F & cFunctionName == 'radioButtons' ) {

      vcReturnString = paste0(
         "<div id=\"",
         paste0(id, vcSuffixes),
         "\" class=\"form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline\">\n  <div class=\"shiny-options-group\">\n    <label class=\"radio-inline\">\n      <input type=\"radio\" name=\"",
         paste0(id, vcSuffixes),
         "\" value=\"T\"/>\n      <span>T</span>\n    </label>\n    <label class=\"radio-inline\">\n      <input type=\"radio\" name=\"",
         paste0(id, vcSuffixes),
         "\" value=\"F\"/>\n      <span>F</span>\n    </label>\n    <label class=\"radio-inline\">\n      <input type=\"radio\" name=\"",
         paste0(id, vcSuffixes),
         "\" value=\"?\"/>\n      <span>?</span>\n    </label>\n  </div>\n</div>"
      )

   } else if ( F & cFunctionName == 'checkboxInput' ) {


      vcReturnString = paste0(
         "<div class=\"form-group shiny-input-container\">\n  <div class=\"checkbox\">\n    <label>\n      <input id=\"",
         paste0(id, vcSuffixes),
         "\" type=\"checkbox\"/>\n      <span></span>\n    </label>\n  </div>\n</div>"
      )
      
   }
   
   FUN = get(cFunctionName)

   vcReturnString = sapply(
      seq(length(vcSuffixes)),
      function ( i ) {
         as.character(
            FUN(
               inputId = paste0(id, vcSuffixes[i]),
               ...
            )
         )
      }
   )

   vcReturnString

}