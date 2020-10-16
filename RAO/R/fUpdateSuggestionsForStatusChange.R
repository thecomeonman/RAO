#' @import data.table
#' @export
fUpdateSuggestionsForStatusChange = function(
   dtLocations,
   dtOptimisedLocations,
   mSuggestions
) {

   if ( F ) {
      
      dtInsertSuggestions = data.table(
         Index = as.integer(setdiff(
            dtLocations[, which(Status == '?')],
            dtOptimisedLocations[, which(Status == '?')]
         ))
      )

      if ( nrow(dtInsertSuggestions) > 0 ) {

         dtInsertSuggestions[, Action := 'Insert']

      }
      
      dtRemoveSuggestions = data.table(
         Index = as.integer(setdiff(
            dtOptimisedLocations[, which(Status == '?')],
            dtLocations[, which(Status == '?')]
         ))
      )

      if ( nrow(dtRemoveSuggestions) > 0 ) {

         dtRemoveSuggestions[, Action := 'Remove']

      }

      dtEditSuggestions = rbind(
         dtInsertSuggestions,
         dtRemoveSuggestions,
         fill = T
      )

      print(dtEditSuggestions)

      if ( nrow(dtEditSuggestions) > 0 ) {

         rm(dtInsertSuggestions)
         rm(dtRemoveSuggestions)
         dtEditSuggestions = dtEditSuggestions[order(Index)]

         for ( iRow in seq(nrow(dtEditSuggestions)) ) {

            if ( dtEditSuggestions[iRow, Action == 'Insert'] ) {

               mSuggestions = cbind(
                  mSuggestions[, 1:dtEditSuggestions[iRow, Index - 1]],
                  rep(0, nrow(mSuggestions)),
                  mSuggestions[, dtEditSuggestions[iRow, Index]:ncol(mSuggestions)]
               )

               if ( iRow < nrow(dtEditSuggestions) ) {

                  dtEditSuggestions[
                     (iRow + 1):nrow(dtEditSuggestions),
                     Index := Index + 1L
                  ]

               }

            }

            if ( dtEditSuggestions[iRow, Action == 'Remove'] ) {

               mSuggestions = cbind(
                  mSuggestions[, 1:dtEditSuggestions[iRow, Index - 1]],
                  mSuggestions[, dtEditSuggestions[iRow, Index + 1]:ncol(mSuggestions)]
               )

               if ( iRow < nrow(dtEditSuggestions) ) {

                  dtEditSuggestions[
                     (iRow + 1):nrow(dtEditSuggestions),
                     Index := Index - 1L
                  ]

               }

            }

         }

      }


   }

   viSuggestionsToFix = which(
      dtLocations[, Status] != dtOptimisedLocations[, Status]
   )

   viSuggestionsToFix = sort(viSuggestionsToFix)

   for ( iSuggestionToFix in viSuggestionsToFix ) {

      print(dtLocations[iSuggestionToFix])
      print(dtOptimisedLocations[iSuggestionToFix])
      dim(mSuggestions)

      if ( 
         dtLocations[iSuggestionToFix, Status] == '?' &
         dtOptimisedLocations[iSuggestionToFix, Status] %in% c('F', 'T')
      ) {

         viSuggestionsToFix[viSuggestionsToFix > iSuggestionToFix] = 
         viSuggestionsToFix[viSuggestionsToFix > iSuggestionToFix] + 1

         mSuggestions = cbind(
            mSuggestions[, 1:(iSuggestionToFix - 1)],
            rep(1, nrow(mSuggestions)),
            mSuggestions[, iSuggestionToFix:ncol(mSuggestions)]
         )

      }

      if ( 
         dtLocations[iSuggestionToFix, Status] %in% c('F', 'T') &
         dtOptimisedLocations[iSuggestionToFix, Status] == '?'
      ) {

         viSuggestionsToFix[viSuggestionsToFix > iSuggestionToFix] = 
         viSuggestionsToFix[viSuggestionsToFix > iSuggestionToFix] - 1

         if ( iSuggestionToFix < ncol(mSuggestions) ) {

            mSuggestions = cbind(
               mSuggestions[, 1:(iSuggestionToFix - 1)],
               mSuggestions[, (iSuggestionToFix + 1):ncol(mSuggestions)]
            )

         } else {

            mSuggestions = mSuggestions[, 1:(iSuggestionToFix - 1)]

         }

      }

      dim(mSuggestions)

   }
   
   return ( mSuggestions )

}