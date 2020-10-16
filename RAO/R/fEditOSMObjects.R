#' @import data.table
#' @export
fEditOSMObjects = function (
   dtWays,
   dtNodes,
   vcMapEdits
) {

   if ( length(vcMapEdits) > 0 ) {
         
      # example of vcMapEdits
      if ( cCityName == 'Bangalore' & F ) {

         # keep incrementing counter below. new nodes / paths are
         # assigned id = counter value - 1.
         # nodeid: -1
         # pathid: -1
         vcMapEdits = c(
            # '#',
            # '# Kengeri Main Road service road connection',
            'AddNode,[NodeID:-1;Latitude_deg:12.96854;Longitude_deg:77.51268]',
            'AddWay,[SourceID:-1;DestinationID:446892459;Type:service;OneWay:no;PathID:192051394]',
            'AddWay,[SourceID:-1;DestinationID:2026109092;Type:service;OneWay:no;PathID:192051394]',
            'RemoveWay,[SourceID:446892459;DestinationID:2026109092]',
            'AddWay,[SourceID:-1;DestinationID:429863906;Type:service;OneWay:no;PathID:-1]',
            # '#',
            # '# U-turn under Tin Factory flyover',
            'RemoveWay,[SourceID:2411859752,DestinationID:2411859784]',
            'RemoveWay,[SourceID:2411859784,DestinationID:2411859779]',
            'RemoveWay,[SourceID:2411859779,DestinationID:2411859775]',
            'RemoveWay,[SourceID:2411859775,DestinationID:2411859753]',
            'RemoveWay,[SourceID:2411859753,DestinationID:2411859790]',
            'RemoveWay,[SourceID:2411859790,DestinationID:2411859789]',
            'RemoveWay,[SourceID:2411859789,DestinationID:2411859774]',
            'AddWay,[SourceID:2411859752,DestinationID:2411859784;Type:primary_link;OneWay:no;PathID:232876409]',
            'AddWay,[SourceID:2411859784,DestinationID:2411859779;Type:primary_link;OneWay:no;PathID:232876409]',
            'AddWay,[SourceID:2411859779,DestinationID:2411859775;Type:primary_link;OneWay:no;PathID:232876409]',
            'AddWay,[SourceID:2411859775,DestinationID:2411859753;Type:primary_link;OneWay:no;PathID:232876409]',
            'AddWay,[SourceID:2411859753,DestinationID:2411859790;Type:primary_link;OneWay:no;PathID:232876409]',
            'AddWay,[SourceID:2411859790,DestinationID:2411859789;Type:primary_link;OneWay:no;PathID:232876409]',
            'AddWay,[SourceID:2411859789,DestinationID:2411859774;Type:primary_link;OneWay:no;PathID:232876409]',
            # '#',
            # '# Removing the barrier on Marathahalli crossing',
            'RemoveWay,[SourceID:4089226786,DestinationID:4017395224]',
            'RemoveWay,[SourceID:4089226786,DestinationID:4017395223]',
            '#'
         )

      }

      vcMapEdits = grep(
         x = vcMapEdits,
         pattern = '#',
         invert = T,
         value = T
      )
         
      dtNodeMapping = data.table() 

      for ( cMapEdit in vcMapEdits ) {   

         print(cMapEdit)
         
         vcMapEdit = unlist(strsplit(cMapEdit, ','))
         
         vcMapEditDetails = unlist(strsplit(
            unlist(
               strsplit(
                  gsub(
                     vcMapEdit[2],
                     pattern = '\\[|\\]',
                     replacement = ''
                  ),
                  ';'
               )
            ),
            ':'
         ))
         
         vcMapEditDetailNames = vcMapEditDetails[c(T,F)]
         vcMapEditDetails = vcMapEditDetails[c(F,T)]
         names(vcMapEditDetails) = vcMapEditDetailNames
         
         if ( vcMapEdit[1] == 'AddWay' ) {
            
            cSourceID = vcMapEditDetails[['SourceID']]
            if ( !cSourceID %in% dtNodes[, NodeID] ) {
               
               cSourceID = dtNodeMapping[cSourceID == PseudoNodeID, NodeID]
               
               if ( length(cSourceID) == 0 ) {
                  
                  stop('Can\'t find SourceID in Nodes. Stopping. Please rectify and rerun.')
                  
               }
               
            }
            
            cDestinationID = vcMapEditDetails[['DestinationID']]
            if ( !cDestinationID %in% dtNodes[, NodeID] ) {
               
               cDestinationID = dtNodeMapping[cDestinationID == PseudoNodeID, NodeID]
               
               if ( length(cDestinationID) == 0 ) {
                  
                  stop('Can\'t find DestinationID in Nodes. Stopping. Please rectify and rerun.')
                  
               }
               
            }
            
            dtWays = rbind(
               dtWays,
               data.table(
                  SourceID = cSourceID,
                  DestinationID = cDestinationID,
                  Latitude_deg1 = dtNodes[NodeID == cSourceID, Latitude_deg],
                  Longitude_deg1 = dtNodes[NodeID == cSourceID, Longitude_deg],
                  Latitude_deg2 = dtNodes[NodeID == cDestinationID, Latitude_deg],
                  Longitude_deg2 = dtNodes[NodeID == cDestinationID, Longitude_deg],
                  Type = vcMapEditDetails[['Type']],
                  OneWay = vcMapEditDetails[['OneWay']],
                  PathID = as.character(ifelse(
                     vcMapEditDetails[['PathID']] < 0,
                     dtWays[, max(as.integer64(PathID)) + 1],
                     vcMapEditDetails[['PathID']]
                  )),
                  WayID = as.character(dtWays[, max(as.integer64(WayID)) + 1]),
                  TravelTime_s = 0
               )[, 
                  Distance_m := sqrt(((Latitude_deg2 - Latitude_deg1)^2) + ((Longitude_deg2 - Longitude_deg1)^2)) * 40000 * 1000 / 360
               ]
            )
            
            setDT(dtWays)
            
         } else if ( vcMapEdit[1] == 'RemoveWay' ) {
            
            # Remove to and fro rows from dtWays
            # update nodeIDs in actions list
            
            iCurrentRows = nrow(dtWays)
            
            dtWays = dtWays[
               !(
                  SourceID %in% c(vcMapEditDetails[['DestinationID']], vcMapEditDetails[['SourceID']])
                  & DestinationID %in% c(vcMapEditDetails[['DestinationID']], vcMapEditDetails[['SourceID']])
               )
            ]
            
            if ( iCurrentRows == nrow(dtWays) ) {
            
               stop('No ways removed. Stopping. Please rectify and rerun.')
            
            }
            
         } else if ( vcMapEdit[1] == 'AddNode' ) {
            
            if ( vcMapEditDetails[['NodeID']] %in% dtNodes[, NodeID] ) {
               
               stop('Node already in the nodes data. Stopping. Please rectify and rerun.')
               
            } else {
               
               dtNodes = rbind(
                  dtNodes,
                  data.table(
                     NodeID = as.character(max(dtNodes[, as.integer64(NodeID)]) + 1),
                     Latitude_deg = as.numeric(vcMapEditDetails[['Latitude_deg']]),
                     Longitude_deg = as.numeric(vcMapEditDetails[['Longitude_deg']])
                  )
               )
               
               setDT(dtNodes)
               
               dtNodeMapping = rbind(
                  dtNodeMapping,
                  data.table(
                     NodeID = dtNodes[, as.character(max(as.integer64(NodeID)))], # this will already have the + 1 incorporated
                     PseudoNodeID = vcMapEditDetails[['NodeID']]
                  )
               )
               
               setDT(dtNodeMapping)
               
            }
            
         } else if ( vcMapEdit[1] == 'RemoveNode' ) {
            
            iCurrentRows = nrow(dtNodes)
            
            dtNodes = dtNodes[
               !(
                  NodeID == vcMapEditDetails[['NodeID']]
               )
            ]
            
            dtWays = dtWays[
               !(
                  SourceID == vcMapEditDetails[['NodeID']]
                  || DestinationID == vcMapEditDetails[['NodeID']]
               )
            ]
            
            if ( iCurrentRows == nrow(dtNodes) ) {
            
               stop('No nodes removed. Stopping. Please rectify and rerun.')
               
            }
            
         } else {
            
            stop('Unknown action. Stopping. Please rectify and rerun.')
            
         }

      }

   }

   return (
      list(
         dtWays = dtWays,
         dtNodes = dtNodes
      )
   )

}