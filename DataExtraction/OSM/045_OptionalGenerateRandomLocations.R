rm(list = ls())

library(ggmap)
library(data.table)
library(igraph)
library(bit64)
library(mgcv)
library(ggplot2)
library(RAO)
theme_set(theme_bw(12))

cAtherGitRepo = Sys.getenv('AtherGitRepo')
source(
    paste0(
        cAtherGitRepo,
        '/RAO/DataExtraction/OSM/000_Parameters.R'
    )
)


# ==============================================================================

load(cOSMServiceableAreaFile)

dtLocations = dtNodes[sample(nrow(dtNodes), iRandomStationsToGenerate)]
setnames(
   dtLocations,
   'NodeID',
   'LocationID'
)

dtLocations[, LocationName := as.character(LocationID)]

# Status column described in the readme
dtLocations[, Status := '?']
dtLocations[, Score := 0]

write.csv(
   dtLocations,
   cLocationsFile,
   row.names = F,
   quote = F,
   na = ''
)