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










# Setting up input data
# ==============================================================================

load(
   file = paste0(cAtherDataLocation, '/Processed/RAO/',cCityName,'/OSMServiceable.Rdata')
)

dtLocations = fread(paste0(cAtherDataLocation, '/Raw/RAO/',cCityName,'/Locations.csv'))
dtLocations = dtLocations[!LocationID %in% vcStationsToExclude]
















# Calculating the coverage area of each location
# ==============================================================================

# load(paste0('./Processed/RAO/',cCityName,'/Analysis/WorstTrafficOSMServiceable.Rdata'))
dtNodes = dtNodes[NodeID %in% dtWays[, SourceID] | NodeID %in% dtWays[, DestinationID]]
dtLocations[, LocationID := as.character(LocationID)]


selectizeInputNewLocations = dtLocations[, LocationID]

dtLocationsToBeProcessed = dtLocations[LocationID %in% selectizeInputNewLocations]

fUpdateCoverageData (
   igraphLinks = NULL,
   dtWays,
   cCoverageFileName,
   bOverwriteExistingCoverageCalculation,
   dtLocationsToBeProcessed,
   nMaxVertexDistanceFromPoint_m = nMaxVertexDistanceFromPoint_m,
   nDistanceCutoff_m = nDistanceCutoff_m
)