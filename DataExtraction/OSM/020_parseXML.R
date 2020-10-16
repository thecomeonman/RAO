rm(list = ls())

library(data.table)
library(bit64)

cAtherGitRepo = Sys.getenv('AtherGitRepo')
source(
    paste0(
        cAtherGitRepo,
        '/RAO/DataExtraction/OSM/000_Parameters.R'
    )
)




# Setting up input data
# ==============================================================================

result <- readLines(cMapXMLFilePath)


# Extractin and writing out output data
# ==============================================================================

dir.create(
   paste0(
      cAtherDataLocation,
      '/Raw/RAO/',
      cCityName,
      '/OSMData/'
   ),
   showWarnings = F,
   recursive = T
)


vcNodes = grep(result, pattern = 'node id', value = T)

dtNodes = data.table(
   NodeID = gsub(x = vcNodes, pattern = '.*id="(.*?)".*', replacement = '\\1'),
   Latitude_deg = gsub(x = vcNodes, pattern = '.*lat="(.*?)".*', replacement = '\\1'),
   Longitude_deg = gsub(x = vcNodes, pattern = '.*lon="(.*?)".*', replacement = '\\1')
)

rm(vcNodes)

write.csv(
   dtNodes,
   file = paste0(
      cAtherDataLocation,
      '/Raw/RAO/',
      cCityName,
      '/OSMData/nodedetails.txt'
   ),
   quote = F,
   na = '',
   row.names = F
)



dtWays = data.table(
   Index1 = grep(result, pattern = '<way'),
   Index2 = grep(result, pattern = '</way')
)

dtWays[, PathID := gsub(x = result[Index1], pattern = '.*"(.*?)".*', replacement = '\\1')]

dtWays = dtWays[,
   list(Index = seq(Index1 + 1, Index2, 1)),
   PathID
]

dtWays[, Entry := result[Index]]
dtWays[grepl(x = Entry, pattern = 'nd ref'), NodeID := gsub(x = Entry, pattern = '.*"(.*?)".*', replacement = '\\1')]
dtWays[grepl(x = Entry, pattern = 'oneway'), OneWay := gsub(x = Entry, pattern = '.*v.*"(.*?)".*', replacement = '\\1')]
dtWays[grepl(x = Entry, pattern = 'k="highway"'), Type := gsub(x = Entry, pattern = '.*v.*"(.*?)".*', replacement = '\\1')]

dtWays = dtWays[,
   list(
      NodeID = NodeID[!is.na(NodeID)],
      OneWay = OneWay[!is.na(OneWay)][1],
      Type = Type[!is.na(Type)][1]
   ),
   PathID
]


dtWays = dtWays[,
   list(
      SourceID = head(NodeID,-1),
      DestinationID = tail(NodeID,-1)
   ),
   list(
      PathID,
      OneWay,
      Type
   )
]

# alternating, reversing, etc. also forced to no for simplicity
dtWays[OneWay != 'yes', OneWay := 'no']
dtWays[is.na(OneWay), OneWay := 'no']


write.csv(
   dtWays,
   file = paste0(
      cAtherDataLocation,
      '/Raw/RAO/',
      cCityName,
      '/OSMData/ways.txt'
   ),
   quote = F,
   na = '',
   row.names = F
)

