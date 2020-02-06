#############################################################
#                                                           #
#         Script to process GHCND data from NOAA            #
#                   Kirk R. Wythers                         #
#                     2017.01.12                            #
#                                                           #
#############################################################
# Process GHCND climate data 
# clear and load data file
rm(list=ls())
#library(readr)
#library(data.table)
#library(rnoaa)
#library(splitstackshape)
#library(plyr)
#library(data.table)
#library(stringi)
# require(dplyr)  # for mutate()
# require(tidyr)  # for unnest()
# require(purrr)  # for map(), reduce()
require(data.table)
require(LaF)
require(tidyverse)  # for read_csv()

columnwidths <- c(11, 4, 2, 4, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                  5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1)

columnnames <- c("STATION_ID", "YEAR", "MONTH", "ELEMENT",
                 "VALUE1", "MFLAG1", "QFLAG1", "SFLAG1",
                 "VALUE2", "MFLAG2", "QFLAG2", "SFLAG2",
                 "VALUE3", "MFLAG3", "QFLAG3", "SFLAG3",
                 "VALUE4", "MFLAG4", "QFLAG4", "SFLAG4",
                 "VALUE5", "MFLAG5", "QFLAG5", "SFLAG5",
                 "VALUE6", "MFLAG6", "QFLAG6", "SFLAG6",
                 "VALUE7", "MFLAG7", "QFLAG7", "SFLAG7",
                 "VALUE8", "MFLAG8", "QFLAG8", "SFLAG8",
                 "VALUE9", "MFLAG9", "QFLAG9", "SFLAG9",
                 "VALUE10", "MFLAG10", "QFLAG10", "SFLAG10",
                 "VALUE11", "MFLAG11", "QFLAG11", "SFLAG11",
                 "VALUE12", "MFLAG12", "QFLAG12", "SFLAG12",
                 "VALUE13", "MFLAG13", "QFLAG13", "SFLAG13",
                 "VALUE14", "MFLAG14", "QFLAG14", "SFLAG14",
                 "VALUE15", "MFLAG15", "QFLAG15", "SFLAG15",
                 "VALUE16", "MFLAG16", "QFLAG16", "SFLAG16",
                 "VALUE17", "MFLAG17", "QFLAG17", "SFLAG17",
                 "VALUE18", "MFLAG18", "QFLAG18", "SFLAG18",
                 "VALUE19", "MFLAG19", "QFLAG19", "SFLAG19",
                 "VALUE20", "MFLAG20", "QFLAG20", "SFLAG20",
                 "VALUE21", "MFLAG21", "QFLAG21", "SFLAG21",
                 "VALUE22", "MFLAG22", "QFLAG22", "SFLAG22",
                 "VALUE23", "MFLAG23", "QFLAG23", "SFLAG23",
                 "VALUE24", "MFLAG24", "QFLAG24", "SFLAG24",
                 "VALUE25", "MFLAG25", "QFLAG25", "SFLAG25",
                 "VALUE26", "MFLAG26", "QFLAG26", "SFLAG26",
                 "VALUE27", "MFLAG27", "QFLAG27", "SFLAG27",
                 "VALUE28", "MFLAG28", "QFLAG28", "SFLAG28",
                 "VALUE29", "MFLAG29", "QFLAG29", "SFLAG29",
                 "VALUE30", "MFLAG30", "QFLAG30", "SFLAG30",
                 "VALUE31", "MFLAG31", "QFLAG31", "SFLAG31")

columntypes <- c("string", "integer", "integer", "string", 
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string",
                  "integer", "string", "string", "string")



stations <- read_fwf("/Users/kirkw/projects/noaa_climate/ghcnd-stations.txt", 
                     fwf_widths(c(11, 9, 10, 7, 3, 31, 4, 4, 6), 
                                c("STATION_ID", "LAT", "LON", "ELEVATION", 
                                  "STATE", "NAME", "GSN_FLAG", "HCN-CRN_FLAG", "WMO_ID"))
)

inventory <- read_fwf("/Users/kirkw/projects/noaa_climate/ghcnd-inventory.txt",
                      fwf_widths(c(12, 10, 9, 5, 5, 4), 
                                 c("STATION_ID", "LAT", "LON", "ELEMENT", "FIRSTYEAR", "LASTYEAR"))
)

states <- read_fwf("/Users/kirkw/projects/noaa_climate/ghcnd-states.txt",
                   fwf_widths(c(3, NA), 
                              c("POSTAL_CODE", "STATE_NAME"))
)

countries <- read_fwf("/Users/kirkw/projects/noaa_climate/ghcnd-countries.txt",
                      fwf_widths(c(3, NA), 
                                 c("A2_CODE", "COUNTRY_NAME"))
)


############### USE TO CREATE NEW DATA FILE ###############
setwd("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data/")

file_list <- list.files()

# Start the clock!
ptm <- proc.time()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it, and add records
  if (!exists("dataset")){
    dataset <- do.call("rbind", lapply(file_list, FUN=function(files) {
      x <- laf_open_fwf(filename = files, column_types = columntypes, 
                        column_widths = columnwidths, column_names = columnnames) 
      xx <- x[, ] 
    } 
    )
    )
  }
}

# Stop the clock
proc.time() - ptm
##############################
setwd("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data/")

file_list <- list.files()

for (file in file_list){ 
  
  # if the merged dataset does exist and new records are being read, append to it
  if (!exists("tb")){
    tb <- do.call("rbind", lapply(file_list, FUN=function(files) {
      x <- laf_open_fwf(filename = files, column_types = columntypes, 
                        column_widths = columnwidths, column_names = columnnames) 
      xx <- x[, ] 
    } 
    ) 
    ) 
  }
  }




## convert to data tables
dataset <- as.data.table(dataset)
countries <- as.data.table(countries)
inventory <- as.data.table(inventory)
stations <- as.data.table(stations)
states <- as.data.table(states)
## set keys
setkey(dataset, STATION_ID)
setkey(inventory, STATION_ID)
setkey(stations, STATION_ID)


