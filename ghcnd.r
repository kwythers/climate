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
require(readr)  # for read_csv()


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

# dt <- read_fwf("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data/USC00217087.dly",
#                       fwf_widths(c(11, 4, 2, 4, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
#                                    5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1),
#                                  c("STATION_ID", "YEAR", "MONTH", "ELEMENT",
#                                    "VALUE1", "MFLAG1", "QFLAG1", "SFLAG1",
#                                     "VALUE2", "MFLAG2", "QFLAG2", "SFLAG2",
#                                     "VALUE3", "MFLAG3", "QFLAG3", "SFLAG3",
#                                     "VALUE4", "MFLAG4", "QFLAG4", "SFLAG4",
#                                     "VALUE5", "MFLAG5", "QFLAG5", "SFLAG5",
#                                     "VALUE6", "MFLAG6", "QFLAG6", "SFLAG6",
#                                     "VALUE7", "MFLAG7", "QFLAG7", "SFLAG7",
#                                     "VALUE8", "MFLAG8", "QFLAG8", "SFLAG8",
#                                     "VALUE9", "MFLAG9", "QFLAG9", "SFLAG9",
#                                     "VALUE10", "MFLAG10", "QFLAG10", "SFLAG10",
#                                     "VALUE11", "MFLAG11", "QFLAG11", "SFLAG11",
#                                     "VALUE12", "MFLAG12", "QFLAG12", "SFLAG12",
#                                     "VALUE13", "MFLAG13", "QFLAG13", "SFLAG13",
#                                     "VALUE14", "MFLAG14", "QFLAG14", "SFLAG14",
#                                     "VALUE15", "MFLAG15", "QFLAG15", "SFLAG15",
#                                     "VALUE16", "MFLAG16", "QFLAG16", "SFLAG16",
#                                     "VALUE17", "MFLAG17", "QFLAG17", "SFLAG17",
#                                     "VALUE18", "MFLAG18", "QFLAG18", "SFLAG18",
#                                     "VALUE19", "MFLAG19", "QFLAG19", "SFLAG19",
#                                     "VALUE20", "MFLAG20", "QFLAG20", "SFLAG20",
#                                     "VALUE21", "MFLAG21", "QFLAG21", "SFLAG21",
#                                     "VALUE22", "MFLAG22", "QFLAG22", "SFLAG22",
#                                     "VALUE23", "MFLAG23", "QFLAG23", "SFLAG23",
#                                     "VALUE24", "MFLAG24", "QFLAG24", "SFLAG24",
#                                     "VALUE25", "MFLAG25", "QFLAG25", "SFLAG25",
#                                     "VALUE26", "MFLAG26", "QFLAG26", "SFLAG26",
#                                     "VALUE27", "MFLAG27", "QFLAG27", "SFLAG27",
#                                     "VALUE28", "MFLAG28", "QFLAG28", "SFLAG28",
#                                     "VALUE29", "MFLAG29", "QFLAG29", "SFLAG29",
#                                     "VALUE30", "MFLAG30", "QFLAG30", "SFLAG30",
#                                     "VALUE31", "MFLAG31", "QFLAG31", "SFLAG31")
#                                  ),
#                  col_types = cols(STATION_ID = "c", YEAR = "i", MONTH = "i", ELEMENT = "c", 
#                    VALUE1 = "i", MFLAG1 = "c", QFLAG1 = "c", SFLAG1 = "c",
#                    VALUE2 = "i", MFLAG2 = "c", QFLAG2 = "c", SFLAG2 = "c",
#                    VALUE3 = "i", MFLAG3 = "c", QFLAG3 = "c", SFLAG3 = "c",
#                    VALUE4 = "i", MFLAG4 = "c", QFLAG4 = "c", SFLAG4 = "c",
#                    VALUE5 = "i", MFLAG5 = "c", QFLAG5 = "c", SFLAG5 = "c",
#                    VALUE6 = "i", MFLAG6 = "c", QFLAG6 = "c", SFLAG6 = "c",
#                    VALUE7 = "i", MFLAG7 = "c", QFLAG7 = "c", SFLAG7 = "c",
#                    VALUE8 = "i", MFLAG8 = "c", QFLAG8 = "c", SFLAG8 = "c",
#                    VALUE9 = "i", MFLAG9 = "c", QFLAG9 = "c", SFLAG9 = "c",
#                    VALUE10 = "i", MFLAG10 = "c", QFLAG10 = "c", SFLAG10 = "c",
#                    VALUE11 = "i", MFLAG11 = "c", QFLAG11 = "c", SFLAG11 = "c",
#                    VALUE12 = "i", MFLAG12 = "c", QFLAG12 = "c", SFLAG12 = "c",
#                    VALUE13 = "i", MFLAG13 = "c", QFLAG13 = "c", SFLAG13 = "c",
#                    VALUE14 = "i", MFLAG14 = "c", QFLAG14 = "c", SFLAG14 = "c",
#                    VALUE15 = "i", MFLAG15 = "c", QFLAG15 = "c", SFLAG15 = "c",
#                    VALUE16 = "i", MFLAG16 = "c", QFLAG16 = "c", SFLAG16 = "c",
#                    VALUE17 = "i", MFLAG17 = "c", QFLAG17 = "c", SFLAG17 = "c",
#                    VALUE18 = "i", MFLAG18 = "c", QFLAG18 = "c", SFLAG18 = "c",
#                    VALUE19 = "i", MFLAG19 = "c", QFLAG19 = "c", SFLAG19 = "c",
#                    VALUE20 = "i", MFLAG20 = "c", QFLAG20 = "c", SFLAG20 = "c",
#                    VALUE21 = "i", MFLAG21 = "c", QFLAG21 = "c", SFLAG21 = "c",
#                    VALUE22 = "i", MFLAG22 = "c", QFLAG22 = "c", SFLAG22 = "c",
#                    VALUE23 = "i", MFLAG23 = "c", QFLAG23 = "c", SFLAG23 = "c",
#                    VALUE24 = "i", MFLAG24 = "c", QFLAG24 = "c", SFLAG24 = "c",
#                    VALUE25 = "i", MFLAG25 = "c", QFLAG25 = "c", SFLAG25 = "c",
#                    VALUE26 = "i", MFLAG26 = "c", QFLAG26 = "c", SFLAG26 = "c",
#                    VALUE27 = "i", MFLAG27 = "c", QFLAG27 = "c", SFLAG27 = "c",
#                    VALUE28 = "i", MFLAG28 = "c", QFLAG28 = "c", SFLAG28 = "c",
#                    VALUE29 = "i", MFLAG29 = "c", QFLAG29 = "c", SFLAG29 = "c",
#                    VALUE30 = "i", MFLAG30 = "c", QFLAG30 = "c", SFLAG30 = "c",
#                    VALUE31 = "i", MFLAG31 = "c", QFLAG31 = "c", SFLAG31 = "c")
#                  )

##############################################
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

columntypes <- c(STATION_ID = "c", YEAR = "i", MONTH = "i", ELEMENT = "c", 
              VALUE1 = "i", MFLAG1 = "c", QFLAG1 = "c", SFLAG1 = "c",
              VALUE2 = "i", MFLAG2 = "c", QFLAG2 = "c", SFLAG2 = "c",
              VALUE3 = "i", MFLAG3 = "c", QFLAG3 = "c", SFLAG3 = "c",
              VALUE4 = "i", MFLAG4 = "c", QFLAG4 = "c", SFLAG4 = "c",
              VALUE5 = "i", MFLAG5 = "c", QFLAG5 = "c", SFLAG5 = "c",
              VALUE6 = "i", MFLAG6 = "c", QFLAG6 = "c", SFLAG6 = "c",
              VALUE7 = "i", MFLAG7 = "c", QFLAG7 = "c", SFLAG7 = "c",
              VALUE8 = "i", MFLAG8 = "c", QFLAG8 = "c", SFLAG8 = "c",
              VALUE9 = "i", MFLAG9 = "c", QFLAG9 = "c", SFLAG9 = "c",
              VALUE10 = "i", MFLAG10 = "c", QFLAG10 = "c", SFLAG10 = "c",
              VALUE11 = "i", MFLAG11 = "c", QFLAG11 = "c", SFLAG11 = "c",
              VALUE12 = "i", MFLAG12 = "c", QFLAG12 = "c", SFLAG12 = "c",
              VALUE13 = "i", MFLAG13 = "c", QFLAG13 = "c", SFLAG13 = "c",
              VALUE14 = "i", MFLAG14 = "c", QFLAG14 = "c", SFLAG14 = "c",
              VALUE15 = "i", MFLAG15 = "c", QFLAG15 = "c", SFLAG15 = "c",
              VALUE16 = "i", MFLAG16 = "c", QFLAG16 = "c", SFLAG16 = "c",
              VALUE17 = "i", MFLAG17 = "c", QFLAG17 = "c", SFLAG17 = "c",
              VALUE18 = "i", MFLAG18 = "c", QFLAG18 = "c", SFLAG18 = "c",
              VALUE19 = "i", MFLAG19 = "c", QFLAG19 = "c", SFLAG19 = "c",
              VALUE20 = "i", MFLAG20 = "c", QFLAG20 = "c", SFLAG20 = "c",
              VALUE21 = "i", MFLAG21 = "c", QFLAG21 = "c", SFLAG21 = "c",
              VALUE22 = "i", MFLAG22 = "c", QFLAG22 = "c", SFLAG22 = "c",
              VALUE23 = "i", MFLAG23 = "c", QFLAG23 = "c", SFLAG23 = "c",
              VALUE24 = "i", MFLAG24 = "c", QFLAG24 = "c", SFLAG24 = "c",
              VALUE25 = "i", MFLAG25 = "c", QFLAG25 = "c", SFLAG25 = "c",
              VALUE26 = "i", MFLAG26 = "c", QFLAG26 = "c", SFLAG26 = "c",
              VALUE27 = "i", MFLAG27 = "c", QFLAG27 = "c", SFLAG27 = "c",
              VALUE28 = "i", MFLAG28 = "c", QFLAG28 = "c", SFLAG28 = "c",
              VALUE29 = "i", MFLAG29 = "c", QFLAG29 = "c", SFLAG29 = "c",
              VALUE30 = "i", MFLAG30 = "c", QFLAG30 = "c", SFLAG30 = "c",
              VALUE31 = "i", MFLAG31 = "c", QFLAG31 = "c", SFLAG31 = "c")

columntypes2 <- c("string", "integer", "integer", "string", 
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

####################################################################
#data_path <- "/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data/"   # path to the data
# put all data file names into a list
#files <- dir(data_path, pattern = "*.dly") # get file names
#files <- list.files(path = "/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data", pattern = ".dly", full.names = TRUE)
#data <- files
#read_fwf(data, fwf_widths(widths, names), types) 
#alldata <- lapply(files, read_fwf(files, fwf_widths(widths, names), types))
  
# data <- 
#   do.call("rbind", 
#           lapply(data, 
#                  function(x) 
#                    read_fwf(files, fwf_widths(widths, names), types)))
####################################################################
setwd("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data100/")

file_list <- list.files()

for (file in file_list){ 
  # if the merged dataset doesn't exist, create it 
  if (!exists("dataset")){ 
    dataset <- read_fwf(file, fwf_widths(c(11, 4, 2, 4, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                           5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1), 
                                         c("STATION_ID", "YEAR", "MONTH", "ELEMENT", 
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
                                          "VALUE31", "MFLAG31", "QFLAG31", "SFLAG31")), 
                        col_types = cols(STATION_ID = "c", YEAR = "i", MONTH = "i", ELEMENT = "c", 
                                         VALUE1 = "i", MFLAG1 = "c", QFLAG1 = "c", SFLAG1 = "c",
                                         VALUE2 = "i", MFLAG2 = "c", QFLAG2 = "c", SFLAG2 = "c",
                                         VALUE3 = "i", MFLAG3 = "c", QFLAG3 = "c", SFLAG3 = "c",
                                         VALUE4 = "i", MFLAG4 = "c", QFLAG4 = "c", SFLAG4 = "c",
                                         VALUE5 = "i", MFLAG5 = "c", QFLAG5 = "c", SFLAG5 = "c",
                                         VALUE6 = "i", MFLAG6 = "c", QFLAG6 = "c", SFLAG6 = "c",
                                         VALUE7 = "i", MFLAG7 = "c", QFLAG7 = "c", SFLAG7 = "c",
                                         VALUE8 = "i", MFLAG8 = "c", QFLAG8 = "c", SFLAG8 = "c",
                                         VALUE9 = "i", MFLAG9 = "c", QFLAG9 = "c", SFLAG9 = "c",
                                         VALUE10 = "i", MFLAG10 = "c", QFLAG10 = "c", SFLAG10 = "c",
                                         VALUE11 = "i", MFLAG11 = "c", QFLAG11 = "c", SFLAG11 = "c",
                                         VALUE12 = "i", MFLAG12 = "c", QFLAG12 = "c", SFLAG12 = "c",
                                         VALUE13 = "i", MFLAG13 = "c", QFLAG13 = "c", SFLAG13 = "c",
                                         VALUE14 = "i", MFLAG14 = "c", QFLAG14 = "c", SFLAG14 = "c",
                                         VALUE15 = "i", MFLAG15 = "c", QFLAG15 = "c", SFLAG15 = "c",
                                         VALUE16 = "i", MFLAG16 = "c", QFLAG16 = "c", SFLAG16 = "c",
                                         VALUE17 = "i", MFLAG17 = "c", QFLAG17 = "c", SFLAG17 = "c",
                                         VALUE18 = "i", MFLAG18 = "c", QFLAG18 = "c", SFLAG18 = "c",
                                         VALUE19 = "i", MFLAG19 = "c", QFLAG19 = "c", SFLAG19 = "c",
                                         VALUE20 = "i", MFLAG20 = "c", QFLAG20 = "c", SFLAG20 = "c",
                                         VALUE21 = "i", MFLAG21 = "c", QFLAG21 = "c", SFLAG21 = "c",
                                         VALUE22 = "i", MFLAG22 = "c", QFLAG22 = "c", SFLAG22 = "c",
                                         VALUE23 = "i", MFLAG23 = "c", QFLAG23 = "c", SFLAG23 = "c",
                                         VALUE24 = "i", MFLAG24 = "c", QFLAG24 = "c", SFLAG24 = "c",
                                         VALUE25 = "i", MFLAG25 = "c", QFLAG25 = "c", SFLAG25 = "c",
                                         VALUE26 = "i", MFLAG26 = "c", QFLAG26 = "c", SFLAG26 = "c",
                                         VALUE27 = "i", MFLAG27 = "c", QFLAG27 = "c", SFLAG27 = "c",
                                         VALUE28 = "i", MFLAG28 = "c", QFLAG28 = "c", SFLAG28 = "c",
                                         VALUE29 = "i", MFLAG29 = "c", QFLAG29 = "c", SFLAG29 = "c",
                                         VALUE30 = "i", MFLAG30 = "c", QFLAG30 = "c", SFLAG30 = "c",
                                         VALUE31 = "i", MFLAG31 = "c", QFLAG31 = "c", SFLAG31 = "c"), progress = interactive())
    }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- read_fwf(file, fwf_widths(c(11, 4, 2, 4, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 
                                                5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1), 
                                              c("STATION_ID", "YEAR", "MONTH", "ELEMENT", 
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
                                                "VALUE31", "MFLAG31", "QFLAG31", "SFLAG31")), 
                             col_types = cols(STATION_ID = "c", YEAR = "i", MONTH = "i", ELEMENT = "c", 
                                              VALUE1 = "i", MFLAG1 = "c", QFLAG1 = "c", SFLAG1 = "c",
                                              VALUE2 = "i", MFLAG2 = "c", QFLAG2 = "c", SFLAG2 = "c",
                                              VALUE3 = "i", MFLAG3 = "c", QFLAG3 = "c", SFLAG3 = "c",
                                              VALUE4 = "i", MFLAG4 = "c", QFLAG4 = "c", SFLAG4 = "c",
                                              VALUE5 = "i", MFLAG5 = "c", QFLAG5 = "c", SFLAG5 = "c",
                                              VALUE6 = "i", MFLAG6 = "c", QFLAG6 = "c", SFLAG6 = "c",
                                              VALUE7 = "i", MFLAG7 = "c", QFLAG7 = "c", SFLAG7 = "c",
                                              VALUE8 = "i", MFLAG8 = "c", QFLAG8 = "c", SFLAG8 = "c",
                                              VALUE9 = "i", MFLAG9 = "c", QFLAG9 = "c", SFLAG9 = "c",
                                              VALUE10 = "i", MFLAG10 = "c", QFLAG10 = "c", SFLAG10 = "c",
                                              VALUE11 = "i", MFLAG11 = "c", QFLAG11 = "c", SFLAG11 = "c",
                                              VALUE12 = "i", MFLAG12 = "c", QFLAG12 = "c", SFLAG12 = "c",
                                              VALUE13 = "i", MFLAG13 = "c", QFLAG13 = "c", SFLAG13 = "c",
                                              VALUE14 = "i", MFLAG14 = "c", QFLAG14 = "c", SFLAG14 = "c",
                                              VALUE15 = "i", MFLAG15 = "c", QFLAG15 = "c", SFLAG15 = "c",
                                              VALUE16 = "i", MFLAG16 = "c", QFLAG16 = "c", SFLAG16 = "c",
                                              VALUE17 = "i", MFLAG17 = "c", QFLAG17 = "c", SFLAG17 = "c",
                                              VALUE18 = "i", MFLAG18 = "c", QFLAG18 = "c", SFLAG18 = "c",
                                              VALUE19 = "i", MFLAG19 = "c", QFLAG19 = "c", SFLAG19 = "c",
                                              VALUE20 = "i", MFLAG20 = "c", QFLAG20 = "c", SFLAG20 = "c",
                                              VALUE21 = "i", MFLAG21 = "c", QFLAG21 = "c", SFLAG21 = "c",
                                              VALUE22 = "i", MFLAG22 = "c", QFLAG22 = "c", SFLAG22 = "c",
                                              VALUE23 = "i", MFLAG23 = "c", QFLAG23 = "c", SFLAG23 = "c",
                                              VALUE24 = "i", MFLAG24 = "c", QFLAG24 = "c", SFLAG24 = "c",
                                              VALUE25 = "i", MFLAG25 = "c", QFLAG25 = "c", SFLAG25 = "c",
                                              VALUE26 = "i", MFLAG26 = "c", QFLAG26 = "c", SFLAG26 = "c",
                                              VALUE27 = "i", MFLAG27 = "c", QFLAG27 = "c", SFLAG27 = "c",
                                              VALUE28 = "i", MFLAG28 = "c", QFLAG28 = "c", SFLAG28 = "c",
                                              VALUE29 = "i", MFLAG29 = "c", QFLAG29 = "c", SFLAG29 = "c",
                                              VALUE30 = "i", MFLAG30 = "c", QFLAG30 = "c", SFLAG30 = "c",
                                              VALUE31 = "i", MFLAG31 = "c", QFLAG31 = "c", SFLAG31 = "c"), progress = interactive()) 
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
    } 
}

## convert to data tables
dataset <- as.data.table(dataset)
countries <- as.data.table(countries)
inventory <- as.data.table(inventory)
stations <- as.data.table(stations)
states <- as.data.table(states)

setkey(dataset, STATION_ID)
setkey(inventory, STATION_ID)
setkey(stations, STATION_ID)


#####################
#   TEST
#####################


laf <- laf_open_fwf("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data/USC00212737.dly", column_widths = columnwidths, 
                    column_types = columntypes,
                    column_names = columnnames)
x <- laf_open_fwf(filename = "/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data/USC00212737.dly", 
                  column_types = columntypes2, column_widths = columnwidths, column_names = columnnames)

xx <- x[,]


#####################
setwd("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data100/")
  
five_file_list <- list.files()

xxx <- do.call("rbind",lapply(five_file_list, FUN=function(files) {
  x <- laf_open_fwf(filename = files, column_types = columntypes2, 
                    column_widths = columnwidths, column_names = columnnames) 
  xx <- x[, ] 
    } 
  )
)

############### USE TO CREATE NEW DATA FILE ###############
setwd("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data5/")

five_file_list <- list.files()

for (file in five_file_list){
  
  # if the merged dataset doesn't exist, create it, and add records
  if (!exists("dataset")){
    dataset <- do.call("rbind", lapply(five_file_list, FUN=function(files) {
      x <- laf_open_fwf(filename = files, column_types = columntypes2, 
                        column_widths = columnwidths, column_names = columnnames) 
      xx <- x[, ] 
    } 
    )
    )
  }
}
############### USE FOR APPENDING TO EXISTING DATA FILE ###############
setwd("/Users/kirkw/projects/noaa_climate/ghcnd_hcn/data2/")

two_file_list <- list.files()

for (file in two_file_list){ 
  
  # if the merged dataset does exist and new records are being read, append to it
  if (exists("dataset")){
    temp_dataset <- do.call("rbind", lapply(two_file_list, FUN=function(files) {
      x <- laf_open_fwf(filename = files, column_types = columntypes2, 
                        column_widths = columnwidths, column_names = columnnames) 
      xx <- x[, ] 
    }  
    ) 
    ) 
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}



