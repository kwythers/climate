#############################################
#     Script to process MNgage data         #
#     from Minnesota Climate office         #
#             Kirk R. Wythers               #
#             2017.01.12                    #
#############################################
# Process MNgage precipitation data from one column
# per day wide format to long format

# clear and load data file

rm(list=ls())
library(tidyverse)
library(data.table)
library(splitstackshape)
library(lubridate)
mn_precip <- read_csv("~/projects/mngage/mnprecip1970-2016.csv")

# convert to data.table and add an index column for joining on
mnp <- as.data.table(mn_precip)

mnp.m1 <- melt(mnp, id.vars = c("county_township_range_section_station_owner", 
                                "latitude",
                                "longitude",
                                "easting",
                                "northing",
                                "sponsor",
                                "time",
                                "yyyy_mo"
                                ), 
                measure.vars = c("1", 
                                "2",                                     
                                "3",                                     
                                "4",                                    
                                "5",                                     
                                "6",                                     
                                "7",                                     
                                "8",                                     
                                "9",                                     
                                "10",                                    
                                "11",                                    
                                "12",                                    
                                "13",                                    
                                "14",                                    
                                "15",                                    
                                "16",                                    
                                "17",                                    
                                "18",                                    
                                "19",                                    
                                "20",                                    
                                "21",                                    
                                "22",                                    
                                "23",                                    
                                "24",                                    
                                "25",                                    
                                "26",                                    
                                "27",                                    
                                "28",                                    
                                "29",                                    
                                "30",                                    
                                "31"
                                ) 
        )

mnp.m2 <- melt(mnp, id.vars = c("county_township_range_section_station_owner", 
                                "latitude",
                                "longitude",
                                "easting",
                                "northing",
                                "sponsor",
                                "time",
                                "yyyy_mo"
                                ), 
                measure.vars = c("f1",                                    
                                 "f2",                                    
                                 "f3",                                    
                                 "f4",                                    
                                 "f5",                                    
                                 "f6",                                    
                                 "f7",                                    
                                 "f8",                                    
                                 "f9",                                    
                                 "f10",                                   
                                 "f11",                                   
                                 "f12",                                   
                                 "f13",                                   
                                 "f14",                                   
                                 "f15",                                   
                                 "f16",                                   
                                 "f17",                                   
                                 "f18",                                   
                                 "f19",                                   
                                 "f20",                                   
                                 "f21",                                   
                                 "f22",                                   
                                 "f23",                                   
                                 "f24",                                   
                                 "f25",                                   
                                 "f26",                                   
                                 "f27",                                   
                                 "f28",                                   
                                 "f29",                                 
                                 "f30",                                   
                                 "f31" 
                                 ) 
               )
# add an rownumber based index to each data table to perform the join on
mnp.m1[,index:=.GRP, by = row.names(mnp.m1)]
mnp.m2[,index:=.GRP, by = row.names(mnp.m2)]

# rename columns from first data table
names(mnp.m1) # view the column names
names(mnp.m1)[9]<-"day"
names(mnp.m1)[10]<-"precip"

# rename columns from second data table
names(mnp.m2) # view the column names
names(mnp.m2)[10]<-"flag"

# remove un-needed/duplicate columns from second data table
mnp.m2[ , c("county_township_range_section_station_owner", "latitude", 
              "longitude", "easting", "northing", "sponsor", 
              "time", "yyyy_mo", "variable") := NULL]

# do the join 
mnp.m3 <- mnp.m1[mnp.m2, on = "index"]

# split "county township range section station" column on white space and rename
mnp.m4 <- cSplit(mnp.m3, "county_township_range_section_station_owner", sep = " ", direction = "wide", fixed = TRUE,
       drop = TRUE, stripWhite = TRUE, makeEqual = NULL, type.convert = TRUE)
names(mnp.m4) # look at column names
names(mnp.m4)[12] <- "county"
names(mnp.m4)[13] <- "township"
names(mnp.m4)[14] <- "range"
names(mnp.m4)[15] <- "section"
names(mnp.m4)[16] <- "station"
names(mnp.m4)[17] <- "owner"

# drop column 18
mnp.m4[, 18] <- NULL

# add new column based on yyyy_mo, split into yyyy mo, concatinate yyyy, mo, and day
# into a date column, remove extraneous columns, and re-order
mnp.m4[, "year_month" := yyyy_mo]
mnp.m5 <- cSplit(mnp.m4, "year_month", sep = "_", direction = "wide", fixed = TRUE,
                 drop = TRUE, stripWhite = TRUE, makeEqual = NULL, type.convert = TRUE)
mnp.m5$date <- with(mnp.m5, ymd(sprintf('%04d%02d%02d', year_month_1, year_month_2, day)))
# mnp.m5[ , c("year_month_1", "year_month_2") := NULL]
names(mnp.m5)[18] <- "year"
names(mnp.m5)[19] <- "month"
mnp.m5$owner <- mnp.m5$owner %>% na.omit()
mnp.m6 <- unite(mnp.m5, "station_owner", station, owner, sep = " ")
mnp.m7 <- mnp.m6[, c(10, 19, 17:18, 8, 16, 1:2, 5:6, 9, 11)]

# write unique stations to an object with "distinct"
mnp.m8 <- distinct(mnp.m7, latitude, longitude, .keep_all = TRUE)
stations <- mnp.m8[, c(6:9)]
# change longitude sign
stations <- transmute(stations, station_owner = station_owner,
            latitude = latitude,
            longitude = 0 - longitude,
            sponsor = sponsor)

# write output to .csv
write_csv(mnp.m7,"~/projects/mngage/mnprecipfinal1970-2016.csv")
write_csv(stations, "~/projects/mngage/stations.csv")
