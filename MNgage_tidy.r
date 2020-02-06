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

# add index for station ids
# mn_precip <- mn_precip %>%
#   mutate(station_number = row_number())

## gather days for 31 day columns for values and flags
mnp_long_v <- mn_precip %>% 
  gather(key = DAY, 
         value = VALUE, 
         `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, 
         `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, `20`, 
         `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`, `29`, `30`, 
         `31`, convert = TRUE)

mnp_long_f <- mn_precip %>% 
  gather(key = DAY, value = FLAG, 
         `f1`, `f2`, `f3`, `f4`, `f5`, `f6`, `f7`, `f8`, `f9`, `f10`,
         `f11`, `f12`, `f13`, `f14`, `f15`, `f16`, `f17`, `f18`, `f19`, `f20`,
         `f21`, `f22`, `f23`, `f24`, `f25`, `f26`, `f27`, `f28`, `f29`, `f30`,
         `f31`, convert = TRUE)

# select columns to keep
mnp_long_v <- select(mnp_long_v, county_township_range_section_station_owner, latitude, longitude, sponsor, time, yyyy_mo, DAY, VALUE)
mnp_long_f <- select(mnp_long_f, county_township_range_section_station_owner, latitude, longitude, sponsor, time, yyyy_mo, DAY, FLAG)

# pull apart the "f*" column to get days of the month
mnp_long_f <- mnp_long_f %>% 
  separate(DAY, c("f", "day"), 1, convert = TRUE)

# pull apart the "yyyy_mo" column to get year and month
mnp_long_f <- mnp_long_f %>% 
  separate(yyyy_mo, c("year", "month"), convert = TRUE)
mnp_long_v <- mnp_long_v %>% 
  separate(yyyy_mo, c("year", "month"), convert = TRUE)

# count primary keys
mnp_long_f %>% 
  count(county_township_range_section_station_owner, year, month, day) %>%
  filter(n > 1)

# join "value" and "flag" tables
mnp_long <- inner_join(mnp_long_v, mnp_long_f, by = c("county_township_range_section_station_owner", 
                                                      "latitude", "longitude", "sponsor", "time", "year", "month", 
                                                      "DAY" = "day"))
# drop column "f"
mnp_long <- mnp_long %>%
  select(-f)
# rename some things
mnp_long <- rename(mnp_long, day = DAY, value = VALUE, flag = FLAG)

# make a new column for "station"
# x <- mnp_long %>%
#   mutate(station = county_township_range_section_station_owner) 
# xx <- x %>% 
#   separate(station, into = c("sta", "tion"), sep = "[a-zA-Z][a-zA-Z\\s]+$", remove = TRUE)
###"$[A-Z+$]"

# make a copy of "county_township_range_section_station_owner" to use as "station_name"
mnp_long <- mnp_long %>%
  mutate(station_id = county_township_range_section_station_owner)

# separate "county_township_range_section_station_owner" into individual columns
mnp_long <- mnp_long %>% 
  separate(county_township_range_section_station_owner, c("county", "township", "range", 
                                                          "section", "station", "owner"), convert = TRUE)

# concatinate "station" and "owner" into "station"
mnp_long <- mnp_long %>%
  unite(station_name, station, owner, sep = " ", remove = TRUE)

# xx <- cSplit(mnp_long, "county_township_range_section_station_owner", sep = " ", direction = "wide", fixed = TRUE,
#                 drop = TRUE, stripWhite = TRUE, makeEqual = NULL, type.convert = TRUE)

# make new date column while keeping year, month, and day
mnp_long <- mnp_long  %>% 
  mutate(date = make_date(year, month, day)
         )

# check some variables
names(mnp_long)
sum(is.na(mnp_long$station_name))
sum(is.na(mnp_long$county))
sum(is.na(mnp_long$township))
sum(is.na(mnp_long$range))
sum(is.na(mnp_long$section))
sum(is.na(mnp_long$owner))
sum(is.na(mnp_long$sponsor))

mnp_long <- mnp_long %>%
  select(station_id, station_name, date, everything()
         )

mnp_long <- mnp_long %>%
  transmute(station_id = station_id, 
            station_name = station_name, 
            date = date, 
            county = county, 
            township = township, 
            range = range, 
            section = section,
            latitude = latitude, 
            longitude = 0 - longitude, 
            sponsor = sponsor, 
            time = time, 
            year = year, 
            month = month, 
            day = day, 
            value = value, 
            flag = flag 
            )

# remove f and v for memory management
rm(mnp_long_f)
rm(mnp_long_v)

# create date for unique "stations" table
stations <- mnp_long %>% 
  distinct(station_id, latitude, longitude, .keep_all = FALSE)

stations <- stations %>%
  transmute(station_id = station_id, 
            latitude = latitude, 
            longitude = 0 - longitude
            )

# write output to .csv
write_csv(mnp_long,"~/Desktop/mnprecipfinal1970-2016_3.csv")
write_csv(stations, "~/Desktop/stations_3.csv")

# # add an rownumber based index to each data table to perform the join on
# mnp.m1[,index:=.GRP, by = row.names(mnp.m1)]
# mnp.m2[,index:=.GRP, by = row.names(mnp.m2)]
# 
# # rename columns from first data table
# names(mnp.m1) # view the column names
# names(mnp.m1)[9]<-"day"
# names(mnp.m1)[10]<-"precip"
# 
# # rename columns from second data table
# names(mnp.m2) # view the column names
# names(mnp.m2)[10]<-"flag"
# 
# # remove un-needed/duplicate columns from second data table
# mnp.m2[ , c("county_township_range_section_station_owner", "latitude", 
#               "longitude", "easting", "northing", "sponsor", 
#               "time", "yyyy_mo", "variable") := NULL]
# 
# # do the join 
# mnp.m3 <- mnp.m1[mnp.m2, on = "index"]
# 
# # split "county township range section station" column on white space and rename
# mnp.m4 <- cSplit(mnp.m3, "county_township_range_section_station_owner", sep = " ", direction = "wide", fixed = TRUE,
#        drop = TRUE, stripWhite = TRUE, makeEqual = NULL, type.convert = TRUE)
# names(mnp.m4) # look at column names
# names(mnp.m4)[12] <- "county"
# names(mnp.m4)[13] <- "township"
# names(mnp.m4)[14] <- "range"
# names(mnp.m4)[15] <- "section"
# names(mnp.m4)[16] <- "station"
# names(mnp.m4)[17] <- "owner"
# 
# # drop column 18
# mnp.m4[, 18] <- NULL
# 
# # add new column based on yyyy_mo, split into yyyy mo, concatinate yyyy, mo, and day
# # into a date column, remove extraneous columns, and re-order
# mnp.m4[, "year_month" := yyyy_mo]
# mnp.m5 <- cSplit(mnp.m4, "year_month", sep = "_", direction = "wide", fixed = TRUE,
#                  drop = TRUE, stripWhite = TRUE, makeEqual = NULL, type.convert = TRUE)
# mnp.m5$date <- with(mnp.m5, ymd(sprintf('%04d%02d%02d', year_month_1, year_month_2, day)))
# # mnp.m5[ , c("year_month_1", "year_month_2") := NULL]
# names(mnp.m5)[18] <- "year"
# names(mnp.m5)[19] <- "month"
# mnp.m5$owner <- mnp.m5$owner %>% na.omit()
# mnp.m6 <- unite(mnp.m5, "station_owner", station, owner, sep = " ")
# mnp.m7 <- mnp.m6[, c(10, 19, 17:18, 8, 16, 1:2, 5:6, 9, 11)]
# 
# # write unique stations to an object with "distinct"
# mnp.m8 <- distinct(mnp.m7, latitude, longitude, .keep_all = TRUE)

# change longitude sign

