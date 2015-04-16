##### load required libraries
library(data.table)
library(lubridate)
library(reshape2)
library(plyr)

##### read in hourly data
hourlyTable <- fread("/Volumes/disk7/b4warmed3/user_output/user_2012-2013_hourly.csv")

##### construct vectors for subsetting and aggrigating, and build a function to calculated means without NAs
cols <- c("site","block","plot","canopy","power","year","month","week","doy","hour","treatment_abbr","tabovefill_delta",
          "tabovefill","filled_airtc","airtemp_avg")
aggr <- c("site","block","plot","canopy","power","year","month","week","doy","treatment_abbr")
meanrm <- function(x){mean(x, na.rm=TRUE)} ## calculate means without NAs

##### subset the full table to just the needed columns and then aggriagte those data to daily
subHourlyTable <- subset(hourlyTable, select=cols) ## subset the data to the columns in "cols" vector
is.na(subHourlyTable) <- is.na(subHourlyTable) ## make sure that NaNs are converted to NAs
aggrDailyTable <- subHourlyTable[,lapply(.SD,meanrm), by=aggr] ## aggrigate data in "subHourlyTable" by the columns in "aggr" vector
is.na(aggrDailyTable) <- is.na(aggrDailyTable) ## make sure that NaNs are converted to NAs

##### create new variable "airtemp_comb" and fill with best available measured data
aggrDailyTable$airtemp_fill <- ifelse(x <- is.na(aggrDailyTable$filled_airtc), aggrDailyTable$airtemp_avg, aggrDailyTable$filled_airtc) ## fill missing airtc with airtemp_ave
aggrDailyTable$airtemp_comb <- aggrDailyTable$tabovefill ## create combined variable from "tabovefill"
aggrDailyTable$airtemp_comb <- ifelse(x <- aggrDailyTable$power < 1, aggrDailyTable$airtemp_fill, aggrDailyTable$tabovefill) ## fill winter values with filled airtc data

##### create site by canopy by treatment daily means for filling plots
aggr2 <- c("site","canopy","year","doy","treatment_abbr")
aggrDailyTable[,sc_airtemp_comb:= meanrm(airtemp_comb), by=aggr2] ## add mean by site by canopy by the columns in "aggr2" vector

##### create final filled variable 
aggrDailyTable$airtemp_comb_final <- ifelse(x <- is.na(aggrDailyTable$airtemp_comb),aggrDailyTable$sc_airtemp_comb,aggrDailyTable$airtemp_comb)   ### final variable

##### make complete doy,year, plot dataset and merge complete records with aggrDailyTable. Bug... creates a doy 366 in years with 365 days
a <- unique(aggrDailyTable$doy)
b <- unique(aggrDailyTable$plot)
c <- unique(aggrDailyTable$year)
d <- merge(a, b,  allow.cartesian=TRUE)
names(d)
e <- merge(c, b,  allow.cartesian=TRUE)
names(e)
f <- merge(d,e, by= "y", allow.cartesian=TRUE)
f <- as.data.table(f)
g <- subset(f,y!="NA"& x.x!="NA"& x.y!="NA")
gcol <- c("plot","doy","year")
setnames(g,gcol)
setkey(g,"plot","doy","year")
setkey(aggrDailyTable,"plot","doy","year")
aggrDailyTable2 <- merge(g, aggrDailyTable, by=gcol, all.x= TRUE,  allow.cartesian=TRUE)
is.na(aggrDailyTable2) <- is.na(aggrDailyTable2) ## make sure that NaNs are converted to NAs
aggrDailyTable3 <- subset(aggrDailyTable2, site != "NA") ## removes extra lines on doy 366 for years that have 365 days
aggrDailyTable4 <- subset(aggrDailyTable3, year != 2008) ## trim dqta to years 2009-2012

##### aggrigate to block level by treatment
aggr3 <- c("site","canopy","power","year","doy","treatment_abbr")
aggrTreatment <- aggrDailyTable4[,lapply(.SD,meanrm), by=aggr3] ## aggrigate data in "Daily" table to block level by treatment
is.na(aggrTreatment) <- is.na(aggrTreatment) ## make sure that NaNs are converted to NAs

##### read in daily temperature means from near by stations and create new column by merging
ambDailyTemps <- fread("~/Google Drive/b4w/nearbyDailyTempMeans.csv")
adt <- ambDailyTemps[year < 2013 & year > 2008]
cols2 <- c("site","year","doy")
cols3 <- c("site", "year", "doy","temp")
adt <- subset(adt, select = cols3) ## get rid of nCols

setkeyv(aggrTreatment,cols2)
setkeyv(adt,cols2)

dt <- adt[aggrTreatment] ## join the data.tables

##### final fill of ambient means from nearby stations for power off time periods 
dt$airtemp_comb_nearby <- ifelse(x <- is.na(dt$airtemp_comb_final) & dt$power < 1, 
                                 dt$temp, dt$airtemp_comb_final) ## create final column with fill of missing winter values with nearby station data
is.na(dt) <- is.na(dt) ## make sure that NaNs are converted to NAs

###### set GDD base temperature (usually 10 °C), and tmax (usually 30 °C)
tbase0 <- 0
tbase2 <- 2
tbase4 <- 4
tbase6 <- 6
tbase8 <- 8
tbase10 <- 10
tmax <- 30

##### gdd helper functions
adjust_for_tmax <- function(x) ifelse(x > tmax, tmax, x) ## any temperature above tmax is set to tmax
adjust_for_tbase0 <- function(x) ifelse(x - tbase0 < 0, 0, x - tbase0) ## any temperature below tbase is set to tbase
adjust_for_tbase2 <- function(x) ifelse(x - tbase2 < 0, 0, x - tbase2) ## any temperature below tbase is set to tbase
adjust_for_tbase4 <- function(x) ifelse(x - tbase4 < 0, 0, x - tbase4) ## any temperature below tbase is set to tbase
adjust_for_tbase6 <- function(x) ifelse(x - tbase6 < 0, 0, x - tbase6) ## any temperature below tbase is set to tbase
adjust_for_tbase8 <- function(x) ifelse(x - tbase8 < 0, 0, x - tbase8) ## any temperature below tbase is set to tbase
adjust_for_tbase10 <- function(x) ifelse(x - tbase10 < 0, 0, x - tbase10) ## any temperature below tbase is set to tbase
# colCumSums <- function(x) { 
#   for(i in seq_len(dim(x)[2])) { x[,i] <- cumsum(x[,i]) }; x
# }

##### apply 'adjust_for_tmax' function
dt$adj_temp <- adjust_for_tmax(dt$airtemp_comb_nearby)

##### create new variables and calculate gdd temps
dt$gdd0 <- adjust_for_tbase0(dt$adj_temp)
dt$gdd2 <- adjust_for_tbase2(dt$adj_temp)
dt$gdd4 <- adjust_for_tbase4(dt$adj_temp)
dt$gdd6 <- adjust_for_tbase6(dt$adj_temp)
dt$gdd8 <- adjust_for_tbase8(dt$adj_temp)
dt$gdd10 <- adjust_for_tbase10(dt$adj_temp)

##### create new variables and calculate gddsums
dt[order(doy), gddsum0:=cumsum(gdd0), by=c("year", "site", "canopy", "treatment_abbr")]
dt[order(doy), gddsum2:=cumsum(gdd2), by=c("year", "site", "canopy", "treatment_abbr")]
dt[order(doy), gddsum4:=cumsum(gdd4), by=c("year", "site", "canopy", "treatment_abbr")]
dt[order(doy), gddsum6:=cumsum(gdd6), by=c("year", "site", "canopy", "treatment_abbr")]
dt[order(doy), gddsum8:=cumsum(gdd8), by=c("year", "site", "canopy", "treatment_abbr")]
dt[order(doy), gddsum10:=cumsum(gdd10), by=c("year", "site", "canopy", "treatment_abbr")]

#####################################################################
## Views and visualization
#####################################################################
View(dt[order(doy) & year == 2012 & site == "cfc" & treatment_abbr == "h2" & canopy == "closed"]) 

library(ggplot2)
p <- ggplot(dt, aes(x=doy, y=gddsum10, colour=interaction(canopy,treatment_abbr), group=interaction(canopy,treatment_abbr))) + 
  geom_line() + 
  theme_bw() + ## change from default gray theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ## hides horizontal and vertical grid lines
  xlim(100,200) + 
  xlab("Day of Year") +
  ylim(0,800) +
  ylab("Growing Degree Day Sum, Tbase = 10°C") +
  facet_grid(year ~ site)

p # view the graphic "p"










