###########################################################################
##### script for data filling KRW 10/23/20114
##### read in raws fire station monitoring data
##### fill missing climate data
###########################################################################

# paths to data
# path1<-"/Volumes/disk7/b4warmed3/query_output/60min/derived60min_v2009-2011.csv"
path1 <- "/Volumes/disk7/b4warmed3/export/primary2013/derived60v2_2013_complete.csv"
path2 <- "/Volumes/disk7/b4warmed3/sup_climate_raws_2008-2014.dat"
# write path
path3 <- "/Volumes/disk7/b4warmed3/export/primary2013/derived60_2012-2013_finalv1.csv" 
# path3 <- "/Volumes/disk7/b4warmed3/export/primary2013/derived60_2009-2011_finalv1.csv" 


# read data
dt1 <- fread(path1)
raws <- fread(path2)

# rename column, used plyr()
# raws <- rename(raws, c("time5" = "time2"))
setnames(raws, "time5", "time2")
# set keys
setkey(dt1, time2, doy, hour, year, site)
setkey(raws, time2, doy, hour, year, site)

# join raws to dt1
raws <- unique(raws) # filter for unique records
dt1_raws <- raws[dt1] # dt1_raws <- merge(dt1, raws2, all.x = TRUE, by = c("time2", "site"))

# # diagnositics
# sum(is.na(dt1$dd_rh2))
# sum(is.na(raws$sup_rh2))
# sum(is.na(dt1_raws$dd_rh2))
# sum(is.na(dt1_raws$sup_rh2))

# fill new column filled_rh with dd_rh
#dt1_raws$filled_rh2 <- dt1_raws$dd_rh2 # create new column based on dd_rh2
#dt1_raws$filled_rh2[is.na(dt1_raws$filled_rh2)] <- dt1_raws$sup_rh2[is.na(dt1_raws$filled_rh2)] # fill dd_rh2 na's with values from sup_rh2

dt1_raws[dd_rh2==0,dd_rh2:= NA] # replace zeros with NA
dt1_raws[,filled_rh2:=ifelse(is.na(dd_rh2), sup_rh2, dd_rh2)]
dt1_raws[, dd_airtc:= ifelse(dd_airtc < -50, NA, dd_airtc)]
dt1_raws[, sup_airtc:= ifelse(sup_airtc < -50, NA, sup_airtc)]


dt1_raws[,filled_airtc:=ifelse(is.na(dd_airtc), sup_airtc, dd_airtc)]

###calculate vpds...
#### Calculate vapor pressure deficit, from Microclimate 2nd ed. Rossenburg (pg. 170)
#dt[, dd_rh2:= ifelse(dd_rh>100, 100, dd_rh)]
#dt[, dd_rh2:= ifelse(dd_rh2<0,0, dd_rh2)]

dt1_raws[,filled_es:= 0.61078 * exp(17.269 * filled_airtc / (filled_airtc + 237.3))] ##  saturated vapor pressure in kilopascals (kPa)
dt1_raws[,filled_ea:= filled_rh2 / 100 * filled_es] ## vapor pressure air in kilopascals (kPa)     
dt1_raws[,filled_vpd:= filled_es - filled_ea] ## vapor pressure deficit in kilopascals (kPa)

# write out table 
write.table(dt1_raws, path3, append = FALSE, quote = FALSE, sep = ",", 
            na = "NA", dec = ".", row.names = FALSE, 
            col.names = TRUE, qmethod = c("escape", "double"))

# create histograms
ggplot(as.factor(dt1_raws$dd_rh2) + geom_density(alpha=.3))
