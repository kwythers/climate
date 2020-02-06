##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Long term long term weather observation extractor
##### Kirk R. Wythers 20160909. Pulls long term
##### observational records of station weather data for               
##### climate analysis. Uses "weatheR" R package. Based
##### on concepts from Cohen, Piccirelli and McCreight 2015.
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

##### install weatheR library (beta version from github)
require(devtools)
install_github("mpiccirilli/weatheR")
require(weatheR)

allStations <- function()
{
  # full list of ISD weather stations:
  isd <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  response <- suppressWarnings(GET(isd))
  all <- read.csv(text=content(response, "text"), header = TRUE)
  colnames(all)[c(3, 9)] <- c("NAME", "ELEV") # renaming for consistency
  all <- all[!is.na(all$LAT) & !is.na(all$LON),] # removing stations without LAT/LON data
  # all <- all[State == "MN",] # Keep MN stations
  return(all)
}
stations <- allStations() # assigns our subsetted list of ISD stations to variable 'stations'
head(stations)

cities.of.interest <- c("St Paul, MN", "Willmar, MN", "Sauk Center, MN", "Mora, MN")

deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # earth mean radius in km
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # distance in km
}

kNStations <- function(city.list, station.list, k = 5)
{
  # geocodes of each city
  coords <- suppressMessages(geocode(city.list))
  
  # loop through each location, finding the k-nearest stations from the main list:
  kns.ix <- NULL # create a variable to track row index the stations
  kns.dist <- NULL # create a distance variable
  for(i in 1:length(city.list))
  {
    # great cirle distance calculator:
    dist <- gcd.slc(coords$lon[i], coords$lat[i], station.list$LON, station.list$LAT)
    distSort <- sort(dist, ind=TRUE)
    tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
    tmp.dist <- distSort$x[1:k] # temporary distance variable for the loop
    kns.ix <- c(kns.ix, tmp.ix) # append row index of stations for each location
    kns.dist <- c(kns.dist, tmp.dist) # append distances of stations for each location
  }
  st <- station.list[kns.ix,] # subset the full list with k-nearest stations
  st$city <- rep(city.list, each=k) # Insert reference City
  st$Ref_Lat <- rep(coords$lat,each=k) # Insert reference latitude
  st$Ref_Lon <- rep(coords$lon, each=k) # Insert reference longitude
  
  st$kilo_distance <- kns.dist # insert distance into result
  st <- st[with(st,order(city, kilo_distance)),]
  st$rank <- rep(1:k,length(city.list)) # rank closest to farthest (1 to k)
  
  # queries are made to NOAA database by year:
  st$BEGIN_Year <- as.numeric(substr(st$BEGIN,1,4)) # Start Year
  st$END_Year <- as.numeric(substr(st$END, 1, 4)) # End Year
  return(st)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL)
{
  # make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # if layout is NULL, then use 'cols' to determine layout
  if (is.null(layout))
  {
    # make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1){
    print(plots[[1]])
  } else {
    # set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # make each plot, in the correct location
    for (i in 1:numPlots) {
      # i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##### function to plot the stations for each city
##### inputs here are the list of city, which gets passed into the kNStations
##### function we built above, full list of NOAA ISD weather stations, and the
##### k-number of stations you wish to return
##### the output will be a map for each city, with one black dot and up
##### to k red dots (numbered)
##### the black dot is the reference point for that city and the red dots
##### represent the weather stations, labeled 1 to k, ordered from closest to
##### farthest away from the reference point
plotStations <- function(city.list, station.list, k)
{
  kns <- kNStations(city.list, station.list, k)
  nc <- length(city.list)
  plots <- list()
  for (i in 1:nc)
  {
    map <- suppressMessages(get_map(location = city.list[i], zoom = 10))
    p1 <- suppressMessages(ggmap(map) +
                             geom_point(aes(x = LON, y = LAT),
                                        data = kns[kns$city==city.list[i],],
                                        colour="red", size=7, alpha=.5) +
                             geom_text(aes(x = LON, y = LAT, label=rank),
                                       data = kns[kns$city==city.list[i],]) +
                             geom_point(aes(x = lon, y = lat),
                                        data = geocode(city.list[i]),
                                        colour="black", size=7, alpha=.5)) +
      labs(title=city.list[i]) + theme(plot.margin=unit(c(0,0,0,0),"mm"))
    plots[[i]] <- p1
  }
  if (nc == 1) plot(p1) else multiplot(plotlist = plots, cols = round(sqrt(nc)))
}

# run the function:
plotStations(cities.of.interest[1:4], stations, 7)

##### data are fixed width, and saved on the ftp in .gz format the widths and
##### column names of the station data is as follows:
col.width <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1,
               1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)

col.names <- c("CHARS", "USAFID", "WBAN", "YR", "M", "D", "HR", "MIN",
               "DATE.FLAG", "LAT", "LONG", "TYPE.CODE", "ELEV", "CALL.LETTER",
               "QLTY", "WIND.DIR", "WIND.DIR.QLTY", "WIND.CODE",
               "WIND.SPD", "WIND.SPD.QLTY", "CEILING.HEIGHT", "CEILING.HEIGHT.QLTY",
               "CEILING.HEIGHT.DETERM", "CEILING.HEIGHT.CAVOK", "VIS.DISTANCE",
               "VIS.DISTANCE.QLTY", "VIS.CODE", "VIS.CODE.QLTY",
               "TEMP", "TEMP.QLTY", "DEW.POINT", "DEW.POINT.QLTY",
               "ATM.PRES", "ATM.PRES.QLTY")

##### function to download data for a range of years for each of the k-nearest stations
##### to each city that is found in the kNStations function. The function will have 3 inputs:
##### the result of the kNStations function, a beginning year, and an ending year
##### (both in 4-digit format)
dlStationData <- function(kns, beg, end)
{
  base.url <- "ftp:// "
  nstations <- nrow(kns)
  yrs <- seq(beg,end,1)
  nyrs <- end-beg+1
  usaf <- as.numeric(kns$USAF)
  wban <- as.numeric(kns$WBAN)
  # the following dataframe will be printed to show
  # which files were successfully downloaded
  status <- data.frame()
  temp <- as.data.frame(matrix(NA,nstations,5))
  names(temp) <- c("File","Status", "City", "rank", "kilo_distance")
  temp$City <- kns$city
  temp$rank <- kns$rank
  temp$kilo_distance <- kns$kilo_distance
  # setup for the list of data
  temp.list <- df.list <- list()
  city.names <- unlist(lapply(strsplit(kns$city,", "), function(x) x[1])) # City Name
  df.names <- paste(city.names, kns$USAF, sep="_") # City Name_USAF#
  
  # download the desired stations into a list (does not save to disk)
  for (i in 1:nyrs)
  {
    for (j in 1:nstations)
    {
      # create file name
      temp[j,1] <- paste(usaf[j],"-",wban[j],"-", yrs[i], ".gz", sep = "")
      tryCatch({
        # create connect to the .gz file
        gz.url <- paste(base.url, yrs[i], "/", temp[j, 1], sep="")
        con <- gzcon(url(gz.url))
        raw <- textConnection(readLines(con))
        # read the .gz file directly into R without saving to disk
        temp.list[[j]] <- read.fwf(raw, col.width)
        close(con)
        # housekeeping
        names(temp.list)[j] <- df.names[j]
        names(temp.list[[j]]) <- col.names
        temp.list[[j]]$LAT <- temp.list[[j]]$LAT/1000
        temp.list[[j]]$LONG <- temp.list[[j]]$LONG/1000
        temp.list[[j]]$WIND.SPD <- temp.list[[j]]$WIND.SPD/10
        temp.list[[j]]$TEMP <- temp.list[[j]]$TEMP/10
        temp.list[[j]]$DEW.POINT <- temp.list[[j]]$DEW.POINT/10
        temp.list[[j]]$ATM.PRES <- temp.list[[j]]$ATM.PRES/10
        temp.list[[j]]$city <- city.names[j]
        temp.list[[j]]$distance <- kns$kilo_distance[j]
        temp.list[[j]]$rank <- kns$rank[j]
        temp[j,2] <- "Success"
      },
      error=function(cond)
      {
        return(NA)
        next
      },
      finally={ # if any of the files didn't download successfully, label as such
        if(is.na(temp[j,2])=="TRUE") temp[j,2] <- "Failed"
      })
    }
    # combine each year's status and list
    status <- rbind(status, temp)
    status <- status[order(status[,3], status[,4], status[,1]),]
    df.list <- append(df.list, temp.list)
  }
  output.list <- list(status, df.list)
  names(output.list) <- c("dl_status", "station_data")
  return(output.list)
}

##### reduce the number of dataframes we need to work with by combining the data
##### of each year for the same station.
combineWeatherDFs <- function(dfList)
{
  combined.list <- list()
  keys <- unique(names(dfList$station_data))
  keys <- keys[keys!=""]
  nkeys <- length(keys)
  for (i in 1:nkeys)
  {
    track <- which(names(dfList$station_data)==keys[i])
    combined.list[[i]] <- as.data.frame(rbindlist(dfList$station_data[track]))
    names(combined.list)[i] <- keys[i]
  }
  output.list <- list(dfList$dl_status, combined.list)
  names(output.list) <- c("dl_status", "station_data")
  return(output.list)
}

getStationsByCity <- function(city.list, station.list, k, begin, end)
{
  kns <- kNStations(city.list, station.list, k)
  weatherDFs <- dlStationData(kns, begin, end)
  combined.list <- combineWeatherDFs(weatherDFs)
  return(combined.list)
}

##### establish minimum standards for each dataset, evaluate each station's data,
##### then select station containing the best data.
filterStationData <- function(comb.list, distance, hourly_interval, tolerance, begin, end)
{
  
  dlStatus <- comb.list$dl_status
  comb.list <- comb.list$station_data
  
  city.names <- unlist(lapply(comb.list, function(x) unique(x$city)))
  
  # 1 remove stations with little to no data at all
  rm.junk <- names(comb.list[which(sapply(comb.list, function(x) dim(x)[1] <= 10))])
  comb.list <- comb.list[which(sapply(comb.list, function(x) dim(x)[1] > 10))]
  
  # 2 remove stations that exceed maximum distance
  rm.dist <- names(comb.list[which(sapply(comb.list, function(x) max(x["distance"])) > distance)])
  comb.list <- comb.list[which(sapply(comb.list, function(x) max(x["distance"])) < distance)]
  
  # keep track of which stations have been removed
  rm.tmp <- unique(c(rm.junk, rm.dist))
  
  lapply(comb.list, names)
  # 3a remove stations that exceed threshold of missing data,
  # start with counting the 999s:
  cl <- c("TEMP", "DEW.POINT") # Additional columns can be added
  ix <- ix.tmp <- NULL
  ix.ct <- as.data.frame(matrix(nrow=length(comb.list), ncol=length(cl)))
  colnames(ix.ct) <- cl
  rownames(ix.ct) <- names(comb.list)
  for (L in 1:length(comb.list))
  {
    for (i in 1:length(cl))
    {
      ix.tmp <- which(comb.list[[L]][cl[i]]==999.9 | comb.list[[L]][cl[i]]==999 |
                        comb.list[[L]][cl[i]]==99.99)
      ix.ct[L,i] <- length(ix.tmp)
      ix <- union(ix,ix.tmp)
    }
    comb.list[[L]] <- comb.list[[L]][-ix,]
  }
  ix.ct$temp_pct <- ix.ct[,cl[1]]/unlist(lapply(comb.list,nrow))
  ix.ct$dew_pct <- ix.ct[,cl[2]]/unlist(lapply(comb.list,nrow))
  # print(ix.ct)
  
  # ms.obs <- (ix.ct$TEMP)+(ix.ct$DEW.POINT)
  
  # 3b set a minimum number of observations and remove stations that do not
  # meet requirement
  yrs <- seq(begin,end,1)
  nyrs <- end-begin+1
  min.obs <- (24/hourly_interval)*365*nyrs*(1-tolerance)
  obs.ix <- which(sapply(comb.list, nrow) < min.obs)
  rm.obs <- names(comb.list[which(sapply(comb.list, nrow) < min.obs)])
  if(length(rm.obs)==0) comb.list <- comb.list else comb.list <- comb.list[-obs.ix]
  
  # update removed stations
  rm.all <- unique(c(rm.tmp, rm.obs))
  
  # 4 all current stations are assumed to be adequet,
  #   we therefore will take the closest to each reference point
  kept.names <- substr(names(comb.list),1,nchar(names(comb.list))-7)
  kept.ranks <- unname(unlist(lapply(comb.list, function(x) x["rank"][1,1])))
  f.df <- data.frame(location=kept.names, ranks=kept.ranks)
  kp.ix <- as.numeric(rownames(f.df[which(ave(f.df$ranks,f.df$location,FUN=function(x) x==min(x))==1),]))
  final.list <- comb.list[kp.ix]
  
  # show what was removed during the filtering process:
  kept <- names(comb.list)
  st.df <- data.frame(count(city.names))
  rm.df <- count(substr(rm.all, 1, nchar(rm.all)-7))
  kept.df <- count(substr(kept, 1, nchar(kept)-7))
  df.list <- list(st.df, rm.df, kept.df)
  mg.df <- Reduce(function(...) merge(..., by="x", all=T), df.list)
  suppressWarnings(mg.df[is.na(mg.df)] <- 0)
  colnames(mg.df) <- c("city", "stations", "removed", "kept")
  filterStatus <- mg.df
  
  # stations that will be in the final output:
  finalStations <- names(final.list)
  
  # create a list for output
  finalOutput <- list(dlStatus, filterStatus, finalStations, final.list)
  names(finalOutput) <- c("dl_status", "removed_rows", "station_names_final", "station_data")
  return(finalOutput)
}
##### interpolate all the missing values in each station's dataset
interpolateData <- function(wx.list)
{
  clean.list <- lapply(wx.list, function(x){
    ddply(x, .(city, USAFID, distance, rank, YR, M, D, HR), summarise,
          LAT=mean(LAT), LONG=mean(LONG), ELEV=mean(ELEV),
          TEMP=mean(TEMP), DEW.POINT=mean(DEW.POINT))})
  
  # create a column with the full posix date for each hour
  for (i in 1:length(clean.list))
  {
    clean.list[[i]]$dates <- as.POSIXct(paste(paste(clean.list[[i]]$YR,"-",clean.list[[i]]$M,
                                                    "-",clean.list[[i]]$D, " ",clean.list[[i]]$HR,sep=""),
                                              ":",0,":",0,sep=""),"%Y-%m-%d %H:%M:%S", tz="UTC")}
  
  # create a list of dataframes of each hour
  hourly.list <- list()
  for (i in 1:length(clean.list))
  {
    hourly.list[[i]] <- data.frame(hours=seq(
      from=as.POSIXct(paste(min(clean.list[[i]]$YR),"-1-1 0:00", sep=""), tz="UTC"),
      to=as.POSIXct(paste(max(clean.list[[i]]$YR),"-12-31 23:00", sep=""), tz="UTC"),
      by="hour"))
  }
  
  wx.df <- data.frame()
  for (i in 1:length(clean.list))
  {
    temp.df <- merge(hourly.list[[i]], clean.list[[i]], by.x="hours", by.y="dates", all.x=TRUE)
    temp.df$city <- unique(na.omit(temp.df$city))[1]
    temp.df$USAFID <- unique(na.omit(temp.df$USAFID))[1]
    temp.df$distance <- unique(na.omit(temp.df$distance))[1]
    temp.df$rank <- unique(na.omit(temp.df$rank))[1]
    temp.df$LAT <- unique(na.omit(temp.df$LAT))[1]
    temp.df$LONG <- unique(na.omit(temp.df$LONG))[1]
    temp.df$ELEV <- unique(na.omit(temp.df$ELEV))[1]
    temp.df$YR <- as.numeric(format(temp.df$hours,"%Y"))
    temp.df$M <- as.numeric(format(temp.df$hours,"%m"))
    temp.df$D <- as.numeric(format(temp.df$hours,"%d"))
    temp.df$HR <- as.numeric(format(temp.df$hours,"%H"))
    
    # interpolation
    temp.int <- approx(x=temp.df$hours, y=temp.df$TEMP, xout=temp.df$hours)
    temp.df$TEMP <- temp.int$y
    dew.int <- approx(x = temp.df$hours, y = temp.df$DEW.POINT, xout = temp.df$hours)
    temp.df$DEW.POINT <- dew.int$y
    
    # merge the dataframes together
    wx.df <- rbind(wx.df, temp.df)
  }
  return(wx.df)
}

##### function to feed data in the next step of analysis
getInterpolatedDataByCity <- function(city.list, station.list, k, begin, end, distance, hourly_interval, tolerance)
{
  kns <- kNStations(city.list, station.list, k)
  weatherDFs <- dlStationData(kns, begin, end)
  combined.list <- combineWeatherDFs(weatherDFs)
  filteredData <- filterStationData(combined.list, distance, hourly_interval, tolerance, begin, end)
  interpolation <- interpolateData(filteredData$station_data)
  return(interpolation)
}

# cities <- c("Eden Prairie, MN", "Hibbing, MN", "Nisswa, MN", "Canby, MN",
#                                   "Olivia, MN", "Glencoe, MN", "Blue Earth, MN", "Lanesboro, MN",
#                                   "St Peter, MN", "Willmar, MN", "Sauk Center, MN", "Mora, MN",
#                                   "Elbo Lake, MN", "Battle Lake, MN", "Park Rapids, MN", "Hallock, MN",
#                                   "Badger, MN", "Warroad, MN", "Littlefork, MN", "Soudan, MN")

# cities <- c(
#             "St Paul, MN", "Willmar, MN", "Sauk Center, MN", "Mora, MN",
#             "Elbo Lake, MN", "Battle Lake, MN", "Park Rapids, MN", "Hallock, MN"
#             )

cities <- c(
  "St Paul, MN"
)

z <- getInterpolatedDataByCity(cities, stations, 5, 2000, 2016, 50, 1, 0.1)
