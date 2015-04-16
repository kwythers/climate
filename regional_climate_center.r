# load libraries
library(lubridate)
library(data.table)
library(xts)
library(ggplot2)

# read in data from csv file
dallas_airport <- fread("~/Desktop/dallas.csv", header = T, sep = ",")
phoenix_airport <- fread("~/Desktop/phoenix.csv", header = T, sep = ",")
msp <- fread("~/Desktop/msp.csv")

# convert obs time to a date
dallas_airport$DATE <- ymd(dallas_airport$DATE)
phoenix_airport$DATE <- ymd(phoenix_airport$DATE)
msp$Date <- ymd(msp$Date)

# subset data to a single site
dallas <- subset(dallas_airport, STATION_NAME == 'DALLAS FAA AIRPORT TX US')
phoenix <- subset(phoenix_airport, STATION_NAME == 'PHOENIX SKY HARBOR INTERNATIONAL AIRPORT AZ US')

# sort the data by max temp
dallas_sorted <- dallas[order(-TMAX)]
phoenix_sorted <- phoenix[order(-TMAX)]
msp_sorted <- msp[order(-TMAX)]

dallas_top20 <- head(dallas_sorted, 20)
phoenix_top20 <- head(phoenix_sorted, 20)

# histogram overlaid with kernel density curve
ggplot(dallas_top20, aes(x = DATE)) + 
  geom_histogram(aes(y = ..density..),      # histogram with density instead of count on y-axis
                 binwidth = 2,              # set bin width
                 color = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")  # overlay with transparent density plot

# export data
write.table(dallas_top20, "~/Desktop/dallas_top20.txt", sep = ",")
write.table(phoenix_top20, "~/Desktop/phoenix_top20.txt", sep = ",")