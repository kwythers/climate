#Vapor pressure deficit calculation using Teten's formula from Hatfield & Baker, Micrometeorology in Agricultural Systems, 2005, chapter 23 using parameters from Buck 1981.
#units=kPa
#'Tair' is air temperature in deg C
#'RH' is relative humidity in %
#'es' is saturation vapor pressure
#'e' is actual vapor pressure
vpd_calc<-function(tair,rh) {
	a <- 0.61121
	b <- 17.502
	c <- 240.97
	es <- a * exp((b * tair) / (tair + c))
	e <- RH / 100 * es
	vpd <- es - e
	return(vpd)
	}
