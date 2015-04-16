###################################################
#####     Kirk R. Wythers 2014.09.12          #####
##### R sctipt to calculate vapor pressure    #####
##### deficits from temperaure and relative   #####
##### humidity                                #####
###################################################

# saturation pressure (es1) function, from Dennis Hartman "Global Physical Climatology" (p 350)
get.es1 <- function(temp){
  es1 <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp))) ## millibars (mb)
  return(es1)
}

get.vpd1 <- function(rh, temp){
  ## calculate saturation vapor pressure
  es1 <- get.es1(temp)
  ## calculate vapor pressure deficit
  vpd1 <- ((100 - rh) / 100) * es1
  return(vpd)
}

# saturation pressure (es2), from The ASCE Standardized Reference Evapotranspiration Equation
es2 <- 0.6108 * exp(17.27 * T / (T + 237.3)) ## kilopascals (kPa)
ea2 <- rh / 100 * es2 
vpd2 <- ea2 - es2
 
