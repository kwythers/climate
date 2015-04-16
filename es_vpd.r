

get.es <- function(temp){
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp)))
  return(es)
}

get.vpd <- function(rh, temp){
  ## calculate saturation vapor pressure
  es <- get.es(temp)
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}


temp <- -30:30
plot(temp, get.es(temp), type = "l", xlab = "T", ylab = "es or vpd")
lines(temp, get.vpd(50, temp), col = "red")