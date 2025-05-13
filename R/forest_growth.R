#' @param time time since start
#' @param r early exponential growth rate
#' @param C size of the forest in kg/C
#' @param g linear growth rate in kg/year
#' @param K carrying capacity in units kg carbon
#' @param thresh canopy closure threshold


forest_growth <- function (time, C, parms){
  
  if (C < parms$thresh){
    dC <- parms$r * C
  } else {
    dC <- parms$g * (1 - C / parms$K)
  }
  
  return(list(dC))
}
