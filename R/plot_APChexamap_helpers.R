
#' Internal helper to tilt the x-axis for the hexamap plot
#' 
#' Internal helper function to be called in \code{\link{plot_APChexamap}},
#' to tilt the x-axis for the hexamap plot.
#' 
#' @param period_vec Numeric vector of period values.
#' 
compute_xCoordinate <- function(period_vec) {
  x <- period_vec * sqrt(3) / 2
  return(x)
}



#' Internal helper to tilt the x-axis for the hexamap plot
#' 
#' Internal helper function to be called in \code{\link{plot_APChexamap}},
#' to tilt the x-axis for the hexamap plot.
#' 
#' @param period_vec Numeric vector of period values.
#' @param age_vec Numeric vector of age values.
#' 
compute_yCoordinate <- function(period_vec, age_vec){
  y <- age_vec - period_vec / 2
  return(y)
}
