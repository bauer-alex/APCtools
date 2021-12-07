
#' Fit APC model without bivariate tensorproduct spline
#' 
#' @param APC_formula model formula
#' @param data data
#' @param ... additional arguments to \code{\link[mgcv]{gam}}
#' 
#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' @export
#' 
fit_APC_model <- function(APC_formula, data, ...) {
  
  checkmate::assert_formula(APC_formula)

  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  gam <- NULL
  
  
  # Estimate model:
  model <- gam(formula = APC_formula, data = data, ...)
  
  # Return APC model object:
  class(model) <- c(class(model), "apc")
  return(model)
}