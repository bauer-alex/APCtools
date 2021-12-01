
#' Fit APC model without bivariate tensorproduct spline
#' 
#' @param APC_formula model formula
#' @param data data
#' @param ...
#' 
#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' @export
#' 
fit_APC_model <- function(APC_formula, data, ...) {
  
  checkmate::assert_formula(APC_formula)

  # Estimate model:
  model <- gam(formula = APC_formula, data = data, ...)
  
  # Return APC model object:
  class(model) <- c(class(model), "apc")
  return(model)
}