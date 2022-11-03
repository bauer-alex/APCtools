
#' Create model summary tables for multiple estimated GAM models
#' 
#' Create publication-ready summary tables of all linear and nonlinear effects
#' for models fitted with \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' The output format of the tables can be adjusted by passing arguments to
#' \code{\link[knitr]{kable}} via the \code{...} argument.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effects.
#' 
#' The table for linear coefficients includes the estimated coefficient
#' (\code{coef}), the corresponding standard error (\code{se}), lower and upper
#' limits of 95% confidence intervals (\code{CI_lower}, \code{CI_upper}) and
#' the p-values for all coefficients apart from the intercept.
#' 
#' The table for nonlinear coefficients include the estimated degrees of freedom
#' (\code{edf}) and the p-value for each estimate.
#' 
#' @param model_list list of APC models
#' @param digits number of displayed digits
#' @param ... additional arguments to \code{\link[knitr]{kable}}
#' 
#' @return List of tables created with \code{\link[knitr]{kable}}.
#' 
#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' @export
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' library(mgcv)
#' 
#' data(travel)
#' model <- gam(mainTrip_distance ~ te(age, period) + residence_region +
#'              household_size + s(household_income), data = travel)
#' 
#' create_modelSummary(list(model), dat = travel)
#' 
create_modelSummary <- function(model_list, digits = 2, ...) {
  
  checkmate::assert_list(model_list, types = "gam")
  checkmate::assert_number(digits, lower = 0)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  model <- param <- edf <- pvalue <- NULL
  
  
  # retrieve model labels
  if (!is.null(names(model_list))) {
    model_labels <- names(model_list)
  } else {
    model_labels <- paste("model", 1:length(model_list))
  }
  
  # create the summary table for all linear effects
  tab_linear <- lapply(1:length(model_list), function(i) {
    
    extract_summary_linearEffects(model_list[[i]]) %>% 
      mutate(model = model_labels[i]) %>% 
      select(model, everything()) %>% 
      mutate(pvalue = case_when(param == "(Intercept)" ~ "-",
                                pvalue < 0.0001        ~ "<.0001",
                                TRUE                   ~ as.character(round(pvalue, 4))))
    
  }) %>% dplyr::bind_rows()
  
  # create the summary table for all nonlinear effects
  tab_nonlinear <- lapply(1:length(model_list), function(i) {
    
    tab_raw <- mgcv::summary.gam(model_list[[i]])$s.table %>% 
      as.data.frame()
    
    tab <- tab_raw %>% 
      mutate(param = row.names(tab_raw)) %>% 
      dplyr::rename(pvalue = "p-value") %>% 
      mutate(model = model_labels[i]) %>% 
      select(model, param, edf, pvalue) %>%
      mutate(pvalue = case_when(pvalue < 0.0001 ~ "<.0001",
                                TRUE            ~ as.character(round(pvalue, 4))))
    row.names(tab) <- NULL
    return(tab)
    
  }) %>% dplyr::bind_rows()
  
  
  tab_list <- list(knitr::kable(tab_linear,    digits = digits, ...),
                   knitr::kable(tab_nonlinear, digits = digits, ...))
  
  return(tab_list)
}



#' Internal helper to extract summary of linear effects in a gam model
#' 
#' Internal helper function to create a \code{data.frame} containing the linear
#' effects summary of a model fitted with \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect,
#' see argument \code{method_expTransform}.
#' 
#' @param model Model fitted with \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' @param method_expTransform One of \code{c("simple","delta")}, stating if
#' standard errors and confidence interval limits should be transformed by
#' a simple exp transformation or using the delta method. The delta method can
#' be unstable in situations and lead to negative confidence interval limits.
#' Only used when the model was estimated with a log or logit link.
#' 
#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' 
extract_summary_linearEffects <- function(model, method_expTransform = "simple") {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_choice(method_expTransform, choices = c("simple","delta"))
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  param <- coef <- se <- coef_exp <- se_exp <- CI_lower <- CI_upper <-
    CI_lower_exp <- CI_upper_exp <- pvalue <- NULL
  
  
  used_logLink <- (model$family[[2]] %in% c("log","logit")) |
    grepl("Ordered Categorical", model$family[[1]])
  
  x <- mgcv::summary.gam(model)$p.table
  dat <- data.frame(param    = row.names(x),
                    coef     = unname(x[,1]),
                    se       = unname(x[,2]),
                    CI_lower = unname(x[,1] - qnorm(0.975) * x[,2]),
                    CI_upper = unname(x[,1] + qnorm(0.975) * x[,2]),
                    pvalue   = unname(x[,4]),
                    stringsAsFactors = FALSE) %>%
    mutate(param = factor(param, levels = row.names(x)))
  
  if (used_logLink) {
    
    if (method_expTransform == "simple") {
      dat <- dat %>%
        mutate(coef_exp     = exp(coef),
               se_exp       = exp(se),
               CI_lower_exp = exp(CI_lower),
               CI_upper_exp = exp(CI_upper)) %>%
        select(param, coef, se, CI_lower, CI_upper,
               coef_exp, se_exp, CI_lower_exp, CI_upper_exp, pvalue)
      
    } else { # method_expTransform == "delta"
      # confidence intervals on exp scale are computed based on delta method
      dat <- dat %>%
        mutate(coef_exp = exp(coef),
               se_exp = sqrt(se^2 * exp(coef)^2)) %>%
        mutate(CI_lower_exp = coef_exp - qnorm(0.975) * se_exp,
               CI_upper_exp = coef_exp + qnorm(0.975) * se_exp) %>%
        select(param, coef, se, CI_lower, CI_upper,
               coef_exp, se_exp, CI_lower_exp, CI_upper_exp, pvalue)
    }
  }
  
  return(dat)
}
