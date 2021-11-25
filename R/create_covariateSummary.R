
#' Create covariate summary tables for multiple estimated GAM models
#' 
#' Create publication-ready summary tables of all linear and nonlinear effects
#' for models fitted with \code{\link[mgcv]{gam}}. The output format of the
#' tables can be adjusted by passing arguments to \code{\link[knitr]{kable}} via
#' the \code{...} argument.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effects.
#' 
#' @inheritParams extract_summary_linearEffects
#' 
#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' @export
#' 
create_covariateSummary <- function(model_list, digits = 2, ...) {
  
  checkmate::check_list(model_list, types = "gam")
  checkmate::check_number(digits, lower = 0)
  
  
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
    
    tab <- mgcv::summary.gam(model_list[[i]])$s.table %>% 
      as.data.frame() %>% 
      mutate(param = row.names(.)) %>% 
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
#' effects summary of a model fitted with \code{\link[mgcv]{gam}}.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @param model Model fitted with \code{\link[mgcv]{gam}}.
#' 
#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' 
extract_summary_linearEffects <- function(model) {
  
  checkmate::check_class(model, classes = "gam")
  
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  
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
    # confidence intervals on exp scale are computed based on delta method
    dat <- dat %>%
      mutate(coef_exp = exp(coef),
             se_exp = sqrt(se^2 * exp(coef)^2)) %>%
      mutate(CI_lower_exp = coef_exp - qnorm(0.975) * se_exp,
             CI_upper_exp = coef_exp + qnorm(0.975) * se_exp) %>%
      select(param, coef, se, CI_lower, CI_upper,
             coef_exp, se_exp, CI_lower_exp, CI_upper_exp, pvalue)
  }
  
  return(dat)
}
