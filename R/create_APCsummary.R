
#' Create a summary table for multiple estimated GAM models
#' 
#' Create a table to summarize the overall effect strengths of the age, period
#' and cohort effects for models fitted with \code{\link[mgcv]{gam}}. The output
#' format can be adjusted by passing arguments to \code{\link[knitr]{kable}} via
#' the \code{...} argument.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @inheritParams plot_jointMarginalAPCEffects
#' @param digits Number of digits for numeric columns. Defaults to 2.
#' @param ... Optional additional arguments passed to \code{\link[knitr]{kable}}.
#' 
#' @import checkmate dplyr
#' @importFrom knitr kable
#' @export
#' 
create_APCsummary <- function(model_list, dat, digits = 2, ...) {
  
  checkmate::check_list(model_list, types = "gam")
  checkmate::check_data_frame(dat)
  checkmate::check_number(digits, lower = 0)
  
  
  # retrieve model labels
  if (!is.null(names(model_list))) {
    model_labels <- names(model_list)
  } else {
    model_labels <- paste("model", 1:length(model_list))
  }
  
  # create the summary table
  tab <- lapply(1:length(model_list), function(i) {
    
    create_oneAPCsummaryTable(model_list[[i]], dat) %>% 
      mutate(model = model_labels[i]) %>% 
      select(model, everything())
    
  }) %>% dplyr::bind_rows()
  
  
  return(knitr::kable(tab, digits = digits, ...))
}


#' Internal helper to create a summary table for one estimated GAM model
#' 
#' Internal helper function to be called in \code{\link{create_APCsummary}}.
#' This function creates the summary table for one model estimated with
#' \code{\link[mgcv]{gam}}.
#' 
#' @inheritParams plot_APCheatmap
#' @return \code{data.frame} containing aggregated information on the
#' individual effects.
#' 
#' @import checkmate dplyr
#' 
create_oneAPCsummaryTable <- function(model, dat) {
  
  checkmate::check_class(model, classes = "gam")
  checkmate::check_data_frame(dat)
  
  # retrieve datasets with the marginal effects
  dat_list <- plot_marginalAPCeffects(model, dat, return_plotData = TRUE)
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  
  vars <- c("age","period","cohort")
  
  summary_tab <- lapply(vars, function(var) {
    dat_var <- dat_list[[paste0("dat_",var)]]
    tab <- data.frame(effect              = var,
                      value_withMaxEffect = dat_var$value[which.max(dat_var$effect)],
                      value_withMinEffect = dat_var$value[which.min(dat_var$effect)],
                      max_effect          = max(dat_var$effect),
                      min_effect          = min(dat_var$effect))
    
    if (!used_logLink) { # identity link
      tab <- tab %>% mutate(difference = max_effect - min_effect)
    } else { # log or logit link
      tab <- tab %>% mutate(ratio = max_effect / min_effect)
    }
    
    return(tab)
  }) %>% dplyr::bind_rows()
  
  return(summary_tab)
}
