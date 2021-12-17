
#' Create a summary table for multiple estimated GAM models
#' 
#' Create a table to summarize the overall effect strengths of the age, period
#' and cohort effects for models fitted with \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}. The output format can be adjusted by passing
#' arguments to \code{\link[knitr]{kable}} via the \code{...} argument.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @inheritParams plot_jointMarginalAPCeffects
#' @param digits Number of digits for numeric columns. Defaults to 2.
#' @param apc_range Optional list with one or multiple elements with names
#' \code{"age","period","cohort"} to filter the data. Each element should
#' contain a numeric vector of values for the respective variable that should
#' be kept in the data. All other values are deleted before producing the table.
#' @param ... Optional additional arguments passed to \code{\link[knitr]{kable}}.
#' 
#' @return Table created with \code{\link[knitr]{kable}}.
#' 
#' @import checkmate dplyr
#' @importFrom knitr kable
#' @export
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' library(mgcv)
#' 
#' data(travel)
#' model_pure <- gam(mainTrip_distance ~ te(age, period), data = travel)
#' model_cov  <- gam(mainTrip_distance ~ te(age, period) + s(household_income),
#'                   data = travel)
#' 
#' model_list <- list("pure model"      = model_pure,
#'                    "covariate model" = model_cov)
#' 
#' create_APCsummary(model_list, dat = travel)
#' 
create_APCsummary <- function(model_list, dat, digits = 2, apc_range = NULL,
                              ...) {
  
  checkmate::assert_list(model_list, types = "gam")
  checkmate::assert_data_frame(dat)
  checkmate::assert_number(digits, lower = 0)
  checkmate::assert_list(apc_range, types = "numeric", max.len = 3,
                         null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_subset(names(apc_range), choices = c("age","period","cohort"))
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  model <- NULL
  
  
  # retrieve model labels
  if (!is.null(names(model_list))) {
    model_labels <- names(model_list)
  } else {
    model_labels <- paste("model", 1:length(model_list))
  }
  
  # create the summary table
  tab <- lapply(1:length(model_list), function(i) {
    
    create_oneAPCsummaryTable(model_list[[i]], dat, apc_range) %>% 
      mutate(model = model_labels[i]) %>% 
      select(model, everything())
    
  }) %>% dplyr::bind_rows()
  
  
  # remove the 'model' column if only one model is in the table
  if (length(model_list) == 1) {
    tab <- tab %>% select(-model)
  }
  
  
  return(knitr::kable(tab, digits = digits, ...))
}


#' Internal helper to create a summary table for one estimated GAM model
#' 
#' Internal helper function to be called in \code{\link{create_APCsummary}}.
#' This function creates the summary table for one model estimated with
#' \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' 
#' @inheritParams plot_APCheatmap
#' @inheritParams create_APCsummary
#' @return \code{data.frame} containing aggregated information on the
#' individual effects.
#' 
#' @import checkmate dplyr
#' 
create_oneAPCsummaryTable <- function(model, dat, apc_range = NULL) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_data_frame(dat)
  checkmate::assert_list(apc_range, types = "numeric", max.len = 3,
                         null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_subset(names(apc_range), choices = c("age","period","cohort"))
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  value <- max_effect <- min_effect <- NULL
  
  
  # retrieve datasets with the marginal effects
  dat_list <- plot_marginalAPCeffects(model, dat, return_plotData = TRUE)
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  
  vars <- c("age","period","cohort")
  
  summary_tab <- lapply(vars, function(var) {
    dat_var <- dat_list[[paste0("dat_",var)]]
    
    if (var %in% names(apc_range)) { # filter the dataset
      dat_var <- dat_var %>% filter(value %in% apc_range[[var]])
    }
    
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
