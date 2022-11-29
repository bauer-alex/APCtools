
#' Plot linear effects of a gam in an effect plot
#' 
#' Create an effect plot of linear effects of a model fitted with
#' \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @inheritParams extract_summary_linearEffects
#' @param variables Optional character vector of variable names specifying which
#' effects should be plotted. The order of the vector corresponds to
#' the order in the effect plot. If the argument is not specified, all linear
#' effects are plotted according to the order of their appearance in the model
#' output.
#' @param return_plotData If TRUE, the dataset prepared for plotting is
#' returned. Defaults to FALSE.
#' @param ... Additional arguments passed to
#' \code{\link{extract_summary_linearEffects}}.
#' 
#' 
#' @return ggplot object
#' 
#' @import checkmate dplyr
#' @importFrom colorspace scale_colour_discrete_qualitative
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
#' plot_linearEffects(model)
#' 
plot_linearEffects <- function(model, variables = NULL,
                               return_plotData = FALSE, ...) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_character(variables, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  coef <- CI_lower <- CI_upper <- coef_exp <- CI_lower_exp <- CI_upper_exp <-
    param <- vargroup <- varnames <- vars <- var_classes <- NULL
  
  
  used_logLink <- (model$family[[2]] %in% c("log","logit")) |
    grepl("Ordered Categorical", model$family[[1]])
  ylab         <- ifelse(used_logLink, "exp(Effect)", "Effect")
  
  # extract model information
  plot_dat <- extract_summary_linearEffects(model, ...)
  plot_dat$vargroup <- NA
  
  # categorize the coefficients in groups (one for each variable)
  var_classes <- attributes(model$pterms)$dataClasses[-1]
  vars <- names(var_classes)
  for (i in vars) {
    # potentially more than one coefficient for factor or character variables:
    if (var_classes[i] %in% c("character", "factor")) {
      varnames <- paste0(i, unlist(unname(model$xlevels[i])))
      plot_dat$vargroup[which(plot_dat$param %in% varnames)] <- i
    }
    # only a single coefficient for numeric variables
    else {
      plot_dat$vargroup[which(plot_dat$param == i)] <- i
    }
  }
  # remove the intercept
  plot_dat <- plot_dat[-1,]
  # select variables to plot:
  if (!is.null(variables)) {
    plot_dat <- plot_dat %>% filter(vargroup %in% variables)
  }
  # remove the vargroup label from the coefficient labels for categorical variables
  plot_dat$param <- as.character(plot_dat$param)
  cat_coefs      <- which(nchar(plot_dat$param) > nchar(plot_dat$vargroup))
  if (length(cat_coefs) > 0) {
    plot_dat$param[cat_coefs] <- substr(plot_dat$param[cat_coefs],
                                        nchar(plot_dat$vargroup[cat_coefs]) + 1,
                                        100)
  }
  
  # reorder dataset according to specified variable vector
  var_levels <- if(is.null(variables)) unique(plot_dat$vargroup) else variables
  plot_dat <- plot_dat %>%
     mutate(vargroup = factor(x = vargroup, levels = var_levels)) %>%
     arrange(vargroup)
  
  # final preparations
  if (used_logLink) {
    if (any(plot_dat$CI_lower_exp < 0)) {
      warning("Note: After the delta method transformation some values of the
              lower confidence interval border resulted were negative. These
              values were set to 0.01")
      plot_dat$CI_lower_exp[plot_dat$CI_lower_exp < 0] <- 0.01
    }
    
    plot_dat <- plot_dat %>% select(-coef, -CI_lower, -CI_upper) %>%
      dplyr::rename(coef = coef_exp, CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
  }
  
  if (return_plotData) {
    return(plot_dat)
  }
  
  # create plot
  gg <- ggplot(plot_dat, mapping = aes(x = param, y = coef)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
    geom_pointrange(mapping = aes(ymin = CI_lower, ymax = CI_upper, col = vargroup), size = 1) +
    geom_point(mapping = aes(col = vargroup), size = 1) +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab) +
    colorspace::scale_colour_discrete_qualitative(palette = "Dark 3") +
    facet_wrap(~ vargroup, scales = "free_x", nrow = 1) +
    theme(legend.position = "none",
          axis.title.x    = element_blank(),
          axis.text.x     = element_text(angle = 45, hjust = 1))
    
  return(gg)
}
