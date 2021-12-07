
#' Plot linear effects of a gam in an effect plot
#' 
#' Create an effect plot of linear effects of a model fitted with
#' \code{\link[mgcv]{gam}}.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @inheritParams extract_summary_linearEffects

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
plot_linearEffects <- function(model) {
  
  checkmate::assert_class(model, classes = "gam")
  
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  ylab         <- ifelse(used_logLink, "Odds Ratio", "Effect")
  
  # extract model information
  plot_dat <- extract_summary_linearEffects(model)
  
  # categorize the coefficients in groups (one for each variable)
  for (i in names(eval(model$call$data))) {
    plot_dat$vargroup[grepl(i, plot_dat$param)] <- i
  }
  # remove the intercept
  plot_dat <- plot_dat[-1,]
  # remove the vargroup label from the coefficient labels for categorical variables
  plot_dat$param <- as.character(plot_dat$param)
  cat_coefs      <- which(nchar(plot_dat$param) > nchar(plot_dat$vargroup))
  if (length(cat_coefs) > 0) {
    plot_dat$param[cat_coefs] <- substr(plot_dat$param[cat_coefs],
                                        nchar(plot_dat$vargroup[cat_coefs]) + 1,
                                        100)
  }
  
  # final preparations
  if (used_logLink) {
    plot_dat <- plot_dat %>% select(-coef, -CI_lower, -CI_upper) %>%
      dplyr::rename(coef = coef_exp, CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
  }
  
  # create plot
  gg <- ggplot(plot_dat, mapping = aes(x = param, y = coef)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = "firebrick2", lty = 2) +
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
