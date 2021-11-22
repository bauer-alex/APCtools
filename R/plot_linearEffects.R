
#' Plot linear effects of a gam in an effect plot
#' 
#' Create an effect plot of linear effects of a model fitted with
#' \code{\link[mgcv]{gam}}.
#' 
#' If the model was estimated with a log link, the function automatically
#' performs an exponential transformation of the effects.
#' 
#' @inheritParams extract_modelSummary

#' @import colorspace dplyr
#' @export
#' 
plot_linearEffects <- function(model) {
  
  used_logLink <- (model$family[[2]] == "log" | model$family[[2]] == "logit")
  ylab         <- ifelse(used_logLink, "Odds Ratio", "Effect")
  
  # extract model information
  plot_dat <- extract_modelSummary(model)
  
  # categorize the coefficients in groups (one for each variable)
  for (i in names(eval(model$call$data))) {
    plot_dat$vargroup[str_detect(plot_dat$param, i)] <- i
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
  
  # create plot
  gg <- ggplot(plot_dat, mapping = aes(x = param, y = coef_exp)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = "firebrick2") +
    geom_pointrange(mapping = aes(ymin = CI_lower_exp, ymax = CI_upper_exp, col = vargroup), size = 1) +
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


#' Extract summary of linear effects in a gam model
#' 
#' Creates a \code{data.frame} containing the linear effects summary of a 
#' model fitted with \code{\link[mgcv]{gam}}.
#' 
#' If the model was estimated with a log link, the function automatically
#' performs an exponential transformation of the effects.
#' 
#' @param model Model fitted with \code{\link[mgcv]{gam}}.
#' 
#' @import dplyr
#' @importFrom mgcv summary.gam
#' @export
#' 
extract_modelSummary <- function(model) {
  
  used_logLink <- (model$family[[2]] == "log" | model$family[[2]] == "logit")
  
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
