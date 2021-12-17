
#' Plot 1D smooth effects for \code{\link[mgcv]{gam}} models
#'
#' Plots 1D smooth effects for a GAM model fitted with \code{\link[mgcv]{gam}}
#' or \code{\link[mgcv]{bam}}.
#'
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @param model GAM model fitted with \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}.
#' @param plot_ci If \code{TRUE} CIs are plotted. Only used if \code{plot_type = 1}.
#' @param select Index of smooth term to be plotted.
#' @param alpha \code{(1-alpha)} CIs are calculated. The default 0.05 leads to
#' 95% CIs.
#' @param ylim Optional limits of the y-axis.
#' 
#' @return ggplot object
#' 
#' @importFrom grDevices gray
#' @importFrom stats qnorm
#' @import checkmate dplyr ggplot2
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
#' plot_1Dsmooth(model, select = 2)
#' 
plot_1Dsmooth <- function(model, plot_ci = TRUE, select, alpha = 0.05,
                          ylim = NULL) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_logical(plot_ci)
  checkmate::assert_numeric(select, lower = 1)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1)
  checkmate::assert_numeric(ylim, len = 2, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  fit <- se <- fit_exp <- se_exp <- CI_lower <- CI_upper <- CI_lower_exp <-
    CI_upper_exp <- x <- y <- NULL
  
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  ylab         <- ifelse(used_logLink, "Odds Ratio", "Effect")
  
  plotObject <- get_plotGAMobject(model)
  
  plotObject <- plotObject[[select]]
  plot_dat  <- data.frame(x   = plotObject$x,
                          fit = plotObject$fit,
                          se  = plotObject$se) %>% 
    mutate(CI_lower = fit - qnorm(1 - alpha/2)*se,
           CI_upper = fit + qnorm(1 - alpha/2)*se)
  
  if (used_logLink) {
    # confidence intervals on exp scale are computed based on the delta method
    plot_dat <- plot_dat %>%
      mutate(fit_exp = exp(fit),
             se_exp  = sqrt(se^2 * exp(fit)^2)) %>%
      mutate(CI_lower_exp = fit_exp - qnorm(1 - alpha/2) * se_exp,
             CI_upper_exp = fit_exp + qnorm(1 - alpha/2) * se_exp) %>% 
      select(-fit, -se, -CI_lower, -CI_upper) %>% 
      dplyr::rename(fit = fit_exp, se = se_exp,
                    CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
    
    # correct negative CI_lower borders
    if (any(plot_dat$CI_lower < 0)) {
      warning("Note: After the delta method transformation some values of the
              lower confidence interval border resulted were negative. These
              values were set to 0.01")
      plot_dat$CI_lower[plot_dat$CI_lower < 0] <- 0.01
    }
  }
  
  if (plot_ci) {
    poly_dat <- data.frame(x = c(plot_dat$x, rev(plot_dat$x)),
                           y = c(plot_dat$CI_lower, rev(plot_dat$CI_upper)))
  }
  
  gg <- ggplot(plot_dat, aes(x = x, y = fit))
  if (plot_ci) {
    gg <- gg + geom_polygon(data = poly_dat, aes(x = x, y = y), fill = gray(0.75))
  }
  gg <- gg + 
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = "firebrick2", lty = 2) +
    geom_line() + xlab(plotObject$xlab) +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim)
  
  return(gg)
}



#' Extract returned values of plot.gam() while suppressing creation of the plot
#'
#' Internal helper function to extract the values returned of
#' \code{\link[mgcv]{plot.gam}} while suppressing creation of the plot.
#' 
#' @inheritParams plot_1Dsmooth
#' 
#' @importFrom grDevices png dev.off
#' @importFrom mgcv plot.gam
#' @import checkmate
#' 
get_plotGAMobject <- function(model) {
  
  checkmate::assert_class(model, classes = "gam")
  
  
  # Idea: Save the plot in a temporal png file, which is deleted right
  #       afterwards.
  png("temp.png")
  # plot.gam returns all terms by default, select and rug are set only to decrease evaluation time
  plot.df <- mgcv::plot.gam(model, select = 1, rug = FALSE)
  dev.off()
  unlink("temp.png", recursive = TRUE)
  
  # delete 'raw' elements as they are very large but not necessary for plotting the effects
  plot.df <- lapply(plot.df, function(x) {
    x$raw <- NULL
    x
  })
  
  return(invisible(plot.df))
}
