
#' Plot 1D smooth effects for \code{\link[mgcv]{gam}} models
#'
#' Plots 1D smooth effects for a GAM model fitted with \code{\link[mgcv]{gam}}
#' or \code{\link[mgcv]{bam}}.
#'
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect,
#' see argument \code{method_expTransform}.
#' 
#' @param model GAM model fitted with \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}.
#' @param plot_ci If \code{TRUE} CIs are plotted. Only used if \code{plot_type = 1}.
#' @param select Index of smooth term to be plotted.
#' @param alpha \code{(1-alpha)} CIs are calculated. The default 0.05 leads to
#' 95% CIs.
#' @param ylim Optional limits of the y-axis.
#' @param method_expTransform One of \code{c("simple","delta")}, stating if
#' standard errors and confidence interval limits should be transformed by
#' a simple exp transformation or using the delta method. The delta method can
#' be unstable in situations and lead to negative confidence interval limits.
#' Only used when the model was estimated with a log or logit link.
#' @param return_plotData If TRUE, the dataset prepared for plotting is
#' returned. Defaults to FALSE.
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
                          ylim = NULL, method_expTransform = "simple",
                          return_plotData = FALSE) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_logical(plot_ci)
  checkmate::assert_numeric(select, lower = 1)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1)
  checkmate::assert_numeric(ylim, len = 2, null.ok = TRUE)
  checkmate::assert_choice(method_expTransform, choices = c("simple","delta"))
  checkmate::assert_logical(return_plotData, len = 1)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  fit <- se <- fit_exp <- se_exp <- CI_lower <- CI_upper <- CI_lower_exp <-
    CI_upper_exp <- x <- y <- y_exp <- NULL
  
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  ylab         <- ifelse(used_logLink, "exp(Effect)", "Effect")
  
  plotObject <- get_plotGAMobject(model)
  
  plotObject <- plotObject[[select]]
  plot_dat  <- data.frame(x  = plotObject$x,
                          y  = plotObject$fit,
                          se = plotObject$se / plotObject$se.mult) %>% 
    mutate(CI_lower = y - qnorm(1 - alpha/2)*se,
           CI_upper = y + qnorm(1 - alpha/2)*se)
  
  if (used_logLink) {
    
    # transform the point estimates
    plot_dat <- plot_dat %>%
      mutate(y_exp = exp(y)) %>%
      select(-y) %>% 
      dplyr::rename(y = y_exp)
    
    # transform the confidence intervals
    if (plot_ci) {
      
      if (method_expTransform == "simple") {

        plot_dat <- plot_dat %>%
          mutate(se_exp       = exp(se),
                 CI_lower_exp = exp(CI_lower),
                 CI_upper_exp = exp(CI_upper)) %>%
          select(-se, -CI_lower, -CI_upper) %>% 
          dplyr::rename(se = se_exp, CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
        
      } else { # method_expTransform == "delta"
        
        plot_dat <- plot_dat %>%
          mutate(se_exp  = sqrt(se^2 * y^2)) %>%
          mutate(CI_lower_exp = y - qnorm(1 - alpha/2) * se_exp,
                 CI_upper_exp = y + qnorm(1 - alpha/2) * se_exp) %>% 
          select(-se, -CI_lower, -CI_upper) %>% 
          dplyr::rename(se = se_exp, CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
        
        # correct negative CI_lower borders
        if (any(plot_dat$CI_lower < 0)) {
          warning("Note: After the delta method transformation some values of the
              lower confidence interval border resulted were negative. These
              values were set to 0.01")
          plot_dat$CI_lower[plot_dat$CI_lower < 0] <- 0.01
        }
      }
    }
  }
  
  if (return_plotData) {
    return(plot_dat)
  }
  
  # if 'ylim' is set and the CIs exceed it, trim them accordingly
  if (!is.null(ylim)) {
    plot_dat$CI_lower[plot_dat$CI_lower < ylim[1]] <- ylim[1]
    plot_dat$CI_upper[plot_dat$CI_upper > ylim[2]] <- ylim[2]
  }
  
  # plot
  gg <- ggplot(plot_dat, aes(x = x, y = y))
  if (plot_ci) {
    gg <- gg + geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = "")) +
      scale_fill_manual(values = gray(0.75))
  }
  gg <- gg + 
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
    geom_line(aes(col = "")) + xlab(plotObject$xlab) +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    scale_color_manual(values = "black") +
    theme(legend.position = "none")
  
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
