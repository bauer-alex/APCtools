
#' Plot of marginal APC effects based on an estimated GAM model
#' 
#' Plot the marginal effect of age, period or cohort, based on an APC model
#' estimated as a semiparametric additive regression model with \code{\link[mgcv]{gam}}
#' or \code{\link[mgcv]{bam}}.
#' This function is a simple wrapper to \code{\link{plot_partialAPCeffects}},
#' called with argument \code{hide_partialEffects = TRUE}.
#' 
#' @inheritParams plot_partialAPCeffects
#' @param plot_CI Indicator if 95% confidence intervals should be plotted.
#' Defaults to FALSE.
#' 
#' @return ggplot object
#' 
#' @export
#' 
#' @references Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
#' Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
#' destination choice patterns: Using generalized additive models to quantify
#' the impact of age, period, and cohort on travel distances.
#' \emph{Tourism Economics}. doi:10.1177/1354816620987198.
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de},
#' Maximilian Weigert \email{maximilian.weigert@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' library(mgcv)
#' 
#' data(travel)
#' model <- gam(mainTrip_distance ~ te(age, period), data = travel)
#' 
#' plot_marginalAPCeffects(model, dat = travel, variable = "age")
#' 
#' # mark specific cohorts
#' plot_marginalAPCeffects(model, dat = travel, variable = "cohort",
#'                         vlines_vec = c(1966.5,1982.5,1994.5))
#' 
plot_marginalAPCeffects <- function(model, dat, variable = "age",
                                    vlines_vec = NULL, plot_CI = FALSE,
                                    return_plotData = FALSE) {
  
  plot_partialAPCeffects(model               = model,
                         dat                 = dat,
                         variable            = variable,
                         hide_partialEffects = TRUE,
                         vlines_vec          = vlines_vec,
                         plot_CI             = plot_CI,
                         return_plotData     = return_plotData)
  
}


#' Partial APC plots based on an estimated GAM model
#' 
#' Create the partial APC plots based on an APC model estimated as a semiparametric
#' additive regression model with \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @inheritParams plot_APCheatmap
#' @param variable One of \code{c("age","period","cohort")}, specifying the
#' temporal dimension for which the partial effect plots should be created.
#' @param hide_partialEffects If TRUE, only the marginal effect will be plotted.
#' Defaults to FALSE.
#' @param vlines_vec Optional numeric vector of values on the x-axis where
#' vertical lines should be drawn. Can be used to highlight the borders of
#' specific age groups, time intervals or cohorts.
#' @param plot_CI Indicator if 95% confidence intervals for marginal APC effects
#' should be plotted. Only used if \code{hide_partialEffects} is set to TRUE.
#'  Defaults to FALSE.
#' @param return_plotData If TRUE, a list of the datasets prepared for plotting
#' is returned instead of the ggplot object. The list contains one dataset each
#' for the overall effect (= evaluations of the APC surface to plot the partial
#' effects) and for each marginal APC effect (no matter the specified value of
#' the argument \code{variable}). Defaults to FALSE.
#' 
#' @return ggplot object (if \code{hide_partialEffects} is TRUE) or a plot grid
#' created with \code{\link[ggpubr]{ggarrange}} (if FALSE).
#' 
#' @import checkmate dplyr ggplot2 stringr
#' @importFrom ggpubr ggarrange
#' @importFrom mgcv predict.gam
#' @export
#' 
#' @references Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
#' Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
#' destination choice patterns: Using generalized additive models to quantify
#' the impact of age, period, and cohort on travel distances.
#' \emph{Tourism Economics}. doi:10.1177/1354816620987198.
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de},
#' Maximilian Weigert \email{maximilian.weigert@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' library(mgcv)
#' 
#' data(travel)
#' model <- gam(mainTrip_distance ~ te(age, period), data = travel)
#' 
#' plot_partialAPCeffects(model, dat = travel, variable = "age")
#' 
#' # mark specific cohorts
#' plot_partialAPCeffects(model, dat = travel, variable = "cohort",
#'                        vlines_vec = c(1966.5,1982.5,1994.5))
#' 
plot_partialAPCeffects <- function(model, dat, variable = "age",
                                   hide_partialEffects = FALSE,
                                   vlines_vec = NULL,
                                   plot_CI = FALSE, 
                                   return_plotData = FALSE) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_data_frame(dat)
  checkmate::assert_choice(variable, choices = c("age","period","cohort"))
  checkmate::assert_logical(hide_partialEffects)
  checkmate::assert_numeric(vlines_vec, min.len = 1, null.ok = TRUE)
  checkmate::assert_logical(plot_CI)
  checkmate::assert_logical(return_plotData)
  
  # Warning if confidence intervals are supposed to be added when plotting
  # partial effects:
  if (plot_CI == TRUE & hide_partialEffects == FALSE) {
    plot_CI <- FALSE
    warning("Confidence intervals can only be shown for marginal effects.
            Please specify 'hide_partialEffects == TRUE' in this case." )
  }
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  period <- age <- effect <- cohort <- exp_effect <- value <- lower <- upper <- NULL
  
  
  # create a dataset for predicting the values of the APC surface
  grid_age    <- min(dat$age, na.rm = TRUE):max(dat$age, na.rm = TRUE)
  grid_period <- min(dat$period, na.rm = TRUE):max(dat$period, na.rm = TRUE)
  dat_predictionGrid <- expand.grid(age    = grid_age,
                                    period = grid_period) %>% 
    mutate(cohort = period - age)
  # add random values for all further covariates in the model,
  # necessary for calling mgcv:::predict.gam
  covars <- attr(model$terms, "term.labels")
  covars <- covars[!(covars %in% c("age","period","cohort"))]
  covars <- if_else(stringr::str_detect(string = covars,
                                        pattern = "(?<=\\().*(?=\\))"),
                    stringr::str_extract(string = covars,
                                         pattern = "(?<=\\().*(?=\\))"),
                    covars)
  if (length(covars) > 0) {
    dat_cov <- dat[,covars, drop = FALSE]
    row     <- which(apply(dat_cov, 1, function(x) { all(!is.na(x)) }))[1]
    dat_predictionGrid[,covars] <- dat[row, covars]
  }
  
  # create a dataset containing the estimated values of the APC surface
  terms_model     <- sapply(model$smooth, function(x) { x$label })
  terms_index_APC <- which(grepl("age", terms_model) | grepl("period", terms_model))
  term_APCsurface <- terms_model[terms_index_APC]
  
  dat_overallEffect <- dat_predictionGrid %>%
    mutate(effect = rowSums(mgcv::predict.gam(object  = model,
                                              newdata = dat_predictionGrid,
                                              type    = "terms",
                                              terms   = term_APCsurface))) %>% 
    mutate(effect = effect - mean(effect))
  used_logLink <- (model$family[[2]] %in% c("log","logit")) |
    grepl("Ordered Categorical", model$family[[1]])
  if (used_logLink) {
    dat_overallEffect <- dat_overallEffect %>% mutate(exp_effect = exp(effect))
  }
  
  # Calculate the mean effects per age / period / cohort and optionally
  # their confidence intervals:
  dat_age <- compute_marginal_APCeffects(dat = dat_overallEffect, model = model,
                                         variable = "age", plot_CI = plot_CI)
  dat_period <- compute_marginal_APCeffects(dat = dat_overallEffect, model = model,
                                         variable = "period", plot_CI = plot_CI)
  dat_cohort <- compute_marginal_APCeffects(dat = dat_overallEffect, model = model,
                                         variable = "cohort", plot_CI = plot_CI)
  
  # define the theme
  theme <- theme(text             = element_text(size = 16),
                 axis.title       = element_text(size = 16),
                 axis.text        = element_text(size = 16),
                 legend.text      = element_text(size = 12),
                 legend.key.width = unit(1.2, "cm"),
                 plot.title       = element_text(hjust = 0.5, size = 18, face = "bold"),
                 strip.text.y     = element_text(size = 16), 
                 strip.placement  = "outside",
                 strip.background = element_blank(),
                 axis.title.y     = element_text(margin = margin(0, 10, 0, 0)),
                 axis.title.x     = element_text(margin = margin(10, 0, 0, 0))) 
  
  # final preparations
  if (used_logLink) {
    dat_overallEffect <- dat_overallEffect %>% select(-effect) %>%
      dplyr::rename(effect = exp_effect)
  }
  y_lab <- ifelse(used_logLink, "Exp effect", "Effect")
  
  # return the plot data instead of the ggplot object, if specified
  if (return_plotData) {
    return(list("dat_overallEffect" = dat_overallEffect,
                "dat_age"           = dat_age,
                "dat_period"        = dat_period,
                "dat_cohort"        = dat_cohort))
  }
  
  # base plot
  gg <- ggplot()
  
  if (!is.null(vlines_vec)) {
    gg <- gg +
      geom_vline(xintercept = vlines_vec, col = gray(0.5), lty = 2)
  }
  
  
  # main plot
  if (variable == "age") {
    
    if (hide_partialEffects) { # plot with only the marginal age effect
      
      if (plot_CI == TRUE) {
        gg <- gg +
          geom_ribbon(data = dat_age,
                      mapping = aes(x = value, ymin = lower, ymax = upper,
                                    fill = "")) +
          scale_fill_manual(values = gray(0.75)) +
          theme(legend.position = "none")
      }
      gg_final <- gg +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        ylab(y_lab) + xlab("Age") + theme + ggtitle("Marginal age effect")
      
    } else { # plots including the partial effects
      
      # age versus period
      gg_AP <- gg +
        geom_line(data = dat_overallEffect,
                  mapping = aes(x = age, y = effect, group = period, col = period)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Period") +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        ylab(y_lab) + xlab("Age") + theme + ggtitle("Age effect by periods")
      
      # age versus cohort
      gg_AC <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = age, y = effect, group = cohort, col = cohort)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Cohort") +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Age") +
        ggtitle("Age effect by cohorts")
      
      if (used_logLink) {
        gg_AP <- gg_AP + scale_y_continuous(trans = "log2")
        gg_AC <- gg_AC + scale_y_continuous(trans = "log2")
      }
      
      # combine both plots
      gg_final <- ggpubr::ggarrange(plotlist = list(gg_AP, gg_AC),
                                    ncol = 2, legend = "bottom")
    }
    
  } else if (variable == "period") {
    
    if (hide_partialEffects) { # plot with only the marginal age effect
      
      if (plot_CI == TRUE) {
        gg <- gg +
          geom_ribbon(data = dat_period,
                      mapping = aes(x = value, ymin = lower, ymax = upper,
                                    fill = "")) +
          scale_fill_manual(values = gray(0.75)) +
          theme(legend.position = "none")
      }
      gg_final <- gg +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        ylab(y_lab) + xlab("Age") + theme + ggtitle("Marginal age effect")
      gg_final <- gg +
        geom_line(data = dat_period, mapping = aes(x = value, y = effect)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
        ylab(y_lab) + xlab("Period") + theme + ggtitle("Marginal period effect")
      
    } else { # plots including the partial effects
      
      # period versus age
      gg_PA <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = period, y = effect, group = age, col = age)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
        scale_color_continuous(low = "grey90", high = "grey10", name = "Age") +
        geom_line(data = dat_period,
                  mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        ylab(y_lab) + xlab("Period") + theme + ggtitle("Period effect by age")
      
      # period versus cohort
      gg_PC <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = period, y = effect, group = cohort, col = cohort)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Cohort") +
        geom_line(data = dat_period, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Period") +
        ggtitle("Period effect by cohorts")
      
      if (used_logLink) {
        gg_PA <- gg_PA + scale_y_continuous(trans = "log2")
        gg_PC <- gg_PC + scale_y_continuous(trans = "log2")
      }
      
      # combine both plots
      gg_final <- ggpubr::ggarrange(plotlist = list(gg_PA, gg_PC),
                                    ncol = 2, legend = "bottom")
    }
    
  } else if (variable == "cohort") {
    
    if (hide_partialEffects) { # plot with only the marginal age effect
      
      if (plot_CI == TRUE) {
        gg <- gg +
          geom_ribbon(data = dat_cohort,
                      mapping = aes(x = value, ymin = lower, ymax = upper,
                                    fill = "")) +
          scale_fill_manual(values = gray(0.75)) +
          theme(legend.position = "none")
      }
      gg_final <- gg +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        ylab(y_lab) + xlab("Age") + theme + ggtitle("Marginal age effect")
      gg_final <- gg +
        geom_line(data = dat_cohort, mapping = aes(x = value, y = effect)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        ylab(y_lab) + xlab("Cohort") + theme + ggtitle("Marginal cohort effect")
      
    } else { # plots including the partial effects
      
      # cohort versus age
      gg_CA <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = cohort, y = effect, group = age, col = age)) +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Age") +
        geom_line(data = dat_cohort, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        ylab(y_lab) + xlab("Cohort") + theme + ggtitle("Cohort effect by age")
      
      # cohort versus period
      gg_CP <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = cohort, y = effect, group = period,
                                col = period)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Period") +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
        geom_line(data = dat_cohort, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Cohort") +
        ggtitle("Cohort effect by periods")
      
      if (used_logLink) {
        gg_CA <- gg_CA + scale_y_continuous(trans = "log2")
        gg_CP <- gg_CP + scale_y_continuous(trans = "log2")
      }
      
      # combine both plots
      gg_final <- ggpubr::ggarrange(plotlist = list(gg_CA, gg_CP),
                                    ncol = 2, legend = "bottom")
    }
  }
  
  return(gg_final)
}



#' Internal helper to compute marginal APC effects and their
#' confidence intervals 
#' 
#' Internal helper function to add lower and upper confidence boundaries
#' pointwise
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @param model Model fitted with \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' @param dat Dataset containing predicted effects for a grid of all APC
#' dimensions and covariates used in the model.
#' @param variable One of \code{c("age","period","cohort")}, specifying the
#' temporal dimension for which the partial effect plots should be created.
#' @param plot_CI Indicator if 95% confidence intervals for marginal APC effects
#' should be computed. Defaults to FALSE.

#' @import checkmate dplyr
#' @importFrom mgcv summary.gam
#' 
compute_marginal_APCeffects <- function(dat, model, variable, plot_CI = FALSE) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_class(model, classes = "gam")
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  effect <- lower <- upper <- exp_effect <- exp_lower <- exp_upper <- NULL
  
  used_logLink <- (model$family[[2]] %in% c("log","logit")) |
    grepl("Ordered Categorical", model$family[[1]])
  
  # Compute marginal effects:
  dat_marginal <- dat %>% group_by(!!sym(variable)) %>%
    summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(variable = variable) %>% dplyr::rename(value = !!sym(variable))
  
  # Exponentiate effects in case of log link:
  if (used_logLink) {
    dat_marginal <- dat_marginal %>% mutate(exp_effect = exp(effect))
  }
  
  if (plot_CI == TRUE) {
    # Compute standard error for sum of age effects:
    se <- c()
    for (a in sort(unique(dat[, variable]))) {
      dat_temp <- dat %>% filter(!!sym(variable) == a)
      pred <- mgcv::predict.gam(object  = model, newdata = dat_temp,
                                type = "lpmatrix")
      col1 <- rep(1, nrow(pred))
      Xs <- t(col1) %*% pred
      var.sum <- Xs %*% model$Vp %*% t(Xs)
      var.mean <- var.sum / (nrow(dat_temp)^2)
      sqrt.mean <- sqrt(var.mean)
      se[which(sort(unique(unique(dat[, variable]))) == a)] <- sqrt.mean
    }
    # Compute confidence intervals:
    dat_marginal <- dat_marginal %>%
      mutate(se = se, lower = effect - qnorm(0.975) * se,
             upper = effect + qnorm(0.975) * se) %>%
      select(-se)
    # Exponentiate boundaries in case of log link:
    if (used_logLink) {
      dat_marginal <- dat_marginal %>%
        mutate(exp_lower = exp(lower), exp_upper = exp(upper))
    }
  }
  
  # Final preparations:
  if (used_logLink) {
    if (plot_CI == TRUE) {
      dat_marginal <- dat_marginal %>% select(-effect, -lower, -upper) %>% 
        dplyr::rename(effect = exp_effect, lower = exp_lower, upper = exp_upper)
    }
    else {
      dat_marginal <- dat_marginal %>% select(-effect) %>% 
        dplyr::rename(effect = exp_effect)
    }
  }

  return(dat_marginal)
}






