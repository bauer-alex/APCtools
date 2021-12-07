
#' Plot of marginal APC effects based on an estimated GAM model
#' 
#' Plot the marginal effect of age, period or cohort, based on an APC model
#' estimated as a semiparametric additive regression model with \code{\link[mgcv]{gam}}.
#' This function is a simple wrapper to \code{\link{plot_partialAPCeffects}},
#' called with argument \code{hide_partialEffects = TRUE}.
#' 
#' @inheritParams plot_partialAPCeffects
#' 
#' @export
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
                                    vlines_vec = NULL, return_plotData = FALSE) {
  
  plot_partialAPCeffects(model               = model,
                         dat                 = dat,
                         variable            = variable,
                         hide_partialEffects = TRUE,
                         vlines_vec          = vlines_vec,
                         return_plotData     = return_plotData)
  
}


#' Partial APC plots based on an estimated GAM model
#' 
#' Create the partial APC plots based on an APC model estimated as a semiparametric
#' additive regression model with \code{\link[mgcv]{gam}}.
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
#' @param return_plotData If TRUE, a list of the datasets prepared for plotting
#' is returned instead of the ggplot object. The list contains one dataset each
#' for the overall effect (= evaluations of the APC surface to plot the partial
#' effects) and for each marginal APC effect (no matter the specified value of
#' the argument \code{variable}). Defaults to FALSE.
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom mgcv predict.gam
#' @export
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
                                   vlines_vec = NULL, return_plotData = FALSE) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_data_frame(dat)
  checkmate::assert_choice(variable, choices = c("age","period","cohort"))
  checkmate::assert_logical(hide_partialEffects)
  checkmate::assert_numeric(vlines_vec, min.len = 1, null.ok = TRUE)
  checkmate::assert_logical(return_plotData)
  
  
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
  if (length(covars) > 0) {
    row <- which(apply(dat[,covars], 1, function(x) { all(!is.na(x)) }))[1]
    dat_predictionGrid[,covars] <- dat[row, covars]
  }
  
  # create a dataset containing the estimated values of the APC surface
  terms_model     <- sapply(model$smooth, function(x) { x$label })
  terms_index_APC <- which(grepl("age", terms_model) | grepl("period", terms_model))
  term_APCsurface <- terms_model[terms_index_APC]
  
  dat_overallEffect <- dat_predictionGrid %>%
    mutate(effect = rowSums(mgcv::predict.gam(object  = model,
                                              newdata = .,
                                              type    = "terms",
                                              terms   = term_APCsurface))) %>% 
    mutate(effect = effect - mean(effect))
  used_logLink <- model$family[[2]] %in% c("log","logit")
  if (used_logLink) {
    dat_overallEffect <- dat_overallEffect %>% mutate(exp_effect = exp(effect))
  }
  
  # calculate the mean effects per age / period / cohort
  dat_age <- dat_overallEffect %>%
    group_by(age) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(variable = "Age") %>% dplyr::rename(value = age)
  dat_period <- dat_overallEffect %>%
    group_by(period) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(variable   = "Period") %>% dplyr::rename(value = period)
  dat_cohort <- dat_overallEffect %>%
    group_by(cohort) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(variable = "Cohort") %>% dplyr::rename(value = cohort)
  if (used_logLink) {
    dat_age    <- dat_age    %>% mutate(exp_effect = exp(effect))
    dat_period <- dat_period %>% mutate(exp_effect = exp(effect))
    dat_cohort <- dat_cohort %>% mutate(exp_effect = exp(effect))
  }
  
  # define the theme
  theme <- theme(text = element_text(size = 16), axis.title = element_text(size = 16),
                 axis.text = element_text(size = 16),
                 legend.text = element_text(size = 12),
                 plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                 strip.text.y = element_text(size = 16), 
                 strip.placement = "outside", strip.background = element_blank(),
                 axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
                 axis.title.x = element_text(margin = margin(10, 0, 0, 0))) 
  
  # final preparations
  if (used_logLink) {
    dat_overallEffect <- dat_overallEffect %>% select(-effect) %>% dplyr::rename(effect = exp_effect)
    dat_age           <- dat_age           %>% select(-effect) %>% dplyr::rename(effect = exp_effect)
    dat_period        <- dat_period        %>% select(-effect) %>% dplyr::rename(effect = exp_effect)
    dat_cohort        <- dat_cohort        %>% select(-effect) %>% dplyr::rename(effect = exp_effect)
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
      
      gg_final <- gg +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect)) +
        ylab(y_lab) + xlab("Age") + theme + ggtitle("Marginal age effect")
      
    } else { # plots including the partial effects
      
      # age versus period
      gg_AP <- gg +
        geom_line(data = dat_overallEffect,
                  mapping = aes(x = age, y = effect, group = period, col = period)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Period") +
        geom_line(data = dat_age, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        ylab(y_lab) + xlab("Age") + theme + ggtitle("Age effect by periods")
      
      # age versus cohort
      gg_AC <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = age, y = effect, group = cohort, col = cohort)) +
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
      gg_final <- ggarrange(plotlist = list(gg_AP, gg_AC), ncol = 2, legend = "bottom")
    }
    
  } else if (variable == "period") {
    
    if (hide_partialEffects) { # plot with only the marginal age effect
      
      gg_final <- gg +
        geom_line(data = dat_period, mapping = aes(x = value, y = effect)) +
        scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) + 
        ylab(y_lab) + xlab("Period") + theme + ggtitle("Marginal period effect")
      
    } else { # plots including the partial effects
      
      # period versus age
      gg_PA <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = period, y = effect, group = age, col = age)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Age") +
        geom_line(data = dat_period,
                  mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) + 
        ylab(y_lab) + xlab("Period") + theme + ggtitle("Period effect by age")
      
      # period versus cohort
      gg_PC <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = period, y = effect, group = cohort, col = cohort)) +
        scale_color_continuous(low = "grey90", high = "grey10", name = "Cohort") +
        geom_line(data = dat_period, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) +
        theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Period") +
        ggtitle("Period effect by cohorts")
      
      if (used_logLink) {
        gg_PA <- gg_PA + scale_y_continuous(trans = "log2")
        gg_PC <- gg_PC + scale_y_continuous(trans = "log2")
      }
      
      # combine both plots
      gg_final <- ggarrange(plotlist = list(gg_PA, gg_PC), ncol = 2, legend = "bottom")
    }
    
  } else if (variable == "cohort") {
    
    if (hide_partialEffects) { # plot with only the marginal age effect
      
      gg_final <- gg +
        geom_line(data = dat_cohort, mapping = aes(x = value, y = effect)) +
        ylab(y_lab) + xlab("Cohort") + theme + ggtitle("Marginal cohort effect")
      
    } else { # plots including the partial effects
      
      # cohort versus age
      gg_CA <- gg +
        geom_line(data    = dat_overallEffect,
                  mapping = aes(x = cohort, y = effect, group = age, col = age)) +
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
        geom_line(data = dat_cohort, mapping = aes(x = value, y = effect),
                  size = 1.5, col = "RoyalBlue3") +
        theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Cohort") +
        ggtitle("Cohort effect by periods")
      
      if (used_logLink) {
        gg_CA <- gg_CA + scale_y_continuous(trans = "log2")
        gg_CP <- gg_CP + scale_y_continuous(trans = "log2")
      }
      
      # combine both plots
      gg_final <- ggarrange(plotlist = list(gg_CA, gg_CP), ncol = 2, legend = "bottom")
    }
  }
  
  return(gg_final)
  }
