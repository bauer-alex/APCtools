
#' Heatmap of the APC surface based on an estimated GAM model
#' 
#' Plot the heatmap of the two-dimensional tensor product surface for the
#' APC effect, based on a model estimated with \code{\link[mgcv]{gam}}.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @param model Regression model estimated with \code{\link[mgcv]{gam}}.
#' @param dat Dataset as passed to \code{gam} for model estimation, containing
#' columns \code{period} and \code{age}.
#' @param angle Numeric angle of the text labels written on the diagonals that
#' highlight individual cohorts. Defaults to 15.
#' @param plot_CI Indicator if the confidence intervals should be plotted.
#' Defaults to TRUE.
#' 
#' @import checkmate dplyr
#' @export
#' 
plot_APCheatmap <- function(model, dat, angle = 15, plot_CI = TRUE) {
  
  checkmate::assert_class(model, classes = "gam")
  checkmate::assert_data_frame(dat)
  checkmate::assert_numeric(angle, len = 1)
  checkmate::assert_logical(plot_CI)
  
  
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
    dat_predictionGrid[,covars] <- dat[1, covars]
  }

  # create a dataset containing the estimated values of the APC surface
  terms_model     <- sapply(model$smooth, function(x) { x$label })
  terms_index_APC <- which(grepl("age", terms_model) | grepl("period", terms_model))
  term_APCsurface <- terms_model[terms_index_APC]
  
  prediction <- dat_predictionGrid %>% 
    predict(object  = model,
            newdata = .,
            type    = "terms",
            terms   = term_APCsurface,
            se.fit  = TRUE) 
  plot_dat <- dat_predictionGrid %>%
    mutate(effect = prediction$fit, se = prediction$se.fit) %>% 
    mutate(effect = effect - mean(effect)) %>% 
    mutate(upper  = effect + qnorm(0.95) * se,
           lower  = effect - qnorm(0.95) * se)
  
  used_logLink <- model$family[[2]] %in% c("log","logit")
  if (used_logLink) {
    plot_dat <- plot_dat %>% 
      mutate(exp_effect = exp(effect),
             exp_se     = sqrt((se^2) * (exp_effect^2))) %>% 
      mutate(exp_upper  = exp_effect + qnorm(0.975) * exp_se,
             exp_lower  = exp_effect - qnorm(0.975) * exp_se) %>% 
      select(-effect, -se, -upper, -lower) %>% 
      dplyr::rename(effect = exp_effect, se = exp_se,
                    upper  = exp_upper, lower = exp_lower)
  }  
  
  # define thresholds
  thresholds_age    <- seq(10, 100, by = 5)
  thresholds_period <- seq(grid_period[1], grid_period[length(grid_period)], by = 5)
  thresholds_cohort <- c(1946, 1966, 1982, 1994)
  plot_dat <- plot_dat %>%
    mutate(age_cat    = cut(age,    breaks = thresholds_age,    right = FALSE),
           period_cat = cut(period, breaks = thresholds_period, right = FALSE),
           cohort_cat = cut(cohort, breaks = thresholds_cohort, right = FALSE))
  dat_effects <- plot_dat %>%
    group_by(age_cat, period_cat) %>%
    summarize(mean_effect = mean(effect),
              mean_upper  = mean(upper),
              mean_lower  = mean(lower)) %>%
    ungroup()
  plot_dat <- plot_dat %>% full_join(dat_effects)
  
  # preparation of cohort lines
  if (grid_period[1] <= 1970) {
    x1 <- (1970:grid_period[length(grid_period)]) + 0.5
  } else {
    x1 <- (grid_period[1]:grid_period[length(grid_period)]) + 0.5
  }
  beta1 <- c(intercept = -1950, slope = 1)
  y1 <- cbind(1, x1) %*% beta1
  if (grid_period[1] <= 1978) {
    x2 <- (1978:grid_period[length(grid_period)]) + 0.5
  } else {
    x2 <- (grid_period[1]:grid_period[length(grid_period)]) + 0.5
  }
  beta2 <- c(intercept = -1965, slope = 1)
  y2 <- cbind(1, x2) %*% beta2
  if (grid_period[1] <= 1993) {
    x3 <- (1993:grid_period[length(grid_period)]) + 0.5
  } else {
    x3 <- (grid_period[1]:grid_period[length(grid_period)]) + 0.5
  }
  beta3 <- c(intercept = -1980, slope = 1)
  y3 <- cbind(1, x3) %*% beta3
  if (grid_period[1] <= 2008) {
    x4 <- (2008:grid_period[length(grid_period)]) + 0.5
  } else {
    x4 <- (grid_period[1]:grid_period[length(grid_period)]) + 0.5
  }
  beta4 <- c(intercept = -1995, slope = 1)
  y4 <- cbind(1, x4) %*% beta4

  # define the theme
  theme <- theme(text = element_text(size = 12), axis.title = element_text(size = 20),
                 axis.text = element_text(size = 18),
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 20),
                 plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                 strip.text.y = element_text(size = 18),
                 strip.text.x = element_text(size = 18),
                 strip.placement = "outside", strip.background = element_blank(),
                 axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
                 axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
  
  # final preparations
  y_lab   <- ifelse(used_logLink, "Mean exp effect", "Mean effect")
  y_trans <- ifelse(used_logLink, "log", "identity")
  
  # create the base heatmap plots
  gg_effect <- ggplot(data    = plot_dat,
                      mapping = aes(x = period, y = age, fill = mean_effect)) +
    geom_tile() + ylab(y_lab) + ggtitle("Effect")
  
  if (!plot_CI) {
    limits_color <- c(NA,NA)
    gg_list <- list(gg_effect)
    
  } else {
    limits_color <- c(min(floor(plot_dat$mean_lower   * 1000) / 1000),
                      max(ceiling(plot_dat$mean_upper * 1000) / 1000))
    
    gg_lower <- ggplot(data    = plot_dat,
                       mapping = aes(x = period, y = age, fill = mean_lower)) +
      geom_tile() + ggtitle("Lower 95% CI boundary")
    gg_upper <- ggplot(data    = plot_dat,
                       mapping = aes(x = period, y = age, fill = mean_upper)) +
      geom_tile() + ggtitle("Upper 95% CI boundary")
    
    gg_list <- list(gg_effect, gg_lower, gg_upper)
  }
  
  # add the cohort lines and some theme stuff
  for (i in 1:length(gg_list)) {
    gg_list[[i]] <- gg_list[[i]] +
      scale_fill_gradient2(trans  = y_trans, 
                           low    = "dodgerblue3", mid = "white", high = "firebrick3",
                           limits = limits_color) +
      theme + 
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_colourbar(barwidth = 17, barheight = 0.5)) +
      geom_segment(aes(x = x1[1], xend = x1[length(x1)], y = y1[1],
                       yend = y1[length(y1)])) +
      geom_text(aes(x = x1[length(x1)] - 6,
                    y = y1[length(y1)] - 4, label = "Cohort 1945 - 1949"), 
                angle = angle, size = 4) +
      geom_segment(aes(x = x2[1], xend = x2[length(x2)], y = y2[1],
                       yend = y2[length(y2)])) +
      geom_text(aes(x = x2[length(x2)] - 6,
                    y = y2[length(y2)] - 4, label = "Cohort 1960 - 1964"), 
                angle = angle, size = 4) +
      geom_segment(aes(x = x3[1], xend = x3[length(x3)], y = y3[1],
                       yend = y3[length(y3)])) +
      geom_text(aes(x = x3[length(x3)] - 6,
                    y = y3[length(y3)] - 4, label = "Cohort 1975 - 1979"), 
                angle = angle, size = 4) +
      geom_segment(aes(x = x4[1], xend = x4[length(x4)], y = y4[1],
                       yend = y4[length(y4)])) +
      geom_text(aes(x = x4[length(x4)] - 6,
                    y = y4[length(y4)] - 4, label = "Cohort 1990 - 1994"), 
                angle = angle, size = 4) +
      ylab("Age") + xlab("Period")
  }
  
  # join the three plots
  plots <- ggarrange(plotlist = gg_list, legend = "bottom",
                     common.legend = TRUE,
                     ncol = ifelse(plot_CI, 3, 1))
  
  return(plots)
}
