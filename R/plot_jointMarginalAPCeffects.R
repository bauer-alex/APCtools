
#' Joint plot to compare the marginal APC effects of multiple models
#' 
#' This function creates a joint plot of the marginal APC effects of multiple
#' estimated models. It creates a plot with one pane per age, period and
#' cohort effect, each containing one lines for each estimated model.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' Since the plot output created by the function is no \code{ggplot2} object,
#' but an object created with \code{ggpubr::ggarrange}, the overall theme
#' of the plot cannot be changed by adding the theme in the form of
#' '\code{plot_jointMarginalAPCeffects(...) + theme_minimal(...)}'.
#' Instead, you can call \code{theme_set(theme_minimal(...))} as an individual
#' call before calling \code{plot_jointMarginalAPCeffects(...)}. The latter
#' function will then use this global plotting theme.
#' 
#' @inheritParams plot_APCheatmap
#' @param model_list A list of regression models estimated with
#' \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}. If the list is named, the
#' names are used as labels. Can also be a single model object instead of a list.
#' @param vlines_list Optional list that can be used to highlight the borders of
#' specific age groups, time intervals or cohorts. Each element must be a
#' numeric vector of values on the x-axis where vertical lines should be drawn.
#' The list can maximally have three elements and must have names out of
#' \code{c("age","period","cohort"}.
#' @param ylab,ylim Optional ggplot2 styling arguments.
#' @param plot_CI Indicator if 95% confidence intervals should be plotted.
#' Defaults to FALSE.
#' 
#' @return Plot grid created with \code{\link[ggpubr]{ggarrange}}.
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom ggpubr ggarrange
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
#' 
#' # plot marginal effects of one model
#' model_pure <- gam(mainTrip_distance ~ te(age, period), data = travel)
#' plot_jointMarginalAPCeffects(model_pure, dat = travel)
#' 
#' # plot marginal effects of multiple models
#' model_cov  <- gam(mainTrip_distance ~ te(age, period) + s(household_income),
#'                   data = travel)
#' model_list <- list("pure model"      = model_pure,
#'                    "covariate model" = model_cov)
#' plot_jointMarginalAPCeffects(model_list, dat = travel)
#' 
#' # mark specific cohorts
#' plot_jointMarginalAPCeffects(model_list, dat = travel,
#'                              vlines_list = list("cohort" = c(1966.5,1982.5,1994.5)))
#' 
plot_jointMarginalAPCeffects <- function(model_list, dat, vlines_list = NULL,
                                         ylab = NULL, ylim = NULL,
                                         plot_CI = FALSE) {
  
  checkmate::assert_choice(class(model_list)[1], choices = c("list","gam"))
  if (class(model_list)[1] == "list") {
    checkmate::assert_list(model_list, types = "gam")
  }
  checkmate::assert_data_frame(dat)
  checkmate::assert_list(vlines_list, min.len = 1, max.len = 3,
                         types = "numeric", null.ok = TRUE)
  checkmate::assert_subset(names(vlines_list), choices = c("age","period","cohort"))
  checkmate::assert_character(ylab, len = 1, null.ok = TRUE)
  checkmate::assert_numeric(ylim, len = 2, null.ok = TRUE)
  checkmate::assert_logical(plot_CI)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  effect <- type <- value <- lower <- upper <- NULL
  
  
  # reformat 'model_list' to a list, if only one model object was specified
  if (class(model_list)[1] == "gam") {
    model_list <- list(model_list)
  }
  
  # retrieve model labels
  if (!is.null(names(model_list))) {
    model_labels <- names(model_list)
  } else {
    model_labels <- paste("model", 1:length(model_list))
  }
  
  # retrieve datasets with the marginal effects
  datList_list <- lapply(model_list, function(x) {
    plot_marginalAPCeffects(x, dat, plot_CI = plot_CI, return_plotData = TRUE)
  })
  
  if (is.null(ylim)) {
    if (plot_CI == TRUE) {
      ylim <- lapply(datList_list, function(x) { dplyr::bind_rows(x) }) %>% 
        dplyr::bind_rows() %>% select(effect, lower, upper) %>%
        range(na.rm = TRUE)
    }
    else {
      ylim <- lapply(datList_list, function(x) { dplyr::bind_rows(x) }) %>% 
        dplyr::bind_rows() %>% pull(effect) %>% range()
    }
  }
  
  used_logLink <- (model_list[[1]]$family[[2]] %in% c("log","logit")) |
    grepl("Ordered Categorical", model_list[[1]]$family[[1]])
  if (is.null(ylab)) {
    ylab <- ifelse(used_logLink, "exp(Effect)", "Effect")
  }
  
  # base plots
  gg_age <- gg_period <- gg_cohort <- ggplot()
  
  # marginal age effect
  dat_age <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_age %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows() %>% mutate(type = factor(type, levels = model_labels))
  
  if ("age" %in% names(vlines_list)) {
    gg_age <- gg_age + 
      geom_vline(xintercept = vlines_list$age, col = gray(0.5), lty = 2)
  }
  
  gg_age <- gg_age +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2)
  
  if (plot_CI == TRUE) {
    gg_age <- gg_age +
      geom_ribbon(data = dat_age,
                  mapping = aes(x = value, ymin = lower, ymax = upper,
                                fill = type), alpha = 0.2)
  }
  gg_age <- gg_age +
    geom_line(data = dat_age, aes(x = value, y = effect, col = type)) +
    xlab("Age") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank())
  
  # marginal period effect
  dat_period <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_period %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows() %>% mutate(type = factor(type, levels = model_labels))
  
  if ("period" %in% names(vlines_list)) {
    gg_period <- gg_period + 
      geom_vline(xintercept = vlines_list$period, col = gray(0.5), lty = 2)
  }
  
  gg_period <- gg_period +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2)
  
  if (plot_CI == TRUE) {
    gg_period <- gg_period +
      geom_ribbon(data = dat_period,
                  mapping = aes(x = value, ymin = lower, ymax = upper,
                                fill = type), alpha = 0.2)
  }
  gg_period <- gg_period +
    geom_line(data = dat_period, aes(x = value, y = effect, col = type)) +
    xlab("Period") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  # marginal cohort effect
  dat_cohort <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_cohort %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows() %>% mutate(type = factor(type, levels = model_labels))
  
  if ("cohort" %in% names(vlines_list)) {
    gg_cohort <- gg_cohort + 
      geom_vline(xintercept = vlines_list$cohort, col = gray(0.5), lty = 2)
  }
  
  gg_cohort <- gg_cohort +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2)
  
  if (plot_CI == TRUE) {
    gg_cohort <- gg_cohort +
      geom_ribbon(data = dat_cohort,
                  mapping = aes(x = value, ymin = lower, ymax = upper,
                                fill = type), alpha = 0.2)
  }
  gg_cohort <- gg_cohort +
    geom_line(data = dat_cohort, aes(x = value, y = effect, col = type)) +
    xlab("Cohort") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  # no color coding when only one model is plotted
  if (length(model_list) == 1) {
    gg_age    <- gg_age    + scale_color_manual(values = gray(0.2))
    gg_period <- gg_period + scale_color_manual(values = gray(0.2))
    gg_cohort <- gg_cohort + scale_color_manual(values = gray(0.2))
  }
  
  # joint plot
  ggpubr::ggarrange(plotlist      = list(gg_age, gg_period, gg_cohort),
                    legend        = ifelse(length(model_list) == 1, "none", "bottom"),
                    common.legend = TRUE,
                    ncol          = 3,
                    widths        = c(0.36, 0.32, 0.32))
  
}