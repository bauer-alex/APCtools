
#' Joint plot to compare the marginal APC effects of multiple models
#' 
#' This function creates a joint plot of the marginal APC effects of multiple
#' estimated models. It creates a plot with one pane per age, period and
#' cohort effect, each containing one lines for each estimated model.
#' 
#' If the model was estimated with a log or logit link, the function
#' automatically performs an exponential transformation of the effect.
#' 
#' @inheritParams plot_APCheatmap
#' @param model_list A list of regression models estimated with
#' \code{\link[mgcv]{gam}}. If the list is named, the names are used as labels.
#' @param vlines_list Optional list that can be used to highlight the borders of
#' specific age groups, time intervals or cohorts. Each element must be a
#' numeric vector of values on the x-axis where vertical lines should be drawn.
#' The list can maximally have three elements and must have names out of
#' \code{c("age","period","cohort"}.
#' @param ylab Optional title for the y-axis.
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
#' model_pure <- gam(mainTrip_distance ~ te(age, period), data = travel)
#' model_cov  <- gam(mainTrip_distance ~ te(age, period) + s(household_income),
#'                   data = travel)
#' 
#' model_list <- list("pure model"      = model_pure,
#'                    "covariate model" = model_cov)
#' 
#' plot_jointMarginalAPCeffects(model_list, dat = travel)
#' 
#' # mark specific cohorts
#' plot_jointMarginalAPCeffects(model_list, dat = travel,
#'                              vlines_list = list("cohort" = c(1966.5,1982.5,1994.5)))
#' 
plot_jointMarginalAPCeffects <- function(model_list, dat, vlines_list = NULL,
                                         ylab = NULL) {
  
  checkmate::assert_list(model_list, types = "gam")
  checkmate::assert_data_frame(dat)
  checkmate::assert_list(vlines_list, min.len = 1, max.len = 3,
                         types = "numeric", null.ok = TRUE)
  checkmate::assert_subset(names(vlines_list), choices = c("age","period","cohort"))
  checkmate::assert_character(ylab, len = 1, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  effect <- type <- value <- NULL
  
  
  # retrieve model labels
  if (!is.null(names(model_list))) {
    model_labels <- names(model_list)
  } else {
    model_labels <- paste("model", 1:length(model_list))
  }
  
  # retrieve datasets with the marginal effects
  datList_list <- lapply(model_list, function(x) {
    plot_marginalAPCeffects(x, dat, return_plotData = TRUE)
  })
  
  ylim <- lapply(datList_list, function(x) { dplyr::bind_rows(x) }) %>% 
    dplyr::bind_rows() %>% pull(effect) %>% range()
  used_logLink <- model_list[[1]]$family[[2]] %in% c("log","logit")
  if (is.null(ylab)) {
    ylab <- ifelse(used_logLink, "Odds Ratio", "Effect")
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
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
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
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
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
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
    geom_line(data = dat_cohort, aes(x = value, y = effect, col = type)) +
    xlab("Cohort") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  # joint plot
  ggpubr::ggarrange(plotlist = list(gg_age, gg_period, gg_cohort),
                    legend = "bottom", common.legend = TRUE,
                    ncol = 3, widths = c(0.36, 0.32, 0.32))
  
}