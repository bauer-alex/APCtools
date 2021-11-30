
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
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom ggpubr ggarrange
#' @export
#' 
plot_jointMarginalAPCEffects <- function(model_list, dat) {
  
  checkmate::assert_list(model_list, types = "gam")
  checkmate::assert_data_frame(dat)
  
  
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
  ylab <- ifelse(used_logLink, "Odds Ratio", "Effect")
  
  # marginal age effect
  dat_age <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_age %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows()
  gg_age <- ggplot(dat_age, aes(x = value, y = effect, col = type)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
    geom_line() + xlab("Age") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank())
  
  # marginal period effect
  dat_period <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_period %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows()
  gg_period <- ggplot(dat_period, aes(x = value, y = effect, col = type)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
    geom_line() + xlab("Period") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  # marginal cohort effect
  dat_cohort <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_cohort %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows()
  gg_cohort <- ggplot(dat_cohort, aes(x = value, y = effect, col = type)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
    geom_line() + xlab("Cohort") +
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