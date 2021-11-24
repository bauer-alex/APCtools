
#' Joint plot to compare the marginal APC effects of two models
#' 
#' This function creates a joint plot of the marginal APC effects of two
#' estimated models. It creates a plot with one pane per age, period and
#' cohort effect, each containing two lines for the two estimated models.
#' 
#' If the model was estimated with a log link, the function automatically
#' performs an exponential transformation of the effect.
#' 
#' @inheritParams plot_partialAPCeffects
#' @param model1,model2 Two regression models estimated with
#' \code{\link[mgcv]{gam}}.
#' @param model_labels Character vector containing the labels for the two models.
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom ggpubr ggarrange
#' @export
#' 
plot_jointMarginalAPCEffects <- function(model1, model2, dat,
                                         model_labels = c("model 1", "model 2")) {
  
  checkmate::check_class(model1, classes = "gam")
  checkmate::check_class(model2, classes = "gam")
  checkmate::check_data_frame(dat)
  checkmate::check_character(model_labels, len = 2)
  
  
  # retrieve datasets with the marginal effects
  datList1 <- plot_marginalAPCeffects(model1, dat, return_plotData = TRUE)
  datList2 <- plot_marginalAPCeffects(model2, dat, return_plotData = TRUE)
  
  ylim <- dplyr::bind_rows(c(datList1, datList2)) %>% pull(effect) %>% range()
  used_logLink <- (model1$family[[2]] == "log" | model1$family[[2]] == "logit")
  ylab <- ifelse(used_logLink, "Odds Ratio", "Effect")
  
  # marginal age effect
  dat_age2 <- datList2$dat_age %>% mutate(type = model_labels[2])
  dat_age  <- datList1$dat_age %>% mutate(type = model_labels[1]) %>% 
    dplyr::bind_rows(dat_age2)
  gg_age <- ggplot(dat_age, aes(x = value, y = effect, lty = type)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = "firebrick2", lty = 2) +
    geom_line() + xlab("Age") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank())
  
  # marginal period effect
  dat_period2 <- datList2$dat_period %>% mutate(type = model_labels[2])
  dat_period  <- datList1$dat_period %>% mutate(type = model_labels[1]) %>% 
    dplyr::bind_rows(dat_period2)
  gg_period <- ggplot(dat_period, aes(x = value, y = effect, lty = type)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = "firebrick2", lty = 2) +
    geom_line() + xlab("Period") +
    scale_y_continuous(trans = ifelse(used_logLink, "log2", "identity"),
                       name  = ylab, limits = ylim) +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  # marginal cohort effect
  dat_cohort2 <- datList2$dat_cohort %>% mutate(type = model_labels[2])
  dat_cohort  <- datList1$dat_cohort %>% mutate(type = model_labels[1]) %>% 
    dplyr::bind_rows(dat_cohort2)
  gg_cohort <- ggplot(dat_cohort, aes(x = value, y = effect, lty = type)) +
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = "firebrick2", lty = 2) +
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