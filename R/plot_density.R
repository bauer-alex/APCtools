
#' Plot the density of one metric or categorical variable
#' 
#' Create a density plot of one metric or categorical variable, based on a
#' specific subset of the data.
#' 
#' @param dat Dataset with columns \code{period} and \code{age} and the
#' main variable specified through argument \code{yvar}.
#' @param y_var Character name of the main variable to be plotted.
#' @param apc_range Optional list with one or multiple elements with names
#' \code{"age","period","cohort"} to filter the data. Each element should
#' contain a numeric vector of values for the respective variable that should
#' be kept in the data. All other values are deleted.
#' @param y_var_breaks Optional numeric vector of breaks to categorize
#' \code{y_var} based on calling function \code{\link{cut}}. Only used to
#' highlight the categories based on different colors. And only used if the
#' \code{y_var} column is numeric.
#' @param weights_var Optional character name of a weights variable used to
#' project the results in the sample to some population.
#' @param log_scale Indicator if the main variable should be log10 transformed.
#' Only used if the \code{y_var} column is numeric.
#' @param xlab,ylab,legend_title Optional plot annotations.
#' @param ... Additional arguments passed to \code{\link[stats]{density}}.
#' 
#' @import checkmate dplyr
#' @export
#' 
plot_density <- function(dat, y_var, apc_range = NULL, y_var_breaks = NULL,
                         weights_var = NULL, log_scale = FALSE, xlab = NULL,
                         ylab = "Density", legend_title = NULL, ...) {
  
  checkmate::check_data_frame(dat)
  checkmate::check_character(y_var, max.len = 1)
  checkmate::check_list(apc_range, types = "character", max.len = 3,
                        null.ok = TRUE, any.missing = FALSE)
  checkmate::check_subset(names(apc_range), choices = c("age","period","cohort"))
  checkmate::check_numeric(y_var_breaks, lower = 1, null.ok = TRUE)
  checkmate::check_character(weights_var, max.len = 1, null.ok = TRUE)
  checkmate::check_logical(log_scale, len = 1)
  checkmate::check_character(xlab, len = 1, null.ok = TRUE)
  checkmate::check_character(ylab, len = 1, null.ok = TRUE)
  checkmate::check_character(legend_title, max.len = 1, null.ok = TRUE)
  
  dat$cohort <- dat$period - dat$age
  
  # categorize the numeric response variable
  if (!is.null(y_var_breaks) & is.numeric(dat[,y_var])) {
    dat[,paste0(y_var, "_cat")] <- cut(dat[,y_var], breaks = y_var_breaks)
  }
  
  # filter the dataset
  if (!is.null(apc_range)) {
    if (!is.null(apc_range$age)) {
      dat <- dat %>% filter(age %in% apc_range$age)
    }
    if (!is.null(apc_range$period)) {
      dat <- dat %>% filter(period %in% apc_range$period)
    }
    if (!is.null(apc_range$cohort)) {
      dat <- dat %>% filter(cohort %in% apc_range$cohort)
    }
  }

  
  # Matrix of densities:
  # if (multiple == TRUE) {
  #   if (!is.null(age)) {
  #     ages <- sort(unique(dat$age))
  #   }
  #   if (!is.null(period)) {
  #     periods <- sort(unique(dat$period))
  #   }
  #   if (!is.null(cohort)) {
  #     cohorts <- sort(unique(dat$cohort))
  #   }
  #   if (is.null(age)) {
  #     grid <- expand.grid(periods, cohorts)
  #     colnames(grid) <- c("period", "cohort")
  #   }
  #   if (is.null(period)) {
  #     grid <- expand.grid(ages, cohorts)
  #     colnames(grid) <- c("age", "cohort")
  #   }
  #   if (is.null(cohort)) {
  #     grid <- expand.grid(ages, periods)
  #     colnames(grid) <- c("age", "period")
  #   }
  #   dens_list <- lapply(X = 1:nrow(grid), FUN = function(index) {
  #     if (is.null(age)) {
  #       dat_dens_index <- dat %>% filter(period == grid[index, "period"]) %>%
  #         filter(cohort == grid[index, "cohort"])
  #     }
  #     if (is.null(period)) {
  #       dat_dens_index <- dat %>% filter(age == grid[index, "age"]) %>%
  #         filter(cohort == grid[index, "cohort"])
  #     }
  #     if (is.null(cohort)) {
  #       dat_dens_index <- dat %>% filter(age == grid[index, "age"]) %>%
  #         filter(period == grid[index, "period"])
  #     }
  #     
  #     if (!is.null(weights_var)) {
  #       density <- stats::density(x       = dat_dens_index[,y_var],
  #                                 weights = dat_dens_index[,weights_var], ...)
  #     } else {
  #       density <- stats::density(x = dat_dens_index$travel_distance, ...)
  #     }
  #     density <- data.frame(x = density$x, y = density$y, var1 = grid[index, 1],
  #                           var2 = grid[index, 2])
  #     return(density)
  #   })
  #   dat_dens <- bind_rows(dens_list)
  #   if (is.null(age)) {
  #     colnames(dat_dens)[3:4] <- c("period", "cohort")
  #   }
  #   if (is.null(period)) {
  #     colnames(dat_dens)[3:4] <- c("age", "cohort")
  #   }
  #   if (is.null(cohort)) {
  #     colnames(dat_dens)[3:4] <- c("age", "period")
  #   }
  # }
  
  # if (multiple == TRUE) {
  #   dat_dens <- dat_dens %>%
  #     mutate(y = 5 * y,
  #            cohort = as.numeric(substr(x = as.character(dat_dens$period),
  #                                       start = 1, stop = 4)) -
  #              as.numeric(substr(x = as.character(dat_dens$age),
  #                                start = 1, stop = 2)),
  #            cohort_group = case_when(cohort %in% 1950:1959 ~ "born 1950 - 1959", 
  #                                     cohort %in% 1970:1979 ~ "born 1970 - 1979", 
  #                                     TRUE ~ "other cohorts"),
  #            cohort_group = factor(cohort_group),
  #            fill_variable = factor(paste0(cohort_group, " - ", travel_distance_cat),
  #                                   levels = paste0(rep(sort(unique(cohort_group)),
  #                                                       each = length(labels)),
  #                                                   " - ",
  #                                                   rep(labels, times = length(unique(cohort_group))))))
  # }
  
  # Actual plotting:
  # theme <- theme(text = element_text(size = 18), axis.title = element_text(size = 18),
  #                axis.text = element_text(size = 14),
  #                legend.text = element_text(size = 16),
  #                legend.title = element_text(size = 18),
  #                plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
  #                strip.text.y = element_text(size = 16),
  #                strip.text.x = element_text(size = 16), legend.text.align = 0,
  #                strip.placement = "outside", strip.background = element_blank(),
  #                axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
  #                axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
  
  if (is.numeric(dat[[y_var]])) { # metric variable
    gg <- plot_density_metric(dat, y_var, y_var_breaks, weights_var, log_scale,
                              xlab, ylab, legend_title, ...)
  } else { # categorical variable
    gg <- plot_density_categorical(dat, y_var, weights_var, xlab, ylab, ...)
  }
  
  
  # if (multiple == TRUE) {
  #   n_cat <- length(labels)
  #   gg <- ggplot(data = dat_dens, mapping = aes(x = x, y = y)) +
  #     geom_line() + xlim(xlim) + theme +
  #     geom_ribbon(aes(ymin = 0, ymax = y, fill = fill_variable)) +
  #     scale_fill_manual(values = c((RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG")[1:n_cat]),
  #                                  rev(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG"))[1:n_cat],
  #                                  gray(level = seq(0.4, 0.9, length.out = n_cat))),
  #                       guide = "none") +
  #     geom_line(data    = data.frame(x = rep(1:n_cat,          each = 3), 
  #                                    y = rep(LETTERS[1:3],     each = n_cat),
  #                                    z = rep(LETTERS[1:n_cat], each = 3)),
  #               mapping = aes(x = x, y = x, color = y, alpha = z), size = 6)   +
  #     scale_color_manual(values = c((RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG")[1:n_cat])[n_cat-3],
  #                                   (rev(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG"))[1:n_cat])[n_cat-3],
  #                                   "gray55"),
  #                        labels = levels(dat_dens$cohort_group),
  #                        name   = "Cohort") +
  #     scale_alpha_manual(values = seq(0.6, 0.1, length.out = n_cat),
  #                        labels = labels,
  #                        name   = "Distance category")
  # }
  return(gg)
}



#' Internal helper to plot a metric density
#' 
#' Internal helper function to plot one metric density, to be called from within
#' \code{\link{plot_density}}.
#' 
#' @inheritParams plot_density
#' 
#' @import dplyr ggplot2
#' 
plot_density_metric <- function(dat, y_var, y_var_breaks = NULL,
                                weights_var = NULL, log_scale = FALSE, 
                                xlab = NULL, ylab = "Density",
                                legend_title = NULL, ...) {
  
  # log10 transform the main variable, and create a function to accordingly
  # adjust the labels on the x axis (the function is passed to scale_x_continuous())
  if (log_scale) {
    dat[[y_var]] <- log10(dat[[y_var]])
    if (!is.null(y_var_breaks)) {
      y_var_breaks <- log10(y_var_breaks)
    }
    
    label_function <- function(x) { paste0("10^",x) }
    
  } else { # no log transformation
    label_function <- function(x) { x } # identity function
  }
  
  # calculate the density
  weights_vector <- NULL
  if (!is.null(weights_var)) {
    # make sure the weights are not NA
    if (any(is.na(dat[[weights_var]]))) {
      warning("Deleting ",sum(is.na(dat[[weights_var]])), " observations where the weights variable is NA.")
      dat <- dat[!is.na(dat[[weights_var]]),]
    }
    
    weights_vector <- dat[[weights_var]]
  }
  dens <- stats::density(x       = dat[[y_var]],
                         weights = weights_vector, ...)
  dat_dens <- data.frame(x = dens$x, y = dens$y)
  
  # categorize y_var
  if (!is.null(y_var_breaks)) {
    dat_dens <- dat_dens %>% 
      mutate(x_cat = cut(x, breaks = y_var_breaks, dig.lab = 6))
  }
  
  # final plot preparations
  xlim <- range(dat_dens$x)
  if (is.null(xlab)) {
    xlab <- ifelse(!log_scale, y_var, paste(y_var, "on log10 scale"))
  }
  
  # main plot
  gg <- ggplot(data = dat_dens, aes(x = x, y = y)) +
    geom_line(col = gray(0.3))
  
  if (!is.null(y_var_breaks)) {
    gg <- gg + geom_ribbon(aes(ymin = 0, ymax = y, fill = x_cat))
  } else {
    gg <- gg + geom_ribbon(aes(ymin = 0, ymax = y), fill = gray(0.3))
  }
  
  gg <- gg +
    xlab(xlab) + ylab(ylab) + labs(fill = legend_title) + xlim(xlim) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    scale_x_continuous(labels = label_function) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(legend.position = "bottom")
  
  return(gg)
}



#' Internal helper to plot a categorical density
#' 
#' Internal helper function to plot one categorical density, to be called from
#' within \code{\link{plot_density}}.
#' 
#' @inheritParams plot_density
#' 
#' @import dplyr ggplot2
#' 
plot_density_categorical <- function(dat, y_var, weights_var = NULL,
                                     xlab = NULL, ylab = "Density") {
  
  # make sure the main variable is encoded as factor
  dat <- dat %>% dplyr::rename(x = y_var) %>% mutate(x = factor(x))
  
  # preparations of the weights
  if (!is.null(weights_var)) {
    dat <- dat %>% dplyr::rename(weight = weights_var)
  } else {
    dat$weight <- 1
  }
  
  # final plot preparations
  if (is.null(xlab)) {
    xlab <- y_var
  }
  
  # main plot
  gg <- ggplot(data = dat, aes(x = x, y = ..count../sum(..count..), weight = weight)) +
    geom_bar(fill = gray(0.3)) +
    xlab(xlab) + ylab(ylab)
  
  return(gg)
}