
#' Create a ridgeline plot
#' 
#' Create a ridgeline plot for a specific subset of the data.
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
#' @param multiple TODO
#' @param xlab,ylab,legend_title Optional plot annotations.
#' @param ... Additional arguments passed to \code{\link[stats]{density}}.
#' 
#' @import checkmate dplyr
#' @export
#' 
plot_ridgeline <- function(dat, y_var, apc_range = NULL, y_var_breaks = NULL,
                           weights_var = NULL, multiple = FALSE, xlab = y_var,
                           ylab = "Density", legend_title = NULL, ...) {
  
  checkmate::check_data_frame(dat)
  checkmate::check_character(y_var, max.len = 1)
  checkmate::check_list(apc_range, types = "character", max.len = 3,
                        null.ok = TRUE, any.missing = FALSE)
  checkmate::check_subset(names(apc_range), choices = c("age","period","cohort"))
  checkmate::check_character(weights_var, max.len = 1, null.ok = TRUE)
  checkmate::check_logical(multiple, max.len = 1, null.ok = TRUE)
  checkmate::check_character(xlab, max.len = 1)
  checkmate::check_character(ylab, max.len = 1)
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

  ########### TODO Ab hier muss Funktion noch überarbeitet werden
  
    
  # Calculation of densities:
  # Single density;
  if (multiple == FALSE) {
    
    if (!is.null(weights_var)) {
      dens <- stats::density(x       = dat[,y_var],
                      weights = dat[,weights_var], ...)
    } else {
      dens <- stats::density(x = dat[,y_var], ...)
    }
    dat_dens <- data.frame(x = dens$x, y = dens$y)
  }
  # Matrix of densities:
  if (multiple == TRUE) {
    if (!is.null(age)) {
      ages <- sort(unique(dat$age))
    }
    if (!is.null(period)) {
      periods <- sort(unique(dat$period))
    }
    if (!is.null(cohort)) {
      cohorts <- sort(unique(dat$cohort))
    }
    if (is.null(age)) {
      grid <- expand.grid(periods, cohorts)
      colnames(grid) <- c("period", "cohort")
    }
    if (is.null(period)) {
      grid <- expand.grid(ages, cohorts)
      colnames(grid) <- c("age", "cohort")
    }
    if (is.null(cohort)) {
      grid <- expand.grid(ages, periods)
      colnames(grid) <- c("age", "period")
    }
    dens_list <- lapply(X = 1:nrow(grid), FUN = function(index) {
      if (is.null(age)) {
        dat_dens_index <- dat %>% filter(period == grid[index, "period"]) %>%
          filter(cohort == grid[index, "cohort"])
      }
      if (is.null(period)) {
        dat_dens_index <- dat %>% filter(age == grid[index, "age"]) %>%
          filter(cohort == grid[index, "cohort"])
      }
      if (is.null(cohort)) {
        dat_dens_index <- dat %>% filter(age == grid[index, "age"]) %>%
          filter(period == grid[index, "period"])
      }
      
      if (!is.null(weights_var)) {
        density <- stats::density(x       = dat_dens_index[,y_var],
                                  weights = dat_dens_index[,weights_var], ...)
      } else {
        density <- stats::density(x = dat_dens_index$travel_distance, ...)
      }
      density <- data.frame(x = density$x, y = density$y, var1 = grid[index, 1],
                            var2 = grid[index, 2])
      return(density)
    })
    dat_dens <- bind_rows(dens_list)
    if (is.null(age)) {
      colnames(dat_dens)[3:4] <- c("period", "cohort")
    }
    if (is.null(period)) {
      colnames(dat_dens)[3:4] <- c("age", "cohort")
    }
    if (is.null(cohort)) {
      colnames(dat_dens)[3:4] <- c("age", "period")
    }
  }
  dat_dens <- dat_dens %>%
    mutate(travel_distance = x,
           travel_distance_cat =
             case_when(travel_distance < thresholds[1] ~ labels[1],
                       travel_distance < thresholds[2] ~ labels[2],
                       travel_distance < thresholds[3] ~ labels[3],
                       travel_distance < thresholds[4] ~ labels[4],
                       TRUE ~ labels[5]),
           travel_distance_cat = factor(travel_distance_cat, levels = labels))
  if (multiple == TRUE) {
    dat_dens <- dat_dens %>%
      mutate(y = 5 * y,
             cohort = as.numeric(substr(x = as.character(dat_dens$period),
                                        start = 1, stop = 4)) -
               as.numeric(substr(x = as.character(dat_dens$age),
                                 start = 1, stop = 2)),
             cohort_group = case_when(cohort %in% 1950:1959 ~ "born 1950 - 1959", 
                                      cohort %in% 1970:1979 ~ "born 1970 - 1979", 
                                      TRUE ~ "other cohorts"),
             cohort_group = factor(cohort_group),
             fill_variable = factor(paste0(cohort_group, " - ", travel_distance_cat),
                                    levels = paste0(rep(sort(unique(cohort_group)),
                                                        each = length(labels)),
                                                    " - ",
                                                    rep(labels, times = length(unique(cohort_group))))))
  }
  xlim <- range(dat_dens$x)
  
  # Actual plotting:
  theme <- theme_minimal() +
    theme(text = element_text(size = 18), axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          strip.text.y = element_text(size = 16),
          strip.text.x = element_text(size = 16), legend.text.align = 0,
          strip.placement = "outside", strip.background = element_blank(),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
  if (multiple == FALSE) {
    plot <- ggplot(data = dat_dens, aes(x = x, y = y)) +
      geom_line() +
      geom_ribbon(aes(ymin = 0, ymax = y, fill = travel_distance_cat)) +
      xlab(xlab) + ylab(ylab) + labs(fill = legend_title) + xlim(xlim) +
      scale_fill_brewer(palette = "Blues", direction = -1) + theme +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
      guides(fill=guide_legend(nrow = 2)) + theme(legend.position = "bottom")
  }
  if (multiple == TRUE) {
    n_cat <- length(labels)
    plot <- ggplot(data = dat_dens, mapping = aes(x = x, y = y)) +
      geom_line() + xlim(xlim) + theme +
      geom_ribbon(aes(ymin = 0, ymax = y, fill = fill_variable)) +
      scale_fill_manual(values = c((RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG")[1:n_cat]),
                                   rev(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG"))[1:n_cat],
                                   gray(level = seq(0.4, 0.9, length.out = n_cat))),
                        guide = "none") +
      geom_line(data    = data.frame(x = rep(1:n_cat,          each = 3), 
                                     y = rep(LETTERS[1:3],     each = n_cat),
                                     z = rep(LETTERS[1:n_cat], each = 3)),
                mapping = aes(x = x, y = x, color = y, alpha = z), size = 6)   +
      scale_color_manual(values = c((RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG")[1:n_cat])[n_cat-3],
                                    (rev(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info["BrBG", "maxcolors"], "BrBG"))[1:n_cat])[n_cat-3],
                                    "gray55"),
                         labels = levels(dat_dens$cohort_group),
                         name   = "Cohort") +
      scale_alpha_manual(values = seq(0.6, 0.1, length.out = n_cat),
                         labels = labels,
                         name   = "Distance category")
  }
  return(plot)
}
