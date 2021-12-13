
#' Plot the density of one metric or categorical variable
#' 
#' Create a density plot or a boxplot of one metric variable or a barplot
#' of one categorical variable, based on a specific subset of the data.
#' 
#' If \code{plot_density} is called internally from within
#' \code{\link{plot_densityMatrix}} (i.e., if the dataset contains some of the
#' columns \code{c("age_group","period_group","cohort_group")}), this function
#' will calculate the metric densities individually for these groups.
#' 
#' @param dat Dataset with columns \code{period} and \code{age} and the
#' main variable specified through argument \code{y_var}.
#' @param y_var Character name of the main variable to be plotted.
#' @param plot_type One of \code{c("density","boxplot")}. Only used if the
#' \code{y_var} column is metric.
#' @param apc_range Optional list with one or multiple elements with names
#' \code{"age","period","cohort"} to filter the data. Each element should
#' contain a numeric vector of values for the respective variable that should
#' be kept in the data. All other values are deleted.
#' @param highlight_diagonals Optional internal parameter which is only
#' specified when \code{plot_density} is called from within
#' \code{plot_densityMatrix}. See \code{\link{plot_densityMatrix}} for details.
#' @param y_var_cat_breaks Optional numeric vector of breaks to categorize
#' \code{y_var} based on calling function \code{\link{cut}}. Only used to
#' highlight the categories based on different colors. And only used if the
#' \code{y_var} column is numeric.
#' @param y_var_cat_labels Optional character vector for the names of the
#' categories that were defined based on \code{y_var_cat_breaks}. The length of
#' this vector must be one shorter than \code{length(y_var_cat_breaks)}. Only
#' used if the \code{y_var} column is numeric.
#' @param weights_var Optional character name of a weights variable used to
#' project the results in the sample to some population.
#' @param log_scale Indicator if the main variable should be log10 transformed.
#' Only used if the \code{y_var} column is numeric. Defaults to FALSE.
#' @param xlab,ylab,legend_title Optional plot annotations.
#' @param ... Additional arguments passed to \code{\link[stats]{density}}.
#' 
#' @return ggplot object
#' 
#' @import checkmate dplyr
#' @export
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de},
#' Maximilian Weigert \email{maximilian.weigert@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' data(travel)
#' 
#' plot_density(dat = travel, y_var = "mainTrip_distance")
#' 
#' plot_density(dat = travel, y_var = "mainTrip_distance")
#' 
plot_density <- function(dat, y_var, plot_type = "density", apc_range = NULL,
                         highlight_diagonals = NULL,
                         y_var_cat_breaks = NULL, y_var_cat_labels = NULL,
                         weights_var = NULL, log_scale = FALSE, xlab = NULL,
                         ylab = "Density", legend_title = NULL, ...) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_character(y_var, len = 1)
  checkmate::assert_choice(plot_type, choices = c("density","boxplot"))
  checkmate::assert_list(apc_range, types = "numeric", max.len = 3,
                         null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_subset(names(apc_range), choices = c("age","period","cohort"))
  checkmate::assert_list(highlight_diagonals, types = "numeric", null.ok = TRUE)
  checkmate::assert_numeric(y_var_cat_breaks, null.ok = TRUE)
  checkmate::assert_character(y_var_cat_labels, len = length(y_var_cat_breaks) - 1,
                              null.ok = TRUE)
  checkmate::assert_character(weights_var, max.len = 1, null.ok = TRUE)
  checkmate::assert_logical(log_scale, len = 1)
  checkmate::assert_character(xlab, len = 1, null.ok = TRUE)
  checkmate::assert_character(ylab, len = 1, null.ok = TRUE)
  checkmate::assert_character(legend_title, max.len = 1, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  age <- period <- cohort <- NULL
  
  
  dat$cohort <- dat$period - dat$age
  
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

  # create a dataset to highlight specific diagonals
  if (!is.null(highlight_diagonals)) {
    dat_diag <- create_highlightDiagonalData(dat, highlight_diagonals)
  } else { dat_diag <- NULL }
  
  # main plot
  if (is.numeric(dat[[y_var]])) { # metric variable
    gg <- plot_density_metric(dat                    = dat,
                              y_var                  = y_var,
                              plot_type              = plot_type,
                              dat_highlightDiagonals = dat_diag,
                              y_var_cat_breaks       = y_var_cat_breaks,
                              y_var_cat_labels       = y_var_cat_labels,
                              weights_var            = weights_var,
                              log_scale              = log_scale,
                              xlab                   = xlab,
                              ylab                   = ylab,
                              legend_title           = legend_title,
                              ...)
    
  } else { # categorical variable
    gg <- plot_density_categorical(dat                    = dat,
                                   y_var                  = y_var,
                                   dat_highlightDiagonals = dat_diag,
                                   weights_var            = weights_var,
                                   xlab                   = xlab,
                                   ylab                   = ylab,
                                   ...)
  }
  
  return(gg)
}



#' Internal helper to plot a metric density
#' 
#' Internal helper function to plot one metric density, to be called from within
#' \code{\link{plot_density}}.
#' 
#' @inheritParams plot_density
#' @param dat_highlightDiagonals Optional dataset created by
#' \code{\link{create_highlightDiagonalData}} to highlight specific diagonals
#' in a density matrix.
#' 
#' @import dplyr ggplot2
#' 
plot_density_metric <- function(dat, y_var, plot_type = "density", 
                                dat_highlightDiagonals = NULL,
                                y_var_cat_breaks = NULL, y_var_cat_labels = NULL,
                                weights_var = NULL, log_scale = FALSE, xlab = NULL,
                                ylab = "Density", legend_title = NULL, ...) {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  x <- y <- x_cat <- weight <- NULL
  
  
  # delete potential NA's from y_var, since these mess with stats::density()
  dat <- dat[!is.na(dat[[y_var]]),]
  
  # log10 transform the main variable, and create a function to accordingly
  # adjust the labels on the x axis (the function is passed to scale_x_continuous())
  if (log_scale) {
    dat[[y_var]] <- log10(dat[[y_var]])
    if (!is.null(y_var_cat_breaks)) {
      y_var_cat_breaks <- log10(y_var_cat_breaks)
    }
    
    label_function <- function(x) { paste0("10^",x) }
    
  } else { # no log transformation
    label_function <- function(x) { x } # identity function
  }
  
  # general plot preparations
  if (is.null(xlab)) {
    # axis label, with capitalized first letter
    y_var_cap <- capitalize_firstLetter(y_var)
    xlab      <- ifelse(!log_scale, y_var_cap, paste(y_var_cap, "on log10 scale"))
  }
  
  # base plot
  gg <- ggplot()
  
  if (!is.null(dat_highlightDiagonals)) {
    gg <- gg_highlightDiagonals(gg, dat, dat_highlightDiagonals)
  }
  
  # final plot type-specific preparations
  if (plot_type == "density") {
    
    dat_dens <- calc_density(dat         = dat,
                             y_var       = y_var,
                             weights_var = weights_var,
                             ...)
    
    # categorize y_var
    if (!is.null(y_var_cat_breaks)) {
      dat_dens <- dat_dens %>% 
        mutate(x_cat = cut(x, breaks = y_var_cat_breaks,
                           labels = y_var_cat_labels, dig.lab = 6))
    }
    
    # final plot preparations
    xlim <- range(dat_dens$x)
    
    # main plot
    gg <- gg + geom_line(data = dat_dens, aes(x = x, y = y), col = gray(0.4))

    if (!is.null(y_var_cat_breaks)) {
      gg <- gg + geom_ribbon(data = dat_dens, aes(x = x, ymin = 0, ymax = y, fill = x_cat)) +
        scale_fill_brewer(palette = "Blues", direction = -1)
    } else {
      gg <- gg + geom_ribbon(data = dat_dens, aes(x = x, ymin = 0, ymax = y), fill = gray(0.4))
    }
    
    gg <- gg +
      ylab(ylab) + labs(fill = legend_title) +
      scale_x_continuous(xlab, labels = label_function, limits = xlim,
                         guide = guide_axis(check.overlap = TRUE)) +
      theme(legend.position = "bottom")
    
  } else { # plot_type == "boxplot"
    
    # preparations of the weights
    if (!is.null(weights_var)) {
      dat <- dat %>% dplyr::rename(weight = weights_var)
    } else {
      dat$weight <- 1
    }
    
    # rename the main variable for easier handling
    dat <- dat %>% dplyr::rename(x = y_var)
    
    # main plot
    gg <- gg + geom_boxplot(data = dat, aes(x = x, weight = weight), col = gray(0.3),
                            outlier.color = gray(0.3), outlier.alpha = 0.2) +
      scale_x_continuous(xlab, labels = label_function,
                         guide = guide_axis(check.overlap = TRUE)) +
      ylim(c(-1,1))
    
  }
  
  
  # general theme
  gg <- gg +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  return(gg)
}



#' Internal helper to plot a categorical density
#' 
#' Internal helper function to plot one categorical density, to be called from
#' within \code{\link{plot_density}}.
#' 
#' @inheritParams plot_density_metric
#' 
#' @import dplyr ggplot2
#' 
plot_density_categorical <- function(dat, y_var, dat_highlightDiagonals = NULL,
                                     weights_var = NULL, xlab = NULL,
                                     ylab = "Density") {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  x <- weight <- NULL
  
  
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
    # axis label, with capitalized first letter
    xlab <- capitalize_firstLetter(y_var)
  }
  
  # base plot
  gg <- ggplot()
  
  if (!is.null(dat_highlightDiagonals)) {
    gg <- gg_highlightDiagonals(gg, dat, dat_highlightDiagonals)
  }
  
  # main plot
  gg <- gg +
    geom_bar(data = dat, aes(x = x, y = ..count../sum(..count..),
                             weight = weight, fill = x)) +
    scale_fill_brewer(capitalize_firstLetter(y_var), palette = "Set2") +
    xlab(xlab) + ylab(ylab) +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())
  
  return(gg)
}
