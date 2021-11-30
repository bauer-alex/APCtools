
#' Plot the density of one metric or categorical variable
#' 
#' Create a density plot or a boxplot of one metric variable or a barplot
#' of one categorical variable, based on a specific subset of the data.
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
#' @param age_groups,period_groups,cohort_groups Arguments to create a density
#' matrix based on \code{plot_density}. See \code{\link{plot_densityMatrix}}
#' for details. If specified, the final plot will be created based on a dataset
#' for which the age, period and/or cohort groups were first created, s.t.
#' faceting can eventually be applied in \code{plot_densityMatrix}.
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
plot_density <- function(dat, y_var, plot_type = "density", apc_range = NULL,
                         age_groups = NULL, period_groups = NULL, cohort_groups = NULL,
                         y_var_breaks = NULL, weights_var = NULL,
                         log_scale = FALSE, xlab = NULL, ylab = "Density",
                         legend_title = NULL, ...) {
  
  checkmate::check_data_frame(dat)
  checkmate::check_character(y_var, len = 1)
  checkmate::check_choice(plot_type, choices = c("density","boxplot"))
  checkmate::check_list(apc_range, types = "character", max.len = 3,
                        null.ok = TRUE, any.missing = FALSE)
  checkmate::check_subset(names(apc_range), choices = c("age","period","cohort"))
  checkmate::check_list(age_groups)
  checkmate::check_list(period_groups)
  checkmate::check_list(cohort_groups)
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

  
  # main plot
  if (is.numeric(dat[[y_var]])) { # metric variable
    gg <- plot_density_metric(dat           = dat,
                              y_var         = y_var,
                              plot_type     = plot_type,
                              age_groups    = age_groups, 
                              period_groups = period_groups,
                              cohort_groups = cohort_groups,
                              y_var_breaks  = y_var_breaks,
                              weights_var   = weights_var,
                              log_scale     = log_scale,
                              xlab          = xlab,
                              ylab          = ylab,
                              legend_title  = legend_title,
                              ...)
    
  } else { # categorical variable
    gg <- plot_density_categorical(dat         = dat,
                                   y_var       = y_var,
                                   weights_var = weights_var,
                                   xlab        = xlab,
                                   ylab        = ylab,
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
#' 
#' @import dplyr ggplot2
#' 
plot_density_metric <- function(dat, y_var, plot_type = "density", 
                                age_groups = NULL, period_groups = NULL, cohort_groups = NULL,
                                y_var_breaks = NULL, weights_var = NULL,
                                log_scale = FALSE,  xlab = NULL,
                                ylab = "Density", legend_title = NULL, ...) {
  
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
  
  # general plot preparations
  if (is.null(xlab)) {
    xlab <- ifelse(!log_scale, y_var, paste(y_var, "on log10 scale"))
  }
  
  # final plot type-specific preparations
  if (plot_type == "density") {
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
    
    # main plot
    gg <- ggplot(data = dat_dens, aes(x = x, y = y)) +
      geom_line(col = gray(0.3))
    
    if (!is.null(y_var_breaks)) {
      gg <- gg + geom_ribbon(aes(ymin = 0, ymax = y, fill = x_cat))
    } else {
      gg <- gg + geom_ribbon(aes(ymin = 0, ymax = y), fill = gray(0.3))
    }
    
    gg <- gg +
      ylab(ylab) + labs(fill = legend_title) + xlim(xlim) +
      scale_fill_brewer(palette = "Blues", direction = -1) +
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
    gg <- ggplot(dat, aes(x, weight = weight)) + geom_boxplot()
  }
  
  
  # general theme
  gg <- gg +
    xlab(xlab) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    scale_x_continuous(labels = label_function)
  
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