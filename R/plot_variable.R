
#' Distribution plot of one variable against one APC dimension
#' 
#' Plot the distribution of one variable in the data against age, period or
#' cohort. Creates a bar plot for categorical variables (see argument
#' \code{geomBar_position}) and boxplots or a line plot of median values for
#' metric variables (see \code{plot_type}).
#' 
#' @param dat Dataset containing columns \code{age} and \code{period}.
#' @param y_var Character name of the variable to plot.
#' @param apc_dimension One of \code{c("age","period","cohort")}. Defaults to
#' \code{"period"}.
#' @param log_scale Indicator if the visualized variable should be log10
#' transformed. Only used if the variable is numeric. Defaults to FALSE.
#' @param plot_type One of \code{c("boxplot","line","line-points")}, specifying
#' if boxplots or a line plot of median values should be drawn for metric
#' variables. \code{"line-points"} adds points to the line plot where
#' observations are available.
#' @param geomBar_position Value passed to \code{\link[ggplot2]{geom_bar}} as
#' \code{position} argument. Only used if the visualized variable is categorical.
#' Defaults to \code{"fill"}.
#' @param legend_title Optional character title for the legend which is drawn
#' for categorical variables.
#' @param ylab,ylim Optional arguments for styling the ggplot.
#' 
#' @return ggplot object
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom stats median
#' @export
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' data(travel)
#' 
#' # plot a metric variable
#' plot_variable(dat = travel, y_var = "mainTrip_distance",
#'               apc_dimension = "period", log_scale = TRUE)
#' plot_variable(dat = travel, y_var = "mainTrip_distance",
#'               apc_dimension = "period", log_scale = TRUE, plot_type = "line")
#' 
#' # plot a categorical variable
#' plot_variable(dat = travel, y_var = "household_size", apc_dimension = "period")
#' plot_variable(dat = travel, y_var = "household_size", apc_dimension = "period",
#'               geomBar_position = "stack")
#' 
plot_variable <- function(dat, y_var, apc_dimension = "period",
                          log_scale = FALSE, plot_type = "boxplot",
                          geomBar_position = "fill", legend_title = NULL,
                          ylab = NULL, ylim = NULL) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_choice("age", colnames(dat))
  checkmate::assert_choice("period", colnames(dat))
  checkmate::assert_choice(y_var, choices = colnames(dat))
  checkmate::assert_choice(apc_dimension, choices = c("age","period","cohort"))
  checkmate::assert_logical(log_scale, len = 1)
  checkmate::assert_choice(plot_type, choices = c("boxplot","line",
                                                  "line-points"), null.ok = TRUE)
  checkmate::assert_character(geomBar_position, len = 1)
  checkmate::assert_character(legend_title, len = 1, null.ok = TRUE)
  checkmate::assert_character(ylab, len = 1, null.ok = TRUE)
  checkmate::assert_numeric(ylim, len = 2, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  period <- age <- x <- y <- NULL
  
  
  dat <- dat %>% mutate(cohort = period - age)
  
  var_class <- ifelse(class(dat[[y_var]]) %in% c("character","factor"),
                      "categorical", "metric")
  
  # remove NA measurements
  if (any(is.na(dat[[y_var]]))) {
    message("Excluding ",sum(is.na(dat[[y_var]])), " missing observations of ",y_var,"...")
    dat <- dat[!is.na(dat[[y_var]]),]
  }
  
  # rename the variables for easier handling
  dat <- dat %>% dplyr::rename(x = apc_dimension,
                               y = y_var)
  
  # create plot
  if (var_class == "categorical") {
    
    if (is.null(ylab)) {
      ylab <- ifelse(geomBar_position == "fill", "Rel. frequency", "Frequency")
    }
    if (is.null(legend_title)) {
      legend_title <- y_var
    }
    
    gg <- ggplot(dat, aes(x = factor(x), fill = y)) + 
      geom_bar(position = geomBar_position) +
      scale_fill_brewer(legend_title, palette = "Set2") +
      scale_y_continuous(ylab, limits = ylim) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    
  } else { # var_class == "metric"
    
    # compute the median values
    if (plot_type %in% c("line","line-points")) {
      dat <- dat %>% 
        group_by(x) %>% 
        summarize(y = stats::median(y, na.rm = TRUE)) %>% 
        ungroup()
    }
    
    # log10 transform the main variable, and create a function to accordingly
    # adjust the labels on the y axis (the function is passed to scale_y_continuous())
    if (log_scale) {
      dat            <- dat %>% mutate(y = log10(y))
      label_function <- function(x) { paste0("10^",x) }
      
    } else { # no log transformation
      label_function <- function(x) { x } # identity function
    }
    
    
    # main plot
    if (plot_type == "boxplot") {
      
      gg <- ggplot(dat, aes(x = factor(x), y = y)) + 
        geom_boxplot(col           = gray(0.3),
                     outlier.color = gray(0.3),
                     outlier.alpha = 0.2) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
      
      if (is.null(ylab)) {
        ylab <- y_var
      }
      
    } else { # plot_type %in% c("line","line-points")
      
      gg <- ggplot(dat, aes(x = x, y = y), col = gray(0.3)) +
        geom_line()
      
      if (plot_type == "line-points") {
        gg <- gg + geom_point()
      }
      
      if (is.null(ylab)) {
        ylab <- paste0("median(",y_var,")")
      }
      
    }
    
    gg <- gg +
      scale_y_continuous(ylab, labels = label_function, limits = ylim)
  }
  
  # final theme adjustments
  gg <- gg +
    xlab(capitalize_firstLetter(apc_dimension))
  
  return(gg)
}
