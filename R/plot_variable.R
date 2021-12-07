
#' Distribution plot of one variable against one APC dimension
#' 
#' Plot the distribution of one variable in the data against age, period or
#' cohort. Creates a stacked bar plot for categorical variables and boxplots
#' for metric variables.
#' 
#' @param dat Dataset containing columns \code{age} and \code{period}.
#' @param variable Character name of the variable to plot.
#' @param apc_dimension One of \code{c("age","period","cohort")}. Defaults to
#' \code{"period"}.
#' @param log_scale Indicator if the visualized variable should be log10
#' transformed. Only used if the variable is numeric. Defaults to FALSE.
#' @param geomBar_position Value passed to \code{\link[ggplot2]{geom_bar}} as
#' \code{position} argument. Only used if the visualized variable is categorical.
#' Defaults to \code{"fill"}.
#' 
#' @import checkmate dplyr ggplot2
#' @export
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' data(travel)
#' 
#' # plot a metric variable
#' plot_variable(dat = travel, variable = "mainTrip_distance",
#'               apc_dimension = "period", log_scale = TRUE)
#' 
#' # plot a categorical variable
#' plot_variable(dat = travel, variable = "household_size", apc_dimension = "period")
#' plot_variable(dat = travel, variable = "household_size", apc_dimension = "period",
#'               geomBar_position = "stack")
#' 
plot_variable <- function(dat, variable, apc_dimension = "period",
                          log_scale = FALSE, geomBar_position = "fill") {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_choice("age", colnames(dat))
  checkmate::assert_choice("period", colnames(dat))
  checkmate::assert_choice(variable, choices = colnames(dat))
  checkmate::assert_choice(apc_dimension, choices = c("age","period","cohort"))
  checkmate::assert_logical(log_scale, len = 1)
  checkmate::assert_character(geomBar_position, len = 1)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  period <- age <- x <- y <- NULL
  
  
  dat <- dat %>% mutate(cohort = period - age)
  
  var_class <- ifelse(class(dat[[variable]]) %in% c("character","factor"),
                      "categorical", "metric")
  
  # recode the APC variable to factor
  dat[,apc_dimension] <- factor(dat[[apc_dimension]])
  
  # rename the variables for easier handling
  dat <- dat %>% dplyr::rename(x = apc_dimension,
                               y = variable)
  
  # create plot
  if (var_class == "categorical") {
    
    gg <- ggplot(dat, aes(x = x, fill = y)) + 
      geom_bar(position = geomBar_position) +
      scale_fill_brewer(variable, palette = "Set2")
    
  } else { # var_class == "metric"
    
    # log10 transform the main variable, and create a function to accordingly
    # adjust the labels on the y axis (the function is passed to scale_y_continuous())
    if (log_scale) {
      dat            <- dat %>% mutate(y = log10(y))
      label_function <- function(x) { paste0("10^",x) }
      
    } else { # no log transformation
      label_function <- function(x) { x } # identity function
    }
    
    gg <- ggplot(dat, aes(x = x, y = y)) + 
      geom_boxplot(col           = gray(0.3),
                   outlier.color = gray(0.3),
                   outlier.alpha = 0.2) +
      scale_y_continuous(variable, labels = label_function)
  }
  
  # final theme adjustments
  gg <- gg +
    xlab(capitalize_firstLetter(apc_dimension)) +
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
  
  return(gg)
}
