
#' Distribution plot of one variable against one APC dimension
#' 
#' Plot the distribution of one variable in the data against age, period or
#' cohort. Creates a stacked bar plot for categorical variables and boxplots
#' for metric variables.
#' 
#' @param dat Dataset containing columns \code{age}, \code{period} and
#' \code{cohort}.
#' @param variable Character name of the variable to plot.
#' @param apc_dimension One of \code{c("age","period","cohort")}. Defaults to
#' \code{"period"}.
#' 
#' @import checkmate dplyr ggplot2
#' @export
#' 
plot_variable <- function(dat, variable, apc_dimension = "period") {
  
  checkmate::check_data_frame(dat)
  checkmate::check_choice(variable, choices = names(dat))
  checkmate::check_choice(apc_dimension, choices = c("age","period","cohort"))
  
  
  var_class <- ifelse(class(dat[[variable]]) %in% c("character","factor"),
                      "categorical", "metric")
  
  # recode the APC variable to factor
  dat[,apc_dimension] <- factor(dat[[apc_dimension]])
  
  # create plot
  if (var_class == "categorical") {
    
    gg <- ggplot(dat, aes_string(x = apc_dimension, fill = variable)) + 
      geom_bar(position = "fill")
    
  } else { # var_class == "metric"
    
    gg <- ggplot(dat, aes_string(x = apc_dimension, y = variable)) + 
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90))
  }
  
  return(gg)
}