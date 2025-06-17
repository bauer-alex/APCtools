
#' Create a matrix of density plots
#' 
#' This function creates a matrix of individual density plots
#' (i.e., a \emph{ridgeline matrix}) or boxplots (for
#' metric variables) or of individual barplots (for categorical variables).
#' The age, period or cohort information can each either be plotted on the
#' x-axis or the y-axis.
#' 
#' @inheritParams plot_density
#' @param dimensions Character vector specifying the two APC dimensions that
#' should be visualized along the x-axis and y-axis. Defaults to
#' \code{c("period","age")}.
#' @param age_groups,period_groups,cohort_groups Each a list. Either containing
#' purely scalar values or with each element specifying the two borders of one
#' row or column in the density matrix. E.g., if the period should be visualized
#' in decade columns from 1980 to 2009, specify
#' \code{period_groups = list(c(1980,1989), c(1990,1999), c(2000,2009))}.
#' The list can be named to specify labels for the categories. Only the two
#' arguments must be passed that were specified by the \code{dimensions}
#' argument.
#' @param highlight_diagonals Optional list to define diagonals in the density
#' that should be highlighted with different colors. Each list element should be
#' a numeric vector stating the index of the diagonals (counted from the top
#' left) that should be highlighted in the same color. If the list is named, the
#' names are used as legend labels.
#' @param legend_title Optional plot annotation.
#' @param ... Additional arguments passed to \code{\link{plot_density}}.
#' 
#' @return ggplot object
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom stats as.formula
#' @export
#' 
#' @references Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
#' Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
#' destination choice patterns: Using generalized additive models to quantify
#' the impact of age, period, and cohort on travel distances.
#' \emph{Tourism Economics}. doi:10.1177/1354816620987198.
#' 
#' @author Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de},
#' Maximilian Weigert \email{maximilian.weigert@@stat.uni-muenchen.de}
#' 
#' @examples
#' library(APCtools)
#' 
#' # define categorizations for the main trip distance
#' dist_cat_breaks <- c(1,500,1000,2000,6000,100000)
#' dist_cat_labels <- c("< 500 km","500 - 1,000 km", "1,000 - 2,000 km",
#'                      "2,000 - 6,000 km", "> 6,000 km")
#' 
#' age_groups    <- list(c(80,89),c(70,79),c(60,69),c(50,59),c(40,49),c(30,39),c(20,29))
#' period_groups <- list(c(1970,1979),c(1980,1989),c(1990,1999),c(2000,2009),c(2010,2019))
#' cohort_groups <- list(c(1980,1989),c(1970,1979),c(1960,1969),c(1950,1959),c(1940,1949),
#'                       c(1930,1939),c(1920,1929))
#' 
#' plot_densityMatrix(dat              = travel,
#'                    y_var            = "mainTrip_distance",
#'                    age_groups       = age_groups,
#'                    period_groups    = period_groups,
#'                    log_scale        = TRUE)
#' 
#' \donttest{
#' # highlight two cohorts
#' plot_densityMatrix(dat                 = travel,
#'                    y_var               = "mainTrip_distance",
#'                    age_groups          = age_groups,
#'                    period_groups       = period_groups,
#'                    highlight_diagonals = list(8, 10),
#'                    log_scale           = TRUE)
#' 
#' # also mark different distance categories
#' plot_densityMatrix(dat              = travel,
#'                    y_var            = "mainTrip_distance",
#'                    age_groups       = age_groups,
#'                    period_groups    = period_groups,
#'                    log_scale        = TRUE,
#'                    y_var_cat_breaks = dist_cat_breaks,
#'                    y_var_cat_labels = dist_cat_labels,
#'                    highlight_diagonals = list(8, 10),
#'                    legend_title     = "Distance category")
#' 
#' # flexibly assign the APC dimensions to the x-axis and y-axis
#' plot_densityMatrix(dat              = travel,
#'                    y_var            = "mainTrip_distance",
#'                    dimensions       = c("period","cohort"),
#'                    period_groups    = period_groups,
#'                    cohort_groups    = cohort_groups,
#'                    log_scale        = TRUE,
#'                    y_var_cat_breaks = dist_cat_breaks,
#'                    y_var_cat_labels = dist_cat_labels,
#'                    legend_title     = "Distance category")
#' 
#' # use boxplots instead of densities
#' plot_densityMatrix(dat           = travel,
#'                    y_var         = "mainTrip_distance",
#'                    plot_type     = "boxplot",
#'                    age_groups    = age_groups,
#'                    period_groups = period_groups,
#'                    log_scale     = TRUE,
#'                    highlight_diagonals = list(8, 10))
#' 
#' # plot categorical variables instead of metric ones
#' plot_densityMatrix(dat                 = travel,
#'                    y_var               = "household_size",
#'                    age_groups          = age_groups,
#'                    period_groups       = period_groups,
#'                    highlight_diagonals = list(8, 10))
#' }
#' 
plot_densityMatrix <- function(dat, y_var, dimensions = c("period","age"),
                               age_groups = NULL, period_groups = NULL,
                               cohort_groups = NULL, plot_type = "density",
                               highlight_diagonals = NULL,
                               y_var_cat_breaks = NULL, y_var_cat_labels = NULL,
                               weights_var = NULL, log_scale = FALSE,
                               legend_title = NULL, ...) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_character(y_var, len = 1)
  checkmate::assert_character(dimensions, len = 2)
  checkmate::assert_subset(dimensions, choices = c("age","period","cohort"))
  if ("age" %in% dimensions) {
    checkmate::assert_list(age_groups, null.ok = FALSE)
  } else { checkmate::assert_null(age_groups) }
  if ("period" %in% dimensions) {
    checkmate::assert_list(period_groups, null.ok = FALSE)
  } else { checkmate::assert_null(period_groups) }
  if ("cohort" %in% dimensions) {
    checkmate::assert_list(cohort_groups, null.ok = FALSE)
  } else { checkmate::assert_null(cohort_groups) }
  checkmate::assert_choice(plot_type, choices = c("density","boxplot"))
  checkmate::assert_list(highlight_diagonals, types = "numeric", null.ok = TRUE)
  checkmate::assert_numeric(y_var_cat_breaks, null.ok = TRUE)
  checkmate::assert_character(y_var_cat_labels, len = length(y_var_cat_breaks) - 1,
                              null.ok = TRUE)
  checkmate::assert_character(weights_var, max.len = 1, null.ok = TRUE)
  checkmate::assert_logical(log_scale)
  checkmate::assert_character(legend_title, len = 1, null.ok = TRUE)
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  age_group <- period_group <- cohort_group <- NULL
  
  dat$cohort <- dat$period - dat$age
  
  
  dimX_groups <- get(paste0(dimensions[1], "_groups"))
  dimY_groups <- get(paste0(dimensions[2], "_groups"))
  
  # if intervals were specified for both main dimensions: check if same size
  if (length(dimX_groups[[1]]) == 2 | length(dimY_groups[[1]]) == 2) {
    
    interval_sizes <- c()
    if (length(dimX_groups[[1]]) == 2) {
      interval_sizes <- sapply(dimX_groups, function(x) x[2] - x[1])
    }
    if (length(dimY_groups[[1]]) == 2) {
      interval_sizes <- append(interval_sizes, 
                               sapply(dimY_groups, function(x) x[2] - x[1]))
    }
    
    if (length(unique(interval_sizes)) > 1) {
      stop("All intervals specified in '",dimensions[1],"_groups' and '",
           dimensions[2],"_groups' must have the sime size.")
    }
  }
  
  # if diagonals should be highlighted: calculate the diagonal labels
  if (!is.null(highlight_diagonals)) {
    operation_sign <- ifelse("period" %in% dimensions, -1, +1)
    
    for (i in 1:length(highlight_diagonals)) {
      d <- highlight_diagonals[[i]]
      dimY_refGroup <- min(c(length(dimY_groups), d))
      dimX_refGroup <- ifelse(d <= length(dimY_groups), 1,
                              d - length(dimY_groups) + 1)
      
      # case 1 for label calculation: X and Y groups are both scalar or interval
      if (length(dimX_groups[[1]]) == length(dimY_groups[[1]])) {
        # case 1: two intervals or two scalars
        d_label <- abs(dimX_groups[[dimX_refGroup]][1] +
                         operation_sign * dimY_groups[[dimY_refGroup]][1])
      } else { # case 2: one of X / Y is scalar, the other interval
        d_label <- abs(dimX_groups[[dimX_refGroup]] +
                         operation_sign * dimY_groups[[dimY_refGroup]])
      }
      names(highlight_diagonals)[i] <- paste(sort(d_label), collapse = " - ")
    }
  }
  
  
  # define the APC groups
  if ("age" %in% dimensions) {
    dat <- dat %>% 
      mutate(age_group = create_groupVariable(dat, "age", groups_list = age_groups)) %>% 
      filter(!is.na(age_group))
  }
  if ("period" %in% dimensions) {
    dat <- dat %>% 
      mutate(period_group = create_groupVariable(dat, "period", groups_list = period_groups)) %>% 
      filter(!is.na(period_group))
  }
  if ("cohort" %in% dimensions) {
    dat <- dat %>% 
      mutate(cohort_group = create_groupVariable(dat, "cohort", groups_list = cohort_groups)) %>% 
      filter(!is.na(cohort_group))
  }
  
  
  # define axis labels and facets
  main_lab  <- ifelse(!log_scale, y_var, paste(y_var, "on log10 scale"))
  x_lab     <- capitalize_firstLetter(dimensions[1])
  y_lab     <- capitalize_firstLetter(dimensions[2])
  facet_formula   <- stats::as.formula(paste(paste0(dimensions[2],"_group"), "~",
                                             paste0(dimensions[1],"_group")))
  legend.position <- ifelse(is.numeric(dat[[y_var]]) & is.null(highlight_diagonals),
                            "none", "right")
  
  # create density matrix
  gg <- plot_density(dat                 = dat,
                     y_var               = y_var,
                     plot_type           = plot_type,
                     highlight_diagonals = highlight_diagonals,
                     y_var_cat_breaks    = y_var_cat_breaks,
                     y_var_cat_labels    = y_var_cat_labels,
                     weights_var         = weights_var,
                     log_scale           = log_scale,
                     legend_title        = legend_title,
                     ...) +
    facet_grid(rows = facet_formula, switch = "y") +
    labs(subtitle = x_lab, x = main_lab, y = y_lab) +
    theme(axis.text.y       = element_blank(),
          axis.ticks.y      = element_blank(),
          plot.subtitle     = element_text(hjust = 0.5),
          strip.text.y.left = element_text(angle = 0),
          legend.position   = legend.position,
          panel.grid.minor  = element_blank())
  
  return(gg)
}



#' Internal helper to create a group variable as base for a density matrix
#' 
#' Internal helper function to create a group variable based on the
#' categorization of either age, period or cohort. To be called from within
#' \code{\link{plot_densityMatrix}}.
#' 
#' @param dat Dataset with a column \code{"age"}, \code{"period"} or
#' \code{"cohort"}, dependent on the specified \code{APC_var}.
#' @param APC_var One of \code{c("age","period","cohort")}.
#' @param groups_list A list with each element specifying the borders of one
#' row or column in the density matrix. E.g., if the period should be visualized
#' in decade columns from 1980 to 2009, specify
#' \code{groups_list = list(c(1980,1989), c(1990,1999), c(2000,2009))}.
#' The list can be named to specify labels for the categories.
#' 
#' @return Vector for the grouping that can be added as additional column to
#' the data.
#' 
#' @import dplyr
#' 
create_groupVariable <- function(dat, APC_var, groups_list) {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  group_var <- NULL
  
  
  # rename the 'APC_var' column, for easier handling
  dat <- dat %>% dplyr::rename(var = APC_var)
  
  # create the group labels, if not specified
  if (is.null(names(groups_list))) {
    names(groups_list) <- sapply(groups_list, function(x) { paste(x, collapse = " - ") })
  }
  
  # add a variable 'group_var' to the data
  dat$group_var <- "not categorized"
  for (i in 1:length(groups_list)) {
    
    if (length(groups_list[[i]]) == 1) { # groups are only specific scalar values
      dat <- dat %>% 
        mutate(group_var = case_when(var == groups_list[[i]] ~ names(groups_list)[i],
                                     TRUE                    ~ group_var))
    } else { # groups are intervals
      dat <- dat %>% 
        mutate(group_var = case_when(var >= groups_list[[i]][1] &
                                       var <= groups_list[[i]][2] ~ names(groups_list)[i],
                                     TRUE                         ~ group_var))
    }
  }
  dat <- dat %>% 
    mutate(group_var = factor(group_var, levels = names(groups_list)))
  
  return(dat$group_var)
}
