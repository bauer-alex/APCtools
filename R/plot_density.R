
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
#' Only used if the \code{y_var} column is numeric.
#' @param xlab,ylab,legend_title Optional plot annotations.
#' @param ... Additional arguments passed to \code{\link[stats]{density}}.
#' 
#' @import checkmate dplyr
#' @export
#' 
plot_density <- function(dat, y_var, plot_type = "density", apc_range = NULL,
                         highlight_diagonals = NULL,
                         y_var_cat_breaks = NULL, y_var_cat_labels = NULL,
                         weights_var = NULL, log_scale = FALSE, xlab = NULL,
                         ylab = "Density", legend_title = NULL, ...) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_character(y_var, len = 1)
  checkmate::assert_choice(plot_type, choices = c("density","boxplot"))
  checkmate::assert_list(apc_range, types = "character", max.len = 3,
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

  
  # main plot
  if (is.numeric(dat[[y_var]])) { # metric variable
    gg <- plot_density_metric(dat                 = dat,
                              y_var               = y_var,
                              plot_type           = plot_type,
                              highlight_diagonals = highlight_diagonals,
                              y_var_cat_breaks    = y_var_cat_breaks,
                              y_var_cat_labels    = y_var_cat_labels,
                              weights_var         = weights_var,
                              log_scale           = log_scale,
                              xlab                = xlab,
                              ylab                = ylab,
                              legend_title        = legend_title,
                              ...)
    
  } else { # categorical variable
    gg <- plot_density_categorical(dat                 = dat,
                                   y_var               = y_var,
                                   highlight_diagonals = highlight_diagonals,
                                   weights_var         = weights_var,
                                   xlab                = xlab,
                                   ylab                = ylab,
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
                                highlight_diagonals = NULL,
                                y_var_cat_breaks = NULL, y_var_cat_labels = NULL,
                                weights_var = NULL, log_scale = FALSE, xlab = NULL,
                                ylab = "Density", legend_title = NULL, ...) {
  
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
  
  # final plot type-specific preparations
  if (plot_type == "density") {
    
    dat_dens <- calc_density(dat                 = dat,
                             y_var               = y_var,
                             highlight_diagonals = highlight_diagonals,
                             weights_var         = weights_var,
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
    gg <- ggplot(data = dat_dens, aes(x = x, y = y)) +
      geom_line(col = gray(0.3))
    
    if (!is.null(y_var_cat_breaks)) {
      gg <- gg + geom_ribbon(aes(ymin = 0, ymax = y, fill = x_cat))
    } else {
      gg <- gg + geom_ribbon(aes(ymin = 0, ymax = y, fill = col_group))
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
plot_density_categorical <- function(dat, y_var, highlight_diagonals = NULL,
                                     weights_var = NULL, xlab = NULL,
                                     ylab = "Density") {
  
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
  
  # main plot
  gg <- ggplot(data = dat, aes(x = x, y = ..count../sum(..count..), weight = weight)) +
    geom_bar(fill = gray(0.3)) +
    xlab(xlab) + ylab(ylab)
  
  return(gg)
}



#' Internal helper to calculate the (group-specific) density of a variable
#' 
#' Internal helper function that is called in \code{\link{plot_density}} to
#' calculate the density of a metric variable. If \code{plot_density} is called
#' from within \code{\link{plot_densityMatrix}} (i.e., when some of the columns
#' \code{c("age_group","period_group","cohort_group")} are part of the dataset,
#' the density is computed individually for all respective APC groups.
#' 
#' @inheritParams plot_density
#' 
#' @return Dataset with the calculated densities.
#' 
#' @import dplyr
#' @importFrom stats density
#' 
calc_density <- function(dat, y_var, highlight_diagonals = NULL,
                         weights_var = NULL, ...) {

  # retrieve the weights vector
  weights_vector <- NULL
  if (!is.null(weights_var)) {
    # make sure the weights are not NA
    if (any(is.na(dat[[weights_var]]))) {
      warning("Deleting ",sum(is.na(dat[[weights_var]])), " observations where the weights variable is NA.")
      dat <- dat[!is.na(dat[[weights_var]]),]
    }
    
    weights_vector <- dat[[weights_var]]
  }

  
  # calculate the densities
  if (all(!(c("age_group","period_group","cohort_group") %in% names(dat)))) { # calculate one global density
    
    dens <- stats::density(x       = dat[[y_var]],
                           weights = weights_vector, ...)
    dat_dens <- data.frame(x = dens$x, y = dens$y)
    
  } else { # calculate one density for each APC subgroup
    
    dimensions <- c()
    if ("age_group" %in% names(dat)) {    dimensions <- append(dimensions, "age_group") }
    if ("period_group" %in% names(dat)) { dimensions <- append(dimensions, "period_group") }
    if ("cohort_group" %in% names(dat)) { dimensions <- append(dimensions, "cohort_group") }
    diag_dimension <- ifelse(!("age_group" %in% names(dat)), "age groups",
                             ifelse(!("period_group" %in% names(dat)), "periods",
                                    "cohorts"))
    
    # create a data.frame from 'highlight_diagonals' for easier handling
    if (!is.null(highlight_diagonals)) {
      diag_dat <- data.frame(diagonal = unlist(highlight_diagonals, use.names = FALSE))
      diag_dat$label <- sapply(1:length(highlight_diagonals), function(i) {
        label <- ifelse(!is.null(names(highlight_diagonals)),
                        names(highlight_diagonals)[i], paste("Diagonal",LETTERS[i]))
        return(rep(label, times = length(highlight_diagonals[[i]])))
      }) %>% unlist()
    }
    
    # 'dimensions' always has two elements only when called from within 'plot_densityMatrix'
    dim1_categories <- levels(dat[[dimensions[1]]])
    dim2_categories <- levels(dat[[dimensions[2]]])
    
    dat_list1 <- lapply(1:length(dim1_categories), function(i1) {
      
      dat_list12 <- lapply(1:length(dim2_categories), function(i2) {
        dim12_rows <- which(dat[[dimensions[1]]] == dim1_categories[i1] &
                              dat[[dimensions[2]]] == dim2_categories[i2])
        
        if (length(dim12_rows) < 2) { # return nothing if the combination is not part of the data
          return(NULL)
        }
        
        dens <- stats::density(x       = dat[dim12_rows, y_var, drop = TRUE],
                               weights = weights_vector[dim12_rows], ...)
        dat_dens12 <- data.frame(x         = dens$x,
                                 y         = dens$y,
                                 dim1      = dim1_categories[i1],
                                 dim2      = dim2_categories[i2],
                                 col_group = paste("other",diag_dimension)) # for 'highlight_diagonals'
        
        if (!is.null(highlight_diagonals)) {
          col_group_row <- match(i1 + i2, diag_dat$diagonal)
          if (!is.na(col_group_row)) {
            dat_dens12$col_group <- diag_dat$label[col_group_row]
          }
        }
        
        return(dat_dens12)
      })
      
      dat_dens1 <- dplyr::bind_rows(dat_list12)
      
      return(dat_dens1)
    })
    
    dat_dens <- dplyr::bind_rows(dat_list1)
    dat_dens$dim1 <- factor(dat_dens$dim1, levels = dim1_categories)
    dat_dens$dim2 <- factor(dat_dens$dim2, levels = dim2_categories)
    colnames(dat_dens)[colnames(dat_dens) == "dim1"] <- dimensions[1]
    colnames(dat_dens)[colnames(dat_dens) == "dim2"] <- dimensions[2]
  }
  
  return(dat_dens)
}
