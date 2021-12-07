
#' Hexamap of an APC surface
#' 
#' Plot the heatmap of an APC structure using a hexagon-based plot with adapted
#' axes. In this way, the one temporal dimension that is represented by the
#' diagonal structure is visually not underrepresented compared to the other two
#' dimensions on the x-axis and y-axis. \cr
#' The function can be used in two ways: Either to plot the observed mean
#' structure of a metric variable, by specifying \code{dat} and the variable
#' \code{y_var}, or by specifying \code{dat} and the \code{model} object, to
#' plot some mean structure represented by an estimated two-dimensional tensor
#' product surface. The model must be estimated with \code{\link[mgcv]{gam}}.
#' 
#' See also \code{\link{plot_APCheatmap}} to plot a regular heatmap.
#' 
#' If the plot is created based on the \code{model} object and the model was
#' estimated with a log or logit link, the function automatically performs an
#' exponential transformation of the effect.
#' 
#' @inheritParams plot_APCheatmap
#' @param obs_interval Numeric specifying the interval width based on which the
#' data is spaced. Only used if \code{y_var} is specified. Defaults to 1, i.e.
#' observations each year.
#' @param iso_interval Numeric specifying the interval width between the
#' isolines along each axis. Defaults to 5.
#' @param color_vec Optional character vector of color names, specifying the
#' color continuum.
#' @param color_range Optional numeric vector with two elements, specifying the
#' ends of the color scale in the legend.
#' @param line_width Line width of the isolines. Defaults to 0.5.
#' @param line_color Character color name for the isolines. Defaults to gray.
#' @param label_size Size of the labels along the axes. Defaults to 0.5.
#' @param label_color Character color name for the labels along the axes.
#' @param legend_title Optional character title for the legend.
#' 
#' @import checkmate dplyr
#' @importFrom tidyr pivot_wider
#' @export
#' 
#' @author Hawre Jalal \email{hjalal@@pitt.edu},
#' Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{plot_APCheatmap}}
#' 
#' @examples
#' library(APCtools)
#' library(mgcv)
#' library(dplyr)
#' 
#' data(drug_deaths)
#' 
#' # restrict to data where the mortality rate is available
#' drug_deaths <- drug_deaths %>% filter(!is.na(mortality_rate))
#' 
#' # hexamap of an observed structure
#' plot_APChexamap(dat         = drug_deaths,
#'                 y_var       = "mortality_rate",
#'                 color_range = c(0,40))
#' 
#' # hexamap of a smoothed structure
#' model <- gam(mortality_rate ~ te(age, period, bs = "ps", k = c(8,8)),
#'              data = drug_deaths)
#' 
#' plot_APChexamap(dat = drug_deaths, model = model)
#' 
plot_APChexamap <- function (dat,
                             y_var          = NULL,
                             model          = NULL,
                             apc_range      = NULL,
                             y_var_logScale = FALSE,
                             obs_interval   = 1,
                             iso_interval   = 5,
                             color_vec      = NULL,
                             color_range    = NULL,
                             line_width     = .5,
                             line_color     = gray(0.5),
                             label_size     = .5,
                             label_color    = "black",
                             legend_title   = NULL) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_true(!is.null(y_var) | !is.null(model))
  checkmate::assert_character(y_var, len = 1, null.ok = TRUE)
  checkmate::assert_choice(y_var, choices = colnames(dat), null.ok = TRUE)
  checkmate::assert_class(model, classes = "gam", null.ok = TRUE)
  checkmate::assert_list(apc_range, types = "numeric", max.len = 3,
                         null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_subset(names(apc_range), choices = c("age","period","cohort"))
  checkmate::assert_logical(y_var_logScale, len = 1)
  checkmate::assert_numeric(obs_interval, lower = 0, len = 1)
  checkmate::assert_numeric(iso_interval, lower = 0, len = 1)
  checkmate::assert_character(color_vec, null.ok = TRUE)
  checkmate::assert_numeric(color_range, len = 2, null.ok = TRUE)
  checkmate::assert_numeric(line_width, lower = 0, len = 1)
  checkmate::assert_character(line_color, len = 1)
  checkmate::assert_numeric(label_size, lower = 0, len = 1)
  checkmate::assert_character(label_color, len = 1)
  checkmate::assert_character(legend_title, len = 1, null.ok = TRUE)
  
  
  if (!is.null(y_var)) { # plot observed structures
    
    plot_dat <- dat %>% 
      mutate(cohort = period - age) %>% 
      dplyr::rename(effect = y_var) %>% # rename 'y_var' for easier handling
      filter(!is.na(effect))
    
    if (y_var_logScale) {
      plot_dat <- plot_dat %>% mutate(effect = log10(effect))
      if (!is.null(color_range)) {
        color_range <- log10(color_range)
      }
    }
    
    used_logLink <- FALSE
    
    if (is.null(legend_title)) {
      legend_title <- y_var
    }
    
    
  } else { # plot smoothed, model-based structures
    
    # create a dataset for predicting the values of the APC surface
    grid_age    <- min(dat$age, na.rm = TRUE):max(dat$age, na.rm = TRUE)
    grid_period <- min(dat$period, na.rm = TRUE):max(dat$period, na.rm = TRUE)
    dat_predictionGrid <- expand.grid(age    = grid_age,
                                      period = grid_period) %>% 
      mutate(cohort = period - age)
    # add random values for all further covariates in the model,
    # necessary for calling mgcv:::predict.gam
    covars <- attr(model$terms, "term.labels")
    covars <- covars[!(covars %in% c("age","period","cohort"))]
    if (length(covars) > 0) {
      dat_predictionGrid[,covars] <- dat[1, covars]
    }
    
    # create a dataset containing the estimated values of the APC surface
    terms_model     <- sapply(model$smooth, function(x) { x$label })
    terms_index_APC <- which(grepl("age", terms_model) | grepl("period", terms_model))
    term_APCsurface <- terms_model[terms_index_APC]
    
    prediction <- dat_predictionGrid %>% 
      predict(object  = model,
              newdata = .,
              type    = "terms",
              terms   = term_APCsurface,
              se.fit  = TRUE)
    
    plot_dat <- dat_predictionGrid %>%
      mutate(effect = as.vector(prediction$fit)) %>% 
      mutate(effect = effect - mean(effect)) 
    
    used_logLink <- model$family[[2]] %in% c("log","logit")
    if (is.null(legend_title)) {
      legend_title <- ifelse(used_logLink, "Mean exp effect", "Mean effect")
    }
    
    if (used_logLink) {
      plot_dat <- plot_dat %>% 
        mutate(effect = exp(effect))
    }
    
  }
  
  
  # filter the dataset
  if (!is.null(apc_range)) {
    if (!is.null(apc_range$age)) {
      plot_dat <- plot_dat %>% filter(age %in% apc_range$age)
    }
    if (!is.null(apc_range$period)) {
      plot_dat <- plot_dat %>% filter(period %in% apc_range$period)
    }
    if (!is.null(apc_range$cohort)) {
      plot_dat <- plot_dat %>% filter(cohort %in% apc_range$cohort)
    }
  }
  
  # reformat the data to wide format
  mat <- plot_dat %>% select(period, age, effect) %>% 
    tidyr::pivot_wider(id_cols = age, names_from = period, values_from = effect) %>% 
    select(-1) %>% 
    as.matrix()
  
  # setting default values for missing parameters
  if (is.null(color_range)) {
    color_range <- range(mat, na.rm = TRUE)
  }
  if (is.null(color_vec)) {
    # define jet colormap
    jet.colors <- colorRampPalette(c("dodgerblue4", "white", "firebrick3"))
    color_vec  <- jet.colors(100)
  }
  # end of default values
  
  m <- dim(mat)[1]
  n <- dim(mat)[2]
  
  first_age    <- min(plot_dat$age)
  last_age     <- first_age + (m - 1) * obs_interval
  first_period <- min(plot_dat$period)
  last_period  <- first_period + (n - 1) * obs_interval
  first_cohort <- first_period - last_age
  last_cohort  <- last_period - first_age
  
  age_isolines     <- seq(from = first_age,    to = last_age,    by = iso_interval)
  period_isolines  <- seq(from = first_period, to = last_period, by = iso_interval)
  cohort_isolines  <- seq(from = first_cohort, to = last_cohort, by = iso_interval)
  
  ages      <- seq(from = first_age,    to = last_age,    by = obs_interval)
  periods   <- seq(from = first_period, to = last_period, by = obs_interval)
  cohorts   <- seq(from = first_cohort, to = last_cohort, by = obs_interval)
  n_ages    <- length(ages)
  n_periods <- length(periods)
  n_cohorts <- length(cohorts)
  
  n_age_isolines    <- length(age_isolines)
  n_period_isolines <- length(period_isolines)
  n_cohort_isolines <- length(cohort_isolines)
  
  # apply the limits to the data by truncating it
  mat[mat < color_range[1]] <- color_range[1]
  mat[mat > color_range[2]] <- color_range[2] 
  
  
  ### plotting
  ncol        <- length(color_vec)
  not_nan_mat <- !is.nan(mat)
  
  v_mat <- as.vector(mat[not_nan_mat])
  matc  <- cut(mat[not_nan_mat], # discretize the data 
               seq(from = color_range[1], to = color_range[2], length.out = ncol), 
               include.lowest = T, 
               labels = F)
  
  a  <- obs_interval / sqrt(3) # radius of the hexagon (distance from center to a vertex).
  b  <- sqrt(3)/2 * a # half height of the hexagon (distance from the center perpendicular to the middle of the top edge)
  yv <- c(0, b, b, 0, -b, -b, 0)
  xv <- c(-a, -a/2, a/2, a, a/2, -a/2, -a)
  
  # compute the center of each hexagon by creating an a*p grid for each age-period combination
  P0 <- matrix(periods, nrow = n_ages, ncol=n_periods, byrow = TRUE)
  A0 <- t(matrix(ages, nrow = n_periods, ncol = n_ages, byrow = TRUE))
  
  # convert the grid to the X-Y Coordinate
  X <- compute_xCoordinate(P0)
  Y <- compute_yCoordinate(P0, A0)
  
  # only keep those that have non-NA values
  X <- X[not_nan_mat]
  Y <- Y[not_nan_mat]
  
  # get the color for each level
  color_vec2 <- color_vec[matc]
  
  Xvec <- as.vector(X) 
  Yvec <- as.vector(Y)
  n_hexagons <- length(Xvec)
  
  # compute the X and Y cooridinate for each hexagon - each hexagon is a row and each point is a column
  Xhex <- outer(Xvec, xv, '+') 
  Yhex <- outer(Yvec, yv, '+')
  
  minX <- min(Xhex) - obs_interval
  maxX <- max(Xhex) + obs_interval
  minY <- min(Yhex) - obs_interval
  maxY <- max(Yhex) + obs_interval
  
  layout(t(1:2), widths=c(4,1)) # two columns - one for the plot, the other for the colorbar
  
  par(mar=c(.5,.5,.5,.5))
  
  plot(x = NULL, y = NULL,
       xlim = c(minX,maxX), ylim = c(minY,maxY),
       axes=FALSE, frame.plot=FALSE,
       xaxt = 'n', yaxt = 'n', type = 'n', asp = 1)
  
  for (i in 1:n_hexagons) {
    polygon(x = Xhex[i,],   # X-Coordinates of polygon
            y = Yhex[i,],   # Y-Coordinates of polygon
            col = color_vec2[i],  # Color of polygon
            border = NA, # Color of polygon border
            lwd = 1)                                            
  }
  
  # age-isolines
  y1 <- compute_yCoordinate(first_period, age_isolines)
  y2 <- compute_yCoordinate(last_period + obs_interval, age_isolines)
  x1 <- compute_xCoordinate(first_period)
  x2 <- compute_xCoordinate(last_period + obs_interval)
  
  for (i in 1:n_age_isolines) {
    lines(x=c(x1,x2), y=c(y1[i],y2[i]), col = line_color, lwd = line_width)
    text(x=x2, y=y2[i], labels = paste("A:",age_isolines[i]), 
         col = label_color, cex = label_size, srt = -30, 
         adj = c(0, 0.5))
  }
  
  # period-isolines
  x  <- compute_xCoordinate(period_isolines)
  y1 <- compute_yCoordinate(period_isolines, first_age)
  y2 <- compute_yCoordinate(period_isolines, last_age + obs_interval)
  
  for (i in 1:n_period_isolines) {
    lines(x=c(x[i], x[i]), y=c(y1[i],y2[i]), col = line_color, lwd = line_width)
    text(x=x[i], y=y2[i], labels = paste("P:",period_isolines[i]), 
         col = label_color, cex = label_size, srt = 90, 
         adj = c(0, .5))
  }
  
  # cohort-isolines (need some more processing!)
  # determine the periods where the cohort isolines cross the last age
  p_top <- cohort_isolines + last_age
  p_top <- p_top[p_top < last_period] 
  n_top <- length(p_top)
  # and the periods where they cross the first age
  p_bottom <- cohort_isolines + first_age
  p_bottom <- p_bottom[p_bottom > first_period]
  n_bottom <- length(p_bottom)
  # and the ages where they cross the first period
  a_left <- first_period - cohort_isolines
  a_left <- a_left[a_left >= first_age]
  n_left <- length(a_left)
  # and the ages where they cross the last period
  a_right <- last_period - cohort_isolines
  a_right <- a_right[a_right <= last_age]
  n_right <- length(a_right)
  
  # combine the periods and ages initial and final points on the a*p coordinates
  # first the left-bottom edge
  p1 <- c(rep(first_period, n_left), p_bottom)
  a1 <- c(a_left, rep(first_age, n_bottom))
  # then the top-right edge
  p2 <- c(p_top, rep(last_period, n_right))
  a2 <- c(rep(last_age, n_top), a_right)
  
  # convert the a*p Coordinates to x-y coordinates
  x1 <- compute_xCoordinate(p1 - obs_interval)
  x2 <- compute_xCoordinate(p2)
  y1 <- compute_yCoordinate(p1 - obs_interval, a1 - obs_interval)
  y2 <- compute_yCoordinate(p2, a2)
  # finally draw the lines.
  for (i in 1:n_cohort_isolines) { 
    lines(x = c(x1[i], x2[i]), y = c(y1[i],y2[i]), col = line_color, lwd = line_width)
    text(x = x1[i], y = y1[i], labels = paste("C:",cohort_isolines[i]), 
         col = label_color, cex = label_size, srt = 30, 
         adj = c(1,.5))
  }
  # create the colorbar
  par(las = 2)
  par(mar = c(10,2,10,2.5))
  cb_range <- seq(from = color_range[1], to = color_range[2], length.out = ncol)
  image(y = cb_range, z = t(cb_range), col = color_vec, axes = FALSE,
        main = legend_title, cex.main = .8)
  axis(4, cex.axis = label_size, mgp = c(0,.5,0))
}
