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
#' product surface. The model must be estimated with \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}.
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
#' @param legend_limits Optional numeric vector passed as argument \code{limits}
#' to \code{\link[ggplot2]{scale_fill_gradient2}}.
#' 
#' @return Plot created with \code{ggplot2}.
#' 
#' @import checkmate dplyr graphics ggplot2 reshape2
#' @importFrom mgcv predict.gam
#' @importFrom tidyr pivot_wider
#' @export
#' 
#' @references Jalal, H., Burke, D. (2020). Hexamaps for Age–Period–Cohort
#' Data Visualization and Implementation in R.
#' \emph{Epidemiology}, 31 (6), e47-e49. doi: 10.1097/EDE.0000000000001236.
#' 
#' @author Hawre Jalal \email{hjalal@@pitt.edu},
#' Alexander Bauer \email{alexander.bauer@@stat.uni-muenchen.de}
#' Rickmer Schulte \email{rickmer.schulte@@stablab.stat.uni-muenchen.de}
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
#' ggplot_APChexamap(dat         = drug_deaths,
#'                 y_var       = "mortality_rate",
#'                 color_range = c(0,40))
#' 
#' # hexamap of a smoothed structure
#' model <- gam(mortality_rate ~ te(age, period, bs = "ps", k = c(8,8)),
#'              data = drug_deaths)
#' 
#' ggplot_APChexamap(dat = drug_deaths, model = model)
#' 
ggplot_APChexamap <- function (dat,
                             y_var          = NULL,
                             model          = NULL,
                             apc_range      = NULL,
                             y_var_logScale = FALSE,
                             obs_interval   = 1,
                             iso_interval   = 5,
                             color_vec      = NULL,
                             color_range    = NULL,
                             line_width     = .05,
                             line_color     = gray(0.5),
                             label_size     = 1.5,
                             label_color    = "black",
                             legend_title   = NULL,
                             legend_limits = NULL) {
  
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
  checkmate::assert_numeric(legend_limits, len = 2, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  period <- age <- effect <- cohort <- NULL
  
  
  if (!is.null(y_var)) { # plot observed structures
    
    plot_dat <- dat %>% 
      mutate(cohort = period - age) %>% 
      dplyr::rename(effect = y_var) %>% # rename 'y_var' for easier handling
      filter(!is.na(effect))
    
    # if a period-age combination is appearing multiple times, take the average
    if (max(table(paste(plot_dat$period, plot_dat$age))) > 1) {
      plot_dat <- plot_dat %>% 
        group_by(period, age) %>% 
        summarize(effect = mean(effect)) %>% 
        ungroup()
    }
    
    if (y_var_logScale) {
      plot_dat <- plot_dat %>% mutate(effect = log10(effect))
      if (!is.null(color_range)) {
        color_range <- log10(color_range)
      }
    }
    
    used_logLink <- FALSE
    
    if (is.null(legend_title)) {
      legend_title <- y_var
      if (y_var_logScale) {
        legend_title <- paste0("log10(", y_var, ")")
      }
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
    
    prediction <- mgcv::predict.gam(object  = model,
                                    newdata = dat_predictionGrid,
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
    arrange(age) %>% 
    select(-1) %>% 
    as.matrix()
  row.names(mat) <- sort(unique(plot_dat$age))
  
  # setting default values for missing parameters
  if (is.null(color_range)) {
    color_range <- range(mat, na.rm = TRUE)
    # use a symmetric color scale if the value range spans zero
    if (color_range[1] < 0 & color_range[2] > 0) {
      color_range <- c(-1,1) * max(abs(range(mat, na.rm = TRUE)))
    }
  }
  # end of default values
  
  nA <- 1 + diff(range(as.numeric(row.names(mat))))
  nP <- 1 + diff(range(as.numeric(colnames(mat))))
  
  first_age    <- min(plot_dat$age)
  last_age     <- first_age + (nA - 1) * obs_interval
  first_period <- min(plot_dat$period)
  last_period  <- first_period + (nP - 1) * obs_interval
  first_cohort <- first_period - last_age
  last_cohort  <- last_period - first_age
  
  age_isolines     <- seq(from = first_age,    to = last_age,    by = iso_interval)
  period_isolines  <- seq(from = first_period, to = last_period, by = iso_interval)
  cohort_isolines  <- seq(from = first_cohort, to = last_cohort, by = iso_interval)
  
  ages      <- seq(from = first_age,    to = last_age,    by = obs_interval)
  periods   <- seq(from = first_period, to = last_period, by = obs_interval)
  cohorts   <- seq(from = first_cohort, to = last_cohort, by = obs_interval)
  ages      <- ages[ages %in% row.names(mat)]
  periods   <- periods[periods %in% colnames(mat)]
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
  not_nan_mat <- !is.na(mat) & !is.nan(mat)
  
  v_mat <- as.vector(mat[not_nan_mat])
  
  scale_midpoint <- mean(v_mat)
  y_trans      <- ifelse(used_logLink, "log", "identity")
  
  a  <- obs_interval / sqrt(3) # radius of the hexagon (distance from center to a vertex).
  b  <- sqrt(3)/2 * a # half height of the hexagon (distance from the center perpendicular to the middle of the top edge)
  yv <- c(0, b, b, 0, -b, -b, 0)
  xv <- c(-a, -a/2, a/2, a, a/2, -a/2, -a)
  
  # compute the center of each hexagon by creating an a*p grid for each age-period combination
  P0 <- matrix(periods, nrow = n_ages,    ncol = n_periods, byrow = TRUE)
  A0 <- t(matrix(ages,  nrow = n_periods, ncol = n_ages,    byrow = TRUE))
  
  # convert the grid to the X-Y Coordinate
  X <- compute_xCoordinate(P0)
  Y <- compute_yCoordinate(P0, A0)
  
  minX <- min(X) - 2*obs_interval
  maxX <- max(X) + 2*obs_interval
  minY <- min(Y) - 2*obs_interval
  maxY <- max(Y) + 2*obs_interval
  
  # only keep those that have non-NA values
  X <- X[not_nan_mat]
  Y <- Y[not_nan_mat]
  
  Xvec <- as.vector(X) 
  Yvec <- as.vector(Y)
  n_hexagons <- length(Xvec)
  
  # compute the X and Y cooridinate for each hexagon - each hexagon is a row and
  # each polygon point is a column
  Xhex <- outer(Xvec, xv, '+') 
  Yhex <- outer(Yvec, yv, '+')

  ## create the base hexamap plot
  df_X = data.frame(id=seq(1:nrow(Xhex)),Xhex)
  df_Y = data.frame(id=seq(1:nrow(Yhex)),Yhex)
  df_positions = merge( melt(df_X, id.vars=1), melt(df_Y, id.vars=1), by = c("id", "variable"))
  df_values <- data.frame(id = seq(1:nrow(Xhex)), color = v_mat)
  plot_dat <- merge(df_positions, df_values, by = c("id"))

  gg <- ggplot(plot_dat, aes(x=value.x, y=value.y)) + geom_polygon(aes(fill = color, group = id)) + # fill = as.factor(color)
    theme_bw() +
    scale_fill_gradient2(legend_title, limits = legend_limits,
                       trans = y_trans, low = "dodgerblue4",
                       mid = "white", high = "firebrick3",
                       midpoint = scale_midpoint) +
    coord_equal() +
    theme(axis.line = element_line(colour = "white"),
          axis.title = element_text(colour = "white"),
          axis.text = element_text(colour = "white"),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
          )
  
  # age-isolines
  y1 <- compute_yCoordinate(first_period, age_isolines)
  y2 <- compute_yCoordinate(last_period + obs_interval, age_isolines)
  x1 <- compute_xCoordinate(first_period)
  x2 <- compute_xCoordinate(last_period + obs_interval)
  
  df_age_isolines <- data.frame(x1 = rep(x1, length(y1)),
                                x2 = rep(x2, length(y2)),
                                y1 = y1,
                                y2 = y2,
                                age_isolines_labels = paste("A:", age_isolines),
                                label_color = rep(label_color, length(y1)),
                                label_size = rep(label_size, length(y1)),
                                srt_value = rep(-30, length(y1)))
  gg <- gg + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), col = line_color, lwd = line_width, data = df_age_isolines)
  gg <- gg + geom_text(aes(x = x2, y = y2, label = age_isolines_labels, srt = srt_value), col = label_color, cex = label_size, data = df_age_isolines)
  
  # period-isolines
  
  x  <- compute_xCoordinate(period_isolines)
  y1 <- compute_yCoordinate(period_isolines, first_age)
  y2 <- compute_yCoordinate(period_isolines, last_age + obs_interval)
  df_period_isolines <- data.frame(x1 = rep(x, length(y1)),
                                x2 = rep(x, length(y2)),
                                y1 = y1,
                                y2 = y2,
                                period_isolines_labels = paste("P:", period_isolines),
                                label_color = rep(label_color, length(y1)),
                                label_size = rep(label_size, length(y1)),
                                srt_value = rep(90, length(y1)))
  gg <- gg + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), col = line_color, lwd = line_width, data = df_period_isolines)
  gg <- gg + geom_text(aes(x = x2, y = y2, label = period_isolines_labels, srt = srt_value), col = label_color, cex = label_size, data = df_period_isolines)
  
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
  
  df_cohort_isolines <- data.frame(x1 = rep(x1, length(y1)),
                                   x2 = rep(x2, length(y2)),
                                   y1 = y1,
                                   y2 = y2,
                                   cohort_isolines_labels = paste("C:", cohort_isolines),
                                   label_color = rep(label_color, length(y1)),
                                   label_size = rep(label_size, length(y1)),
                                   srt_value = rep(30, length(y1)),
                                   adj_1 = rep(1, length(y1)),
                                   adj_2 = rep(0.5, length(y1))
                                   )
  gg <- gg + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), col = line_color, lwd = line_width, data = df_cohort_isolines)
  gg <- gg + geom_text(aes(x = x1, y = y1, label = cohort_isolines_labels, srt = srt_value), col = label_color, cex = label_size, data = df_cohort_isolines)
  
  return(gg)
}
  