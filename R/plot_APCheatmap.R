
#' Heatmap of an APC surface
#' 
#' Plot the heatmap of an APC structure. The function can be used in two ways:
#' Either to plot the observed mean structure of a metric variable, by
#' specifying \code{dat} and the variable \code{y_var}, or by specifying
#' \code{dat} and the \code{model} object, to plot some mean structure
#' represented by an estimated two-dimensional tensor product surface. The model
#' must be estimated with \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' 
#' See also \code{\link{plot_APChexamap}} to plot a hexagonal heatmap with
#' adapted axes.
#' 
#' If the plot is created based on the \code{model} object and the model was
#' estimated with a log or logit link, the function automatically performs an
#' exponential transformation of the effect.
#' 
#' @param dat Dataset with columns \code{period} and \code{age}. If \code{y_var}
#' is specified, the dataset must contain the respective column. If \code{model}
#' is specified, the dataset must have been used for model estimation with
#' \code{gam} or \code{bam}.
#' @param y_var Optional character name of a metric variable to be plotted.
#' @param model Optional regression model estimated with \code{\link[mgcv]{gam}}
#' or \code{\link[mgcv]{bam}} to estimate a smoothed APC surface. Only used if
#' \code{y_var} is not
#' specified.
#' @param dimensions Character vector specifying the two APC dimensions that
#' should be visualized along the x-axis and y-axis. Defaults to
#' \code{c("period","age")}.
#' @param apc_range Optional list with one or multiple elements with names
#' \code{"age","period","cohort"} to filter the data. Each element should
#' contain a numeric vector of values for the respective variable that should
#' be kept in the data. All other values are deleted.
#' @param bin_heatmap,bin_heatmapGrid_list \code{bin_heatmap} indicates if the
#' heatmap surface should be binned. Defaults to TRUE. If TRUE, the binning
#' grid borders are defined by \code{bin_heatmapGrid_list}. This is a list with
#' each element a numeric vector and a name out of
#' \code{c("age","period","cohort")}. Can maximally have three elements. Defaults
#' to NULL, where the heatmap is binned in 5 year steps along the x-axis and the
#' y-axis.
#' @param markLines_list Optional list that can be used to highlight the borders
#' of specific age groups, time intervals or cohorts. Each element must be a
#' numeric vector of values where horizontal, vertical or diagonal lines should
#' be drawn (depends on which APC dimension is displayed on which axis).
#' The list can maximally have three elements and must have names out of
#' \code{c("age","period","cohort")}.
#' @param markLines_displayLabels Optional character vector defining for which
#' dimensions the lines defined through \code{markLines_list} should be marked
#' by a respective label. The vector should be a subset of
#' \code{c("age","period","cohort")}, or NULL to suppress all labels.
#' Defaults to \code{c("age","period","cohort")}.
#' @param y_var_logScale Indicator if \code{y_var} should be log10 transformed.
#' Only used if \code{y_var} is specified. Defaults to FALSE.
#' @param plot_CI Indicator if the confidence intervals should be plotted.
#' Only used if \code{y_var} is not specified. Defaults to TRUE.
#' @param legend_limits Optional numeric vector passed as argument \code{limits}
#' to \code{\link[ggplot2]{scale_fill_gradient2}}.
#' 
#' @return Plot grid created with \code{\link[ggpubr]{ggarrange}} (if
#' \code{plot_CI} is TRUE) or a \code{ggplot2} object (if \code{plot_CI} is
#' FALSE).
#' 
#' @import checkmate dplyr ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom mgcv predict.gam
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
#' @seealso plot_APChexamap
#' 
#' @examples
#' library(APCtools)
#' library(mgcv)
#' 
#' data(travel)
#' 
#' # variant A: plot observed mean structures
#' # observed heatmap
#' plot_APCheatmap(dat = travel, y_var = "mainTrip_distance",
#'                 bin_heatmap = FALSE, y_var_logScale = TRUE)
#' 
#' # with binning
#' plot_APCheatmap(dat = travel, y_var = "mainTrip_distance",
#'                 bin_heatmap = TRUE, y_var_logScale = TRUE)
#' 
#' # variant B: plot some smoothed, estimated mean structure
#' model <- gam(mainTrip_distance ~ te(age, period) + residence_region +
#'              household_size + s(household_income), data = travel)
#' 
#' # plot the smooth tensor product surface
#' plot_APCheatmap(dat = travel, model = model, bin_heatmap = FALSE, plot_CI = FALSE)
#' 
#' # ... same plot including the confidence intervals
#' plot_APCheatmap(dat = travel, model = model, bin_heatmap = FALSE)
#' 
#' # the APC dimensions can be flexibly assigned to the x-axis and y-axis
#' plot_APCheatmap(dat = travel, model = model, dimensions = c("age","cohort"),
#'                 bin_heatmap = FALSE, plot_CI = FALSE)
#' 
#' # add some reference lines
#' plot_APCheatmap(dat = travel, model = model, bin_heatmap = FALSE, plot_CI = FALSE,
#'                 markLines_list = list(cohort = c(1910,1939,1955,1980)))
#' 
#' # default binning of the tensor product surface in 5-year-blocks
#' plot_APCheatmap(dat = travel, model = model, plot_CI = FALSE)
#' 
#' # manual binning
#' manual_binning <- list(period = seq(min(travel$period, na.rm = TRUE) - 1,
#'                                     max(travel$period, na.rm = TRUE), by = 5),
#'                        cohort = seq(min(travel$period - travel$age, na.rm = TRUE) - 1,
#'                                     max(travel$period - travel$age, na.rm = TRUE), by = 10))
#' plot_APCheatmap(dat = travel, model = model, plot_CI = FALSE,
#'                 bin_heatmapGrid_list = manual_binning)
#' 
plot_APCheatmap <- function(dat, y_var = NULL, model = NULL,
                            dimensions = c("period","age"), apc_range = NULL,
                            bin_heatmap = TRUE, bin_heatmapGrid_list = NULL,
                            markLines_list = NULL,
                            markLines_displayLabels = c("age","period","cohort"),
                            y_var_logScale = FALSE, plot_CI = TRUE,
                            legend_limits = NULL) {
  
  checkmate::assert_data_frame(dat)
  checkmate::assert_true(!is.null(y_var) | !is.null(model))
  checkmate::assert_character(y_var, len = 1, null.ok = TRUE)
  checkmate::assert_choice(y_var, choices = colnames(dat), null.ok = TRUE)
  checkmate::assert_class(model, classes = "gam", null.ok = TRUE)
  checkmate::assert_character(dimensions, len = 2)
  checkmate::assert_subset(dimensions, choices = c("age","period","cohort"))
  checkmate::assert_list(apc_range, types = "numeric", max.len = 3,
                         null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_subset(names(apc_range), choices = c("age","period","cohort"))
  checkmate::assert_logical(bin_heatmap, len = 1)
  checkmate::assert_list(bin_heatmapGrid_list, min.len = 1, max.len = 3,
                         types = "numeric", null.ok = TRUE)
  checkmate::assert_subset(names(bin_heatmapGrid_list), choices = c("age","period","cohort"))
  checkmate::assert_list(markLines_list, min.len = 1, max.len = 3,
                         types = "numeric", null.ok = TRUE)
  checkmate::assert_subset(names(markLines_list), choices = c("age","period","cohort"))
  checkmate::assert_character(markLines_displayLabels, null.ok = TRUE)
  checkmate::assert_subset(markLines_displayLabels, choices = c("age","period","cohort"))
  checkmate::assert_logical(y_var_logScale, len = 1)
  checkmate::assert_logical(plot_CI, len = 1)
  checkmate::assert_numeric(legend_limits, len = 2, null.ok = TRUE)
  
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  period <- age <- effect <- se <- exp_effect <- exp_se <- upper <- lower <-
    exp_upper <- exp_lower <- cohort <- x <- y <- plot_effect <- plot_lower <-
    plot_upper <- NULL
  
  
  if (!is.null(y_var)) { # plot observed structures
    
    dat <- dat %>% 
      mutate(cohort = period - age) %>% 
      dplyr::rename(effect = y_var) %>% # rename 'y_var' for easier handling
      filter(!is.na(effect))
    
    plot_dat <- dat
    
    # if 'y_var' is not binned, take the average of observations with the same
    # age and period, to prevent overplotting
    if (!bin_heatmap) {
      plot_dat <- plot_dat %>% 
        group_by(period, age) %>% 
        summarize(effect = mean(effect)) %>% 
        ungroup()
    }

    # create some variables and objects, to re-use the model-based code
    plot_dat <- plot_dat %>% 
      mutate(cohort = period - age,
             upper  = effect,
             lower  = effect)
    dat_predictionGrid <- plot_dat
    
    if (y_var_logScale) {
      plot_dat <- plot_dat %>% mutate(effect = log10(effect))
    }
    
    plot_CI      <- FALSE
    used_logLink <- FALSE
    legend_title <- ifelse(!y_var_logScale, paste0("average ",y_var),
                           paste0("average log10(",y_var,")"))
    y_trans      <- "identity"
    
    
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
      mutate(effect = as.vector(prediction$fit),
             se     = as.vector(prediction$se.fit)) %>% 
      mutate(effect = effect - mean(effect)) %>% 
      mutate(lower  = effect - qnorm(0.95) * se,
             upper  = effect + qnorm(0.95) * se)
    
    used_logLink <- (model$family[[2]] %in% c("log","logit")) |
      grepl("Ordered Categorical", model$family[[1]])
    legend_title <- ifelse(used_logLink, "Mean exp effect", "Mean effect")
    y_trans      <- ifelse(used_logLink, "log", "identity")
    
    if (used_logLink) {
      plot_dat <- plot_dat %>% 
        mutate(exp_effect = exp(effect),
               exp_se     = sqrt((se^2) * (exp_effect^2))) %>% 
        mutate(exp_lower  = exp_effect - qnorm(0.975) * exp_se,
               exp_upper  = exp_effect + qnorm(0.975) * exp_se) %>% 
        select(-effect, -se, -upper, -lower) %>% 
        dplyr::rename(effect = exp_effect, se = exp_se,
                      upper  = exp_upper, lower = exp_lower)
    }
    
  }
  
  # filter the data
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
  
  # bin the heatmap surface, if necessary
  if (!bin_heatmap) { # no binning
    
    plot_dat <- plot_dat %>% 
      dplyr::rename(plot_effect = effect, plot_upper = upper, plot_lower = lower)
    
  } else { # bin the heatmap
    
    # define the binning grid, if still necessary
    if (is.null(bin_heatmapGrid_list)) {
      bin_heatmapGrid_list <- list(seq(min(dat_predictionGrid[[dimensions[1]]], na.rm = TRUE) - 1,
                                       max(dat_predictionGrid[[dimensions[1]]], na.rm = TRUE),
                                       by = 5),
                                   seq(min(dat_predictionGrid[[dimensions[2]]], na.rm = TRUE) - 1,
                                       max(dat_predictionGrid[[dimensions[2]]], na.rm = TRUE),
                                       by = 5))
      names(bin_heatmapGrid_list) <- dimensions
    }
    
    dims_toBin       <- names(bin_heatmapGrid_list)
    dims_catVarNames <- paste0(dims_toBin, "_cat")
    
    for (i in 1:length(dims_toBin)) {
      plot_dat[[dims_catVarNames[i]]] <- cut(plot_dat[[dims_toBin[i]]],
                                             breaks = bin_heatmapGrid_list[[dims_toBin[i]]])
    }
    
    plot_dat <- plot_dat %>% 
      group_by_at(vars(dims_catVarNames)) %>% 
      mutate(plot_effect = mean(effect),
             plot_lower  = mean(lower),
             plot_upper  = mean(upper))
  }
  
  
  # create variables x, y, z additional to the APC variables, for easier handling
  plot_dat$x <- plot_dat[[dimensions[1]]]
  plot_dat$y <- plot_dat[[dimensions[2]]]
  dim_3 <- ifelse(!("age" %in% dimensions), "age",
                  ifelse(!("period" %in% dimensions), "period", "cohort"))
  plot_dat$z <- plot_dat[[dim_3]]
  
  x_lab <- capitalize_firstLetter(dimensions[1])
  y_lab <- capitalize_firstLetter(dimensions[2])
  
  
  # overall theme
  gg_theme <- theme(plot.title       = element_text(hjust = 0.5),
                    legend.position  = "bottom",
                    legend.key.width = unit(1.2, "cm"))
  
  # create the base heatmap plot
  gg_effect <- ggplot() +
    geom_tile(data = plot_dat, aes(x = x, y = y, fill = plot_effect)) +
    ggtitle(ifelse(!is.null(y_var), "", "Effect")) +
    xlab(x_lab) + ylab(y_lab) + gg_theme
  
  if (!plot_CI) { # no confidence intervals to be plotted
    
    limits_color <- c(NA,NA)
    gg_list      <- list(gg_effect)
    
  } else { # add heatmaps for the confidence interval borders to the plot
    
    limits_color <- c(min(floor(plot_dat$plot_lower   * 1000) / 1000),
                      max(ceiling(plot_dat$plot_upper * 1000) / 1000))
    
    gg_lower <- ggplot() +
      geom_tile(data = plot_dat, aes(x = x, y = y, fill = plot_lower)) +
      ggtitle("Lower 95% CI boundary") + xlab(x_lab) + gg_theme +
      theme(axis.title.y = element_blank())
    gg_upper <- ggplot() +
      geom_tile(data = plot_dat, aes(x = x, y = y, fill = plot_upper)) +
      ggtitle("Upper 95% CI boundary") + xlab(x_lab) + gg_theme +
      theme(axis.title.y = element_blank())
    
    gg_list <- list(gg_effect, gg_lower, gg_upper)
  }
  
  
  # color scale
  scale_midpoint <- ifelse(!is.null(model), 0, mean(plot_dat$plot_effect))
  gg_list <- lapply(gg_list, function(gg) {
    gg + scale_fill_gradient2(legend_title, limits = legend_limits,
                              trans = y_trans, low = "dodgerblue4",
                              mid = "white", high = "firebrick3",
                              midpoint = scale_midpoint)
  })
  
  
  # mark some age groups / periods / cohorts in each plot
  if (!is.null(markLines_list)) {
    gg_list <- gg_addReferenceLines(gg_list                 = gg_list,
                                    dimensions              = dimensions,
                                    plot_dat                = plot_dat,
                                    markLines_list          = markLines_list,
                                    markLines_displayLabels = markLines_displayLabels)
  }
  
  
  # create final plot output
  if (!plot_CI) {
    plot <- gg_list[[1]] +
      theme(plot.title = element_blank())
    
  } else {
    plot <- ggpubr::ggarrange(plotlist      = gg_list,
                              legend        = "bottom",
                              common.legend = TRUE,
                              ncol          = 3,
                              widths        = c(.34,.32,.32))
  }
  
  return(plot)
}



#' Internal helper to add reference lines in an APC heatmap
#' 
#' Internal helper function to add reference lines in an APC heatmap
#' (vertically, horizontally or diagonally). The function takes an existing list
#' of ggplot objects, adds the specified reference lines in each plot and
#' returns the edited ggplot list again. To be called from within
#' \code{\link{plot_APCheatmap}}.
#' 
#' @inheritParams plot_APCheatmap
#' @param gg_list Existing list of ggplot objects where the reference lines
#' should be marked in each individual ggplot.
#' @param plot_dat Dataset used for creating the heatmap.
#' 
#' @import dplyr ggplot2
#' 
gg_addReferenceLines <- function(gg_list, dimensions, plot_dat, markLines_list,
                                 markLines_displayLabels) {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  x <- y <- x_start <- x_end <- y_start <- y_end <- group <- NULL
  
  
  dim_3 <- ifelse(!("age" %in% dimensions), "age",
                  ifelse(!("period" %in% dimensions), "period", "cohort"))
  
  
  # add vertical lines
  if (dimensions[1] %in% names(markLines_list)) {
    gg_list <- lapply(gg_list, function(gg) {
      gg + geom_vline(xintercept = markLines_list[[dimensions[1]]],
                      col = gray(0.3), lty = 2)
    })
    
    # add labels
    if (dimensions[1] %in% markLines_displayLabels) {
      dim1Labels_dat <- data.frame(x = markLines_list[[dimensions[1]]],
                                   y = max(plot_dat$y))
      
      gg_list <- lapply(gg_list, function(gg) {
        gg + geom_label(data = dim1Labels_dat, aes(x = x, y = y, label = x),
                        hjust = 0, nudge_x = 1, nudge_y = 1)
      })
    }
  }
  
  # add horiztonal lines
  if (dimensions[2] %in% names(markLines_list)) {
    gg_list <- lapply(gg_list, function(gg) {
      gg + geom_hline(yintercept = markLines_list[[dimensions[2]]],
                      col = gray(0.3), lty = 2)
    })
    
    # add labels
    if (dimensions[2] %in% markLines_displayLabels) {
      dim2Labels_dat <- data.frame(x = max(plot_dat$x),
                                   y = markLines_list[[dimensions[2]]])
      
      gg_list <- lapply(gg_list, function(gg) {
        gg + geom_label(data = dim2Labels_dat, aes(x = x, y = y, label = y),
                        vjust = 0, nudge_x = 1, nudge_y = 1)
      })
    }
  }
  
  # add diagonal lines
  if (dim_3 %in% names(markLines_list)) {
    # create a dataset for the line segments
    dat_segments <- lapply(markLines_list[[dim_3]], function(z) {
      
      data.frame(x_start = case_when(dim_3 == "cohort" & dimensions[1] == "period" ~ min(plot_dat$y) + z,
                                     dim_3 == "cohort" & dimensions[1] == "age"    ~ min(plot_dat$y) - z,
                                     dim_3 == "period"                             ~ z - min(plot_dat$y),
                                     dim_3 == "age"    & dimensions[1] == "cohort" ~ min(plot_dat$y) - z,
                                     dim_3 == "age"    & dimensions[1] == "period" ~ min(plot_dat$y) + z),
                 x_end   = case_when(dim_3 == "cohort" & dimensions[1] == "period" ~ max(plot_dat$y) + z,
                                     dim_3 == "cohort" & dimensions[1] == "age"    ~ max(plot_dat$y) - z,
                                     dim_3 == "period"                             ~ z - max(plot_dat$y),
                                     dim_3 == "age"    & dimensions[1] == "cohort" ~ max(plot_dat$y) - z,
                                     dim_3 == "age"    & dimensions[1] == "period" ~ max(plot_dat$y) + z),
                 y_start = min(plot_dat$y),
                 y_end   = max(plot_dat$y),
                 group   = ifelse(match(z, markLines_list[[dim_3]]) == 1,
                                  paste(capitalize_firstLetter(dim_3), z),
                                  as.character(z)))
      
    }) %>% dplyr::bind_rows()
    
    # if necessary, cut each segment, s.t. it doesn't exceed the plot limits
    dat_segments <- ensure_segmentsInPlotRange(dat_segments, plot_dat)
    
    # add the segments to the plots
    gg_list <- lapply(gg_list, function(gg) {
      gg +
        geom_segment(data = dat_segments, aes(x = x_start, xend = x_end,
                                              y = y_start, yend = y_end,
                                              group = group),
                     col = gray(0.3), lty = 2)
    })
    
    # add labels
    if (dim_3 %in% markLines_displayLabels) {
      gg_list <- lapply(gg_list, function(gg) {
        gg + geom_label(data = dat_segments,
                        aes(x = x_end, y = y_end, label = group))
      })
    }
  }
  
  return(gg_list)
}



#' Internal helper for gg_addReferenceLines to keep diagonal lines in the plot range
#' 
#' Internal helper function to be called from within
#' \code{\link{gg_addReferenceLines}}. This function takes the dataset prepared
#' for adding diagonal reference lines in the plot, checks if some diagonals
#' exceed the plot limits, cuts them accordingly, if necessary, and again
#' returns the corrected dataset.
#' 
#' @inheritParams gg_addReferenceLines
#' @param dat_segments Dataset containing information on the diagonal reference
#' lines.
#' 
ensure_segmentsInPlotRange <- function(dat_segments, plot_dat) {
  
  x_range <- range(plot_dat$x)
  
  slopes <- (dat_segments$y_end - dat_segments$y_start) /
    (dat_segments$x_end - dat_segments$x_start)
  
  for (i in 1:nrow(dat_segments)) {
    
    dat_i <- dat_segments[i,]
    
    # since the lines in dat_segments sometimes have negative slope, flexibly
    # retrieve the start and end values from the columns
    x_start_var <- ifelse(dat_i$x_start < dat_i$x_end, "x_start", "x_end")
    y_start_var <- ifelse(dat_i$x_start < dat_i$x_end, "y_start", "y_end")
    x_end_var   <- ifelse(dat_i$x_end > dat_i$x_start, "x_end", "x_start")
    y_end_var   <- ifelse(dat_i$x_end > dat_i$x_start, "y_end", "y_start")
    
    # check the start of the line
    if (dat_i[[x_start_var]]  < x_range[1]) {
      dat_segments[i, x_start_var] <- x_range[1]
      dat_segments[i, y_start_var] <- dat_i[[y_end_var]] - slopes[i] * (dat_i[[x_end_var]] - x_range[1])
    }

    # check the end of the line
    if (dat_i[[x_end_var]] > x_range[2]) {
      dat_segments[i, x_end_var] <- x_range[2]
      dat_segments[i, y_end_var] <- dat_i[[y_start_var]] + slopes[i] * (x_range[2] - dat_i[[x_start_var]])
    }
  }
  
  return(dat_segments)
}