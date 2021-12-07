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
calc_density <- function(dat, y_var, weights_var = NULL, ...) {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  dim1 <- dim2 <- NULL
  
  # remove potential NA values from 'y_var'
  dat <- dat[!is.na(dat[[y_var]]),]
  
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
    
    # 'dimensions' always has two elements only when called from within 'plot_densityMatrix'
    dim1_categories <- levels(dat[[dimensions[1]]])
    dim2_categories <- levels(dat[[dimensions[2]]])
    
    dat_list1 <- lapply(dim1_categories, function(dim1_cat) {
      
      dat_list12 <- lapply(dim2_categories, function(dim2_cat) {
        dim12_rows <- which(dat[[dimensions[1]]] == dim1_cat &
                              dat[[dimensions[2]]] == dim2_cat)
        
        if (length(dim12_rows) < 2) { # return nothing if the combination is not part of the data
          return(NULL)
        }
        
        dens <- stats::density(x       = dat[dim12_rows, y_var, drop = TRUE],
                               weights = weights_vector[dim12_rows], ...)
        dat_dens12 <- data.frame(x         = dens$x,
                                 y         = dens$y,
                                 dim1      = dim1_cat,
                                 dim2      = dim2_cat)
        
        return(dat_dens12)
      })
      
      dat_dens1 <- dplyr::bind_rows(dat_list12)
      
      return(dat_dens1)
    })
    
    dat_dens <- dplyr::bind_rows(dat_list1) %>% 
      mutate(dim1 = factor(dim1, levels = dim1_categories),
             dim2 = factor(dim2, levels = dim2_categories))
    colnames(dat_dens)[colnames(dat_dens) == "dim1"] <- dimensions[1]
    colnames(dat_dens)[colnames(dat_dens) == "dim2"] <- dimensions[2]
    
  }
  
  return(dat_dens)
}



#' Internal helper to add the diagonal highlighting to a ggplot
#' 
#' Internal helper function to highlight diagonals in a density matrix. The
#' function takes an existing ggplot object, adds the diagonal highlighting
#' and returns the edited ggplot object again.
#' 
#' @inheritParams plot_density
#' @param gg Existing ggplot object to which the diagonal highlighting should
#' be added.
#' @param dat_highlightDiagonals Dataset created by
#' \code{\link{create_highlightDiagonalData}} to highlight specific diagonals
#' in a density matrix.
#' 
#' @import dplyr ggplot2
#' @importFrom scales hue_pal
#' 
gg_highlightDiagonals <- function(gg, dat, dat_highlightDiagonals) {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  col_group <- NULL
  
  
  diag_dimension <- ifelse(!("age_group" %in% names(dat)), "Age groups",
                           ifelse(!("period_group" %in% names(dat)), "Periods",
                                  "Cohorts"))
  
  ncols_highlight <- length(unique(dat_highlightDiagonals$col_group)) - 1
  col_vector <- c(scales::hue_pal()(ncols_highlight), gray(0.9))
  
  gg <- gg +
    geom_rect(data = dat_highlightDiagonals, aes(col = col_group),
              size = 2, fill = "transparent",
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    scale_color_manual(diag_dimension, values = col_vector)
  
  return(gg)
}



#' Internal helper to create a dataset for ggplot2 to highlight diagonals
#' 
#' Internal helper function to create a dataset for \code{ggplot2} that can
#' be used to highlight specific diagonals in a density matrix.
#' 
#' @inheritParams plot_density
#' 
#' @import dplyr
#' 
create_highlightDiagonalData <- function(dat, highlight_diagonals) {
  
  # some NULL definitions to appease CRAN checks regarding use of dplyr/ggplot2
  dim1 <- dim2 <- col_group <- NULL
  
  
  dimensions <- c()
  if ("age_group" %in% names(dat)) {    dimensions <- append(dimensions, "age_group") }
  if ("period_group" %in% names(dat)) { dimensions <- append(dimensions, "period_group") }
  if ("cohort_group" %in% names(dat)) { dimensions <- append(dimensions, "cohort_group") }
  diag_dimension <- ifelse(!("age_group" %in% names(dat)), "age groups",
                           ifelse(!("period_group" %in% names(dat)), "periods",
                                  "cohorts"))
  
  # create a data.frame from 'highlight_diagonals' for easier handling
  diag_dat <- data.frame(diagonal = unlist(highlight_diagonals, use.names = FALSE))
  diag_dat$label <- sapply(1:length(highlight_diagonals), function(i) {
    label <- ifelse(!is.null(names(highlight_diagonals)),
                    names(highlight_diagonals)[i], paste("Diagonal",LETTERS[i]))
    return(rep(label, times = length(highlight_diagonals[[i]])))
  }) %>% unlist()
  
  # 'dimensions' always has two elements only when called from within 'plot_densityMatrix'
  dim1_categories <- levels(dat[[dimensions[1]]])
  dim2_categories <- levels(dat[[dimensions[2]]])
  
  dat_list1 <- lapply(1:length(dim1_categories), function(i1) {
    
    dat_list12 <- lapply(1:length(dim2_categories), function(i2) {
      
      dat_highlight12 <- data.frame(dim1      = dim1_categories[i1],
                                    dim2      = dim2_categories[i2],
                                    col_group = paste("other",diag_dimension))
      
      col_group_row <- match(i1 + i2 - 1, diag_dat$diagonal)
      if (!is.na(col_group_row)) {
        dat_highlight12$col_group <- diag_dat$label[col_group_row]
      }
      
      return(dat_highlight12)
    })
    
    dat_highlight1 <- dplyr::bind_rows(dat_list12)
    
    return(dat_highlight1)
  })
  
  dat_highlight <- dplyr::bind_rows(dat_list1) %>% 
    mutate(dim1 = factor(dim1, levels = dim1_categories),
           dim2 = factor(dim2, levels = dim2_categories))
  colnames(dat_highlight)[colnames(dat_highlight) == "dim1"] <- dimensions[1]
  colnames(dat_highlight)[colnames(dat_highlight) == "dim2"] <- dimensions[2]
  
  # use 'other age groups / periods / cohorts' as last level for the 'highlight_diagonals' legend
  dat_highlight <- dat_highlight %>% 
    mutate(col_group = factor(col_group, levels = c(unique(diag_dat$label),
                                                    paste("other",diag_dimension))))
  
  return(dat_highlight)
}
