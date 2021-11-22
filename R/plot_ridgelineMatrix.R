
#' Create a matrix of ridgeline plots
#' 
#' @export
#' 
plot_ridgelineMatrix <- function(data, ages = NULL, periods = NULL, cohorts = NULL,
                                 log = TRUE, adjust = 1) {
  
  ########### TODO Ab hier muss Funktion noch überarbeitet werden
  
  dat$cohort <- dat$period - dat$age
  
  
  # Filtering of data:
  if (!is.null(ages)) {
    data <- data %>% filter(age %in% unlist(!!ages))
  }
  if (!is.null(periods)) {
    data <- data %>% filter(period %in% unlist(!!periods))
  }
  if (!is.null(cohorts)) {
    data <- data %>% filter(birth_year %in% unlist(!!cohorts))
  }
  
  # Determination of factor labels: which are supposed to be plotted:
  if (!is.null(ages)) {
    lapply(X = seq_along(ages), FUN = function(index) {
      if (length(ages[[index]]) > 1) {
        data <<- data %>%
          mutate(age = case_when(data$age %in% ages[[index]] ~
                                   paste0(ages[[index]][1], " to ",
                                          ages[[index]][length(ages[[index]])]),
                                 TRUE ~ as.character(data$age)))
      }
      return(data)
    })
    data$age <- as.factor(data$age)
    data$age <- factor(x = data$age,
                       levels = sort(x = levels(data$age), decreasing = TRUE))
  }
  if (!is.null(periods)) {
    lapply(X = seq_along(periods), FUN = function(index) {
      if (length(periods[[index]]) > 1) {
        data <<- data %>%
          mutate(period = case_when(data$period %in% periods[[index]] ~
                                      paste0(periods[[index]][1], " to ",
                                             periods[[index]][length(periods[[index]])]),
                                    TRUE ~ as.character(data$period)))
      }
      return(data)
    })
    data$period <- as.factor(data$period)
    data$period <- factor(x = data$period,
                          levels = sort(x = levels(data$period),
                                        decreasing = FALSE))
  }
  if (!is.null(cohorts)) {
    lapply(X = seq_along(cohorts), FUN = function(index) {
      if (length(cohorts[[index]]) > 1) {
        data <<- data %>%
          mutate(birth_year = case_when(data$birth_year %in% cohorts[[index]] ~
                                          paste0(cohorts[[index]][1], " to ",
                                                 cohorts[[index]][length(cohorts[[index]])]),
                                        TRUE ~ as.character(data$birth_year)))
      }
    })
    data$cohort <- as.factor(data$birth_year)
    data$cohort <- factor(x = data$cohort,
                          levels = sort(x = levels(data$cohort),
                                        decreasing = FALSE))
  }
  
  # Definition of axis labels and facets:
  if (is.null(ages)) {
    y_var <- "Period"
    sub_var <- "Cohort"
    facet_formula <- period ~ cohort
  }
  if (is.null(periods)) {
    y_var <- "Age"
    sub_var <- "Cohort"
    facet_formula <- age ~ cohort
  }
  if (is.null(cohorts)) {
    y_var <- "Age"
    sub_var <- "Period"
    facet_formula <- age ~ period
  }
  
  # Creation of a ridgeline plot for any age-period combination:
  plot <- plot_ridgeline(data = data, age = ages, period = periods,
                         cohort = cohorts, log = TRUE, weighted = TRUE,
                         adjust = 1, multiple = TRUE) +
    scale_x_continuous(breaks = c(1, 2, 3, 4),
                       labels = c(expression(10^1), expression(10^2),
                                  expression(10^3), expression(10^4))) +
    facet_grid(facets = facet_formula, switch = "y") +
    labs(subtitle = sub_var, y = y_var,
         x = "Travel distance [km] on log10 scale") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.y = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.spacing.x = unit(1.1, "lines"),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12),
          strip.text.y.left = element_text(angle = 0))
  return(plot)
}