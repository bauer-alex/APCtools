
test_that("plot_variable", {
  
  data(travel)
  
  # metric variable
  gg1 <- plot_variable(dat = travel, y_var = "mainTrip_distance")
  gg2 <- plot_variable(dat = travel, y_var = "mainTrip_distance",
                       plot_type = "line")
  gg3 <- plot_variable(dat = travel, y_var = "mainTrip_distance",
                       apc_dimension = "cohort", log_scale = TRUE)
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  expect_s3_class(gg3, class = c("gg","ggplot"))
  
    
  # categorical variable
  gg4 <- plot_variable(dat = travel, y_var = "household_size")
  gg5 <- plot_variable(dat = travel, y_var = "household_size",
                       geomBar_position = "stack")
  
  expect_s3_class(gg4, class = c("gg","ggplot"))
  expect_s3_class(gg5, class = c("gg","ggplot"))
  
  
  # produce some error messages
  expect_error(plot_variable())
  expect_error(plot_variable(dat = travel, y_var = "nonexistent_variable"))
  expect_error(plot_variable(dat = travel, y_var = "household_size",
                             apc_dimension = "stuff"))
})



test_that("plot_density", {
  
  data(travel)
  
  # some basic plots
  gg1 <- plot_density(dat = travel, y_var = "mainTrip_distance")
  gg2 <- plot_density(dat = travel, y_var = "mainTrip_distance",
                      plot_type = "boxplot")
  gg3 <- plot_density(dat = travel, y_var = "household_size")
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  expect_s3_class(gg3, class = c("gg","ggplot"))
  
  
  # more specific settings
  gg4 <- plot_density(dat = travel, y_var = "mainTrip_distance",
                      y_var_cat_breaks = c(0,5000,10000),
                      y_var_cat_labels = c("A","B"),
                      legend_title     = "test")
  expect_warning({
    expect_warning({
      gg5 <- plot_density(dat = travel, y_var = "mainTrip_distance",
                          weights_var = "sampling_weight", log_scale = TRUE,
                          xlab = "test", ylab = "test")
    })
  })
  gg6 <- plot_density(dat = travel, y_var = "mainTrip_distance",
                      apc_range = list("age"    = 20:50,
                                       "period" = 1990:2000,
                                       "cohort" = 1950:1970))
  
  expect_s3_class(gg4, class = c("gg","ggplot"))
  expect_s3_class(gg5, class = c("gg","ggplot"))
  expect_s3_class(gg6, class = c("gg","ggplot"))
  
  
  # produce some error messages
  expect_error(plot_density())
  expect_error(plot_density(dat = travel, y_var = "nonexistent_variable"))
})


test_that("plot_density_metric and plot_density_categorical", {
  
  data(travel)
  
  # metric
  gg1 <- APCtools:::plot_density_metric(dat = travel, y_var = "mainTrip_distance")
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  
  expect_error(APCtools:::plot_density_metric(dat = travel, y_var = "household_size"))
  
  
  # categorical
  gg2 <- APCtools:::plot_density_categorical(dat = travel, y_var = "household_size")
  
  expect_s3_class(gg2, class = c("gg","ggplot"))
})


test_that("other helper functions", {
  
  data(travel)
  
  # calc_density
  expect_warning({
    expect_warning({
      res1 <- calc_density(dat = travel, y_var = "mainTrip_distance",
                           weights_var = "sampling_weight")
    })
  })
  expect_s3_class(res1, class = "data.frame")
  
  expect_error(calc_density(dat = travel, y_var = "household_size"))
  
  
  # create_groupVariable and create_highlightDiagonalData
  age_groups    <- list(c(20,29),c(30,39),c(40,49))
  cohort_groups <- list(c(1940,1949),c(1950,1959),c(1960,1969))
  dat <- travel %>% 
    mutate(cohort = period - age) %>% 
    mutate(age_group    = create_groupVariable(., "age", groups_list    = age_groups),
           cohort_group = create_groupVariable(., "cohort", groups_list = cohort_groups))
  dat_highlightDiagonals <- APCtools:::create_highlightDiagonalData(dat = dat,
                                                                    highlight_diagonals = list("A" = 1,
                                                                                               "B" = 3))
  expect_s3_class(dat_highlightDiagonals, "data.frame")
  expect_identical(colnames(dat_highlightDiagonals),
                   c("age_group","cohort_group","col_group"))
  
  
  # gg_highlightDiagonals
  gg_raw <- plot_densityMatrix(dat = travel, y_var = "mainTrip_distance",
                               dimensions = c("age","cohort"),
                               age_groups = age_groups, cohort_groups = cohort_groups)
  gg <- APCtools:::gg_highlightDiagonals(gg_raw, travel, dat_highlightDiagonals)
  
  expect_s3_class(gg, class = c("gg","ggplot"))
})



test_that("plot_densityMatrix", {
  
  data(travel)
  
  age_groups    <- list(c(20,29),c(30,39),c(40,49))
  period_groups <- list(c(1980,1989),c(1990,1999),c(2000,2009))
  cohort_groups <- list(c(1940,1949),c(1950,1959),c(1960,1969))
  
  gg1 <- plot_densityMatrix(dat = travel, y_var = "mainTrip_distance",
                            dimensions = c("age","cohort"),
                            age_groups = age_groups, cohort_groups = cohort_groups,
                            highlight_diagonals = list("A" = 1, "B" = 3))
  
  gg2 <- plot_densityMatrix(dat = travel, y_var = "mainTrip_distance",
                            dimensions = c("age","cohort"), plot_type = "boxplot",
                            age_groups = age_groups, cohort_groups = cohort_groups,
                            highlight_diagonals = list("A" = 1, "B" = 3))
  
  gg3 <- plot_densityMatrix(dat = travel, y_var = "household_size",
                            dimensions = c("age","cohort"),
                            age_groups = age_groups, cohort_groups = cohort_groups,
                            highlight_diagonals = list("A" = 1, "B" = 3))
  
  gg4 <- plot_densityMatrix(dat = travel, y_var = "mainTrip_distance",
                            dimensions = c("age","period"),
                            age_groups = age_groups, period_groups = period_groups)
  gg5 <- plot_densityMatrix(dat = travel, y_var = "mainTrip_distance",
                            dimensions = c("period","cohort"),
                            period_groups = period_groups, cohort_groups = cohort_groups)
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  expect_s3_class(gg3, class = c("gg","ggplot"))
  expect_s3_class(gg4, class = c("gg","ggplot"))
  expect_s3_class(gg5, class = c("gg","ggplot"))
})
