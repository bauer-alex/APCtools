
test_that("plot_APCheatmap", {
  
  testthat::skip_if_not_installed("mgcv")
  
  data(drug_deaths)
  
  # plot hexamap of observed data
  gg1 <- plot_APCheatmap(dat = drug_deaths, y_var = "mortality_rate")
  
  gg2 <- plot_APCheatmap(dat = drug_deaths, y_var = "mortality_rate",
                         bin_heatmap = FALSE,
                         apc_range = list("cohort" = 1980:2010))
  
  gg3 <- plot_APCheatmap(dat = drug_deaths, y_var = "mortality_rate",
                         markLines_list = list("cohort" = c(1985,1993)),
                         apc_range = list("cohort" = 1980:2010))
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  expect_s3_class(gg3, class = c("gg","ggplot"))
  
  
  # plot hexamap of smoothed structure
  model <- gam(mortality_rate ~ te(period, age), data = drug_deaths)
  
  gg <- plot_APCheatmap(dat = drug_deaths, model = model)
  
  expect_s3_class(gg, class = c("gg","ggplot"))
})



test_that("plot_APChexamap", {
  
  testthat::skip_if_not_installed("mgcv")
  
  data(drug_deaths)
  
  # helper functions
  expect_identical(compute_xCoordinate(period_vec = c(1980,1999)),
                   c(1714.7302994931883,1731.1847821650927))
  expect_identical(compute_yCoordinate(period_vec = c(1990, 1999), age_vec = c(20,50)),
                   c(-975.0, -949.5))
  
  # plot hexamap of observed data
  expect_null(plot_APChexamap(dat = drug_deaths, y_var = "mortality_rate"))
  
  expect_null(plot_APChexamap(dat = drug_deaths, y_var = "mortality_rate",
                              y_var_logScale = TRUE, color_range = c(1,50),
                              apc_range = list("cohort" = 1980:2010)))
  
  # error when 0 values are logarithmized
  expect_error(plot_APChexamap(dat = drug_deaths, y_var = "mortality_rate",
                               y_var_logScale = TRUE, color_range = c(0,50)))
  
  
  # plot hexamap of smoothed structure
  model <- gam(mortality_rate ~ te(period, age), data = drug_deaths)
  
  expect_null(plot_APChexamap(dat = drug_deaths, model = model))
})
