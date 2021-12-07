
test_that("create_APCsummary", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- gam(mainTrip_distance ~ te(period, age) +
                 household_size + residence_region,
               data = travel)
  
  model_list <- list("Model A" = model,
                     "Model B" = model)
  
  apc_range <- list("age"    = 30:60,
                    "period" = 1980:2000,
                    "cohort" = 1930:1970)
  
  # create_APCsummary
  res <- create_APCsummary(model_list, dat = travel, digits = 4,
                           apc_range = apc_range)
  
  expect_s3_class(res, "knitr_kable")
  
  # passing a model instead of a list of models leads to an error
  expect_error(create_APCsummary(model))
  
  
  # create_oneAPCsummaryTable
  tab <- APCtools:::create_oneAPCsummaryTable(model, dat = travel, apc_range = apc_range)
  
  expect_s3_class(tab, "data.frame")
  expect_identical(tab$effect, c("age","period","cohort"))
})



test_that("create_covariateSummary", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- gam(mainTrip_distance ~ te(period, age) +
                 household_size + residence_region,
               data = travel)
  
  model_list <- list("Model A" = model,
                     "Model B" = model)
  
  # create_covariateSummary
  res <- create_covariateSummary(model_list, digits = 4)
  
  expect_length(res, 2)
  expect_s3_class(res[[1]], "knitr_kable")
  expect_s3_class(res[[2]], "knitr_kable")
  
  
  # extract_summary_linearEffects
  tab <- APCtools:::extract_summary_linearEffects(model)
  
  expect_s3_class(tab, "data.frame")
  expect_identical(colnames(tab)[1:3], c("param","coef","se"))
})
