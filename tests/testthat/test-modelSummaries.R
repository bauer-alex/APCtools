
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



test_that("create_modelSummary", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- bam(mainTrip_distance ~ te(period, age) + household_size +
                 residence_region, data = travel)
  model_logLink <- gam(mainTrip_distance ~ te(period, age) +
                         s(household_income) + household_size +
                         residence_region,
                       family = Gamma(link = "log"), data = travel)
  
  model_list <- list("Model A" = model,
                     "Model B" = model_logLink)
  
  # create_modelSummary
  res <- create_modelSummary(model_list, digits = 4)
  
  expect_length(res, 2)
  expect_s3_class(res[[1]], "knitr_kable")
  expect_s3_class(res[[2]], "knitr_kable")
  
  
  # extract_summary_linearEffects
  tab1 <- APCtools:::extract_summary_linearEffects(model)
  tab2 <- APCtools:::extract_summary_linearEffects(model_logLink)
  tab3 <- APCtools:::extract_summary_linearEffects(model_logLink,
                                                   method_expTransform = "delta")
  
  expect_s3_class(tab1, "data.frame")
  expect_s3_class(tab2, "data.frame")
  expect_s3_class(tab3, "data.frame")
  expect_identical(colnames(tab1)[1:3], c("param","coef","se"))
  expect_identical(colnames(tab2)[1:3], c("param","coef","se"))
  expect_identical(colnames(tab3)[1:3], c("param", "coef", "se"))
})
