
test_that("plot_linearEffects", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- gam(mainTrip_distance ~ te(period, age) +
                 household_size + residence_region,
               data = travel)
  
  gg <- plot_linearEffects(model)
  
  expect_s3_class(gg, class = c("gg","ggplot"))
})


test_that("plot_1Dsmooth", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- gam(mainTrip_distance ~ te(period, age) +
                 s(household_income) +
                 household_size + residence_region,
               data = travel)
  
  # plot_1Dsmooth
  gg <- plot_1Dsmooth(model, select = 2, alpha = 0.1)
  
  expect_s3_class(gg, class = c("gg","ggplot"))

  expect_error(plot_1Dsmooth(model, select = 20))
  
  # get_plotGAMobject
  x <- APCtools:::get_plotGAMobject(model)
  
  expect_identical(names(x[[1]])[1:3], c("x","y","scale"))
})
