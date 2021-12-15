
test_that("plot_partialAPCeffects and plot_marginalAPCeffects", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(drug_deaths)
  
  model <- gam(mortality_rate ~ te(period, age), data = drug_deaths)
  drug_deaths$mortality_rate <- drug_deaths$mortality_rate + 1
  model_logLink <- gam(mortality_rate ~ te(period, age),
                       family = Gamma(link = "log"), data = drug_deaths)
  
  # plot_partialAPCeffects
  gg1 <- plot_partialAPCeffects(model, dat = drug_deaths, variable = "age",
                                vlines_vec = c(20,70))
  gg2 <- plot_partialAPCeffects(model, dat = drug_deaths, variable = "period",
                                vlines_vec = c(1990,2010))
  gg3 <- plot_partialAPCeffects(model, dat = drug_deaths, variable = "cohort",
                                vlines_vec = c(1950,1970))
  gg4 <- plot_partialAPCeffects(model_logLink, dat = drug_deaths, variable = "age")
  gg5 <- plot_partialAPCeffects(model_logLink, dat = drug_deaths, variable = "period")
  gg6 <- plot_partialAPCeffects(model_logLink, dat = drug_deaths, variable = "cohort")
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  expect_s3_class(gg3, class = c("gg","ggplot"))
  expect_s3_class(gg4, class = c("gg","ggplot"))
  expect_s3_class(gg5, class = c("gg","ggplot"))
  expect_s3_class(gg6, class = c("gg","ggplot"))
  
  # return the dataset
  dat_list <- plot_partialAPCeffects(model, dat = drug_deaths, variable = "cohort",
                                     return_plotData = TRUE)
  
  expect_length(dat_list, 4)
  expect_s3_class(dat_list[[2]], "data.frame")
  expect_identical(names(dat_list), c("dat_overallEffect","dat_age","dat_period",
                                      "dat_cohort"))
  
  
  # plot_marginalAPCeffects
  gg7 <- plot_marginalAPCeffects(model, dat = drug_deaths, variable = "age")
  gg8 <- plot_marginalAPCeffects(model, dat = drug_deaths, variable = "period")
  gg9 <- plot_marginalAPCeffects(model, dat = drug_deaths, variable = "cohort")
  
  expect_s3_class(gg7, class = c("gg","ggplot"))
  expect_s3_class(gg8, class = c("gg","ggplot"))
  expect_s3_class(gg9, class = c("gg","ggplot"))
  
  # return the dataset
  dat_list2 <- plot_marginalAPCeffects(model, dat = drug_deaths, variable = "period",
                                       return_plotData = TRUE)
  
  expect_length(dat_list2, 4)
  expect_s3_class(dat_list2[[2]], "data.frame")
  expect_identical(names(dat_list2), c("dat_overallEffect","dat_age","dat_period",
                                       "dat_cohort"))
})



test_that("plot_jointMarginalAPCeffects", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(drug_deaths)
  
  model1 <- gam(mortality_rate ~ te(period, age), data = drug_deaths)
  model2 <- gam(mortality_rate ~ te(period, age) + population, data = drug_deaths)
  
  model_list <- list("Model A" = model1, "Model B" = model2)
  
  gg <- plot_jointMarginalAPCeffects(model_list, dat = drug_deaths,
                                     vlines_list = list("age"    = c(20,50),
                                                        "cohort" = c(1950,1980)))
  
  expect_s3_class(gg, class = c("gg","ggplot"))
})
