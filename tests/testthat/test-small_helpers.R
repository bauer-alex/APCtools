
test_that("capitalize_firstLetter", {
  
  value1 <- "habedere"
  value2 <- "Servus"
  value3 <- 2021
  value4 <- TRUE
  
  expect_identical(APCtools:::capitalize_firstLetter(value1), "Habedere")
  expect_identical(APCtools:::capitalize_firstLetter(value2), "Servus")
  expect_error(APCtools:::capitalize_firstLetter(value3))
  expect_error(APCtools:::capitalize_firstLetter(value4))
})
