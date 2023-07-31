# Unit tests for calculating reference values

testthat::test_that(".Get_reference_percentile works", {
  metric <- "CSFA"
  sex <- "Female"
  level <- "T5"
  age <- 40
  measurement <- 208.18
  testthat::expect_equal(.Get_reference_percentile(metric, sex, level, age, measurement), 50)
  testthat::expect_output(.Get_reference_percentile(metric, sex, level, age, measurement, verbose = TRUE), "For a 40-yo Female, a CSFA of 208.18 corresponds to percentile 50")
})


testthat::test_that(".Get_reference_z_score works", {
  metric <- "CSFA"
  sex <- "Female"
  level <- "T5"
  age <- 40
  measurement <- 208.18
  testthat::expect_equal(.Get_reference_z_score(metric, sex, level, age, measurement, digits = 10), 0.0000387444)
  testthat::expect_output(.Get_reference_z_score(metric, sex, level, age, measurement, verbose = TRUE), "For a 40-yo Female, a CSFA of 208.18 corresponds to a z-score of 0")
})


testthat::test_that(".Get_reference_value works", {
  metric <- "CSFA"
  sex <- "Female"
  level <- "T5"
  age <- 40
  percentile <- 50
  z_score <- 0
  testthat::expect_equal(.Get_reference_value(metric, sex, level, age, percentile = percentile, digits = 2), 208.18)
  testthat::expect_equal(.Get_reference_value(metric, sex, level, age, z_score = z_score), 208)
  testthat::expect_output(.Get_reference_value(metric, sex, level, age, z_score = 0.1, verbose = TRUE), "For a 40-yo Female, a z-score of 0.1 corresponds to a CSFA of 220")
})


testthat::test_that(".Get_percent_predicted works", {
  metric <- "CSFA"
  sex <- "Female"
  level <- "T5"
  age <- 40
  measurement <- 104
  testthat::expect_equal(.Get_percent_predicted(metric, sex, level, age, measurement), 50)
  testthat::expect_output(.Get_percent_predicted(metric, sex, level, age, measurement, verbose = TRUE), "For a 40-yo Female, a CSFA of 104 corresponds to 50% of the expected 208")
})


testthat::test_that("reference_percentiles works", {
  metric <- c("CSFA", "CSFA")
  sex <- c("Female", "Male")
  level <- c("T5", "T8")
  age <- c(40, 60)
  measurement <- c(109, 220)
  testthat::expect_equal(reference_percentiles(metric, sex, level, age, measurement), c(17, 80))
})


testthat::test_that("reference_z_scores works", {
  metric <- c("CSFA", "CSFA")
  sex <- c("Female", "Male")
  level <- c("T5", "T8")
  age <- c(40, 60)
  measurement <- c(109, 220)
  testthat::expect_equal(reference_z_scores(metric, sex, level, age, measurement), c(-0.95, 0.84))
})


testthat::test_that("reference_values works", {
  metric <- c("CSFA", "CSFA")
  sex <- c("Female", "Male")
  level <- c("T5", "T8")
  age <- c(40, 60)
  percentile <- c(50, 50)
  testthat::expect_equal(reference_values(metric, sex, level, age, percentile), c(208, 158))
})


testthat::test_that("percent_predicted works", {
  metric <- c("CSFA", "CSFA")
  sex <- c("Female", "Male")
  level <- c("T5", "T8")
  age <- c(40, 60)
  measurement <- c(109, 220)
  testthat::expect_equal(percent_predicted(metric, sex, level, age, measurement), c(52, 139))
})


testthat::test_that(".Get_lambda works", {
  testthat::expect_equal(.Get_lambda("CSFA", "Female", "T5"), 0.6)
  testthat::expect_equal(.Get_lambda("CSVFA", "Female"), 0.3)
  testthat::expect_equal(.Get_lambda("SMG", "Female", "T5"), NA)
})

