#' Get reference models
#'
#' For a given combination of metric, sex, and level return the appropriate LMSP model.
#' Imports the non-CRAN packages musclerefdata and adiposerefdata
#'
#' @param fitted_metric string, name of fitted metric (abbreviation, includes a lowercase b prefix for metrics with box-cox-transformation)
#' @param sex character, "Male" or "Female"
#' @param level string, vertebral level, T5, T8, T10, or L3
#'
#' @return LMSP model
#'
#' @author J.Peter Marquardt
.Get_reference_model <- function(fitted_metric, sex, level) {

  if(fitted_metric %in% c("CSMA", "SMI", "SMRA", "SMG")) {
    if (requireNamespace("musclerefdata", quietly = TRUE)) {
      model <- musclerefdata::muscle_models[[paste(fitted_metric, sex, level, sep='_')]]
      return(model)
    }
    else message("To use this functionality, please install the musclerefdata package using the command:\ninstall.packages(\"musclerefdata\", repos = \"https://p-mq.github.io/drat\")")
    return(NULL)
  }

  else if(fitted_metric %in% c("bCSFA", "SATI", "bSATI", "bSATRA", "bSATG", "bCSVFA", "bVATI", "bVATRA", "bVATG", "bTAT", "bTATI", "bVAT_SAT_ratio")) {
    if (requireNamespace("adiposerefdata", quietly = TRUE)) {
      model <- adiposerefdata::fat_models[[paste(fitted_metric, sex, level, sep='_')]]
      return(model)
    }
    else message("To use this functionality, please install the adiposerefdata package using the command\ninstall.packages(\"adiposerefdata\", repos = \"https://p-mq.github.io/drat\")")
    return(NULL)
  }

  else stop("Fitted metric not in list of known fitted metrics")
}


#' Get percentile for a given combination of model and measurement
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the corresponding percentile for a given measurement
#'
#' @param metric character, body composition metric
#' @param sex character, "Female" or "Male"
#' @param level character, used vertebral level
#' @param age integer, age
#' @param measurement numeric, raw value of measurement
#' @param verbose logical, should messages be displayed
#' @param digits integer, digits to round percentile to
#'
#' @return numeric, corresponding percentile
#'
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom gamlss z.scores
#' @importFrom stats pnorm
#' @importFrom sae bxcx
#'
#' @author J. Peter Marquardt
.Get_reference_percentile <- function(metric, sex, level, age, measurement, verbose=FALSE, digits=0) {
  assertthat::assert_that(metric %in% c("CSMA", "SMI", "SMRA", "SMG", "CSFA", "SATI", "SATRA", "SATG", "CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio"))
  assertthat::assert_that(sex %in% c("Female", "Male"))
  assertthat::assert_that(level %in% c("T5", "T8", "T10", "L3", "TAT", "TATI", "VAT_SAT_ratio"))
  assertthat::is.count(age)
  assertthat::assert_that(38 <= age && age <= 80)
  assertthat::is.number(measurement)
  assertthat::assert_that(is.logical(verbose))
  assertthat::is.count(digits)
  if (metric %in% c("CSVFA", "VATI", "VATRA", "VATG")) {
    assertthat::assert_that(level == "L3")
  }
  # Make sure measurement value is in the supported range
  if (metric %in% c("SATG", "VATG")) {
    if (measurement > -1) stop("Measurement must be <= -1")
  }
  if (metric %in% c("SATRA", "VATRA")) {
    if (measurement < -124) stop("Measurement must be >= -124")
  }
  else {
    if (measurement < 1) stop("Measurement must be >= 1")
  }


  # Adjusting for pre-fitting transforms
  fitted_measurement <- measurement
  if(metric %in% c("SATG", "VATG")) {
    fitted_measurement <- -fitted_measurement
  }
  if(metric %in% c("SATRA", "VATRA")) {
    fitted_measurement <- fitted_measurement + 125
  }
  lambda <- .Get_lambda(metric = metric, sex = sex, level = level)
  if (!is.na(lambda)) {
    fitted_measurement <- sae::bxcx(fitted_measurement, lambda)
    fitted_metric <- paste0("b", metric)
  }
  else fitted_metric <- metric


  model <- .Get_reference_model(fitted_metric, sex, level)
  if (is.null(model)) return(NA)  # quiet error handling if data repository is not available

  pred_z <- gamlss::z.scores(model, y = fitted_measurement, x = age)
  pred_percentile <- round(stats::pnorm(pred_z) * 100, digits = digits)


  if(verbose) print(paste0("For a ", age, "-yo ", sex, ", a ", metric, " of ", measurement, " corresponds to percentile ", pred_percentile))
  return(pred_percentile)

}


#' Get z-score for a given combination of model and measurement
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the corresponding z-score for a given measurement
#'
#' @param metric character, body composition metric
#' @param sex character, "Female" or "Male"
#' @param level character, used vertebral level
#' @param age integer, age
#' @param measurement numeric, raw value of measurement
#' @param verbose logical, should messages be displayed
#' @param digits integer, digits to round percentile to
#'
#' @return numeric, corresponding z-score
#'
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom gamlss z.scores
#' @importFrom sae bxcx
#'
#' @author J. Peter Marquardt
.Get_reference_z_score <- function(metric, sex, level, age, measurement, verbose=FALSE, digits=2) {
  assertthat::assert_that(metric %in% c("CSMA", "SMI", "SMRA", "SMG", "CSFA", "SATI", "SATRA", "SATG", "CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio"))
  assertthat::assert_that(sex %in% c("Female", "Male"))
  assertthat::assert_that(level %in% c("T5", "T8", "T10", "L3"))
  assertthat::is.count(age)
  assertthat::assert_that(38 <= age && age <= 80)
  assertthat::is.number(measurement)
  assertthat::assert_that(is.logical(verbose))
  assertthat::is.count(digits)
  if (metric %in% c("CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio")) {
    assertthat::assert_that(level == "L3")
  }
  # Make sure measurement value is in the supported range
  if (metric %in% c("SATG", "VATG")) {
    if (measurement > -1) stop("Measurement must be <= -1")
  }
  if (metric %in% c("SATRA", "VATRA")) {
    if (measurement < -124) stop("Measurement must be >= -124")
  }
  else {
    if (measurement < 1) stop("Measurement must be >= 1")
  }


  # Adjusting for pre-fitting transforms
  fitted_measurement <- measurement
  if(metric %in% c("SATG", "VATG")) {
    fitted_measurement <- -fitted_measurement
  }
  if(metric %in% c("SATRA", "VATRA")) {
    fitted_measurement <- fitted_measurement + 125
  }
  lambda <- .Get_lambda(metric = metric, sex = sex, level = level)
  if (!is.na(lambda)) {
    fitted_measurement <- sae::bxcx(fitted_measurement, lambda)
    fitted_metric <- paste0("b", metric)
  }
  else fitted_metric <- metric


  model <- .Get_reference_model(fitted_metric, sex, level)
  if (is.null(model)) return(NA)  # quiet error handling if data repository is not available

  pred_z <- round(gamlss::z.scores(model, y = fitted_measurement, x = age), digits = digits)


  if(verbose) print(paste0("For a ", age, "-yo ", sex, ", a ", metric, " of ", measurement, " corresponds to a z-score of ", pred_z))
  return(pred_z)

}



#' Get reference for a given combination of model and percentile/z-score
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the corresponding measurement for a given percentile/z-score
#'
#' @param metric character, body composition metric
#' @param sex character, "Female" or "Male"
#' @param level character, used vertebral level
#' @param age integer, age
#' @param percentile numeric, percentile to return value for. If both percentile and z_score are given, only percentile is evaluated
#' @param z_score numeric, z score to return value for. If both percentile and z_score are given, only percentile is evaluated
#' @param verbose logical, should messages be discplayed
#' @param digits integer, digits to round reference value
#'
#' @return numeric, corresponding percentile
#'
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom gamlss centiles.pred
#' @importFrom stats pnorm
#' @importFrom sae bxcx
#'
#' @author J. Peter Marquardt
.Get_reference_value <- function(metric, sex, level, age, percentile=NULL, z_score=NULL, verbose=FALSE, digits=0) {
  assertthat::assert_that(metric %in% c("CSMA", "SMI", "SMRA", "SMG", "CSFA", "SATI", "SATRA", "SATG", "CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio"))
  assertthat::assert_that(sex %in% c("Female", "Male"))
  assertthat::assert_that(level %in% c("T5", "T8", "T10", "L3"))
  assertthat::is.count(age)
  assertthat::assert_that(38 <= age && age <= 80)
  assertthat::assert_that(is.logical(verbose))
  assertthat::is.count(digits)
  if (metric %in% c("CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio")) {
    assertthat::assert_that(level == "L3")
  }


  lambda <- .Get_lambda(metric = metric, sex = sex, level = level)
  if (!is.na(lambda)) {
    fitted_metric <- paste0("b", metric)
  }
  else fitted_metric <- metric


  if (is.null(percentile)) {
    assertthat::is.number(z_score)

    model <- .Get_reference_model(fitted_metric, sex, level)
    if (is.null(model)) return(NA)  # quiet error handling if data repository is not available


    percentile <- stats::pnorm(z_score) * 100
    pred_measurement <- gamlss::centiles.pred(model, xname = "age", xvalues = age, cent = percentile)[1, as.character(percentile)]

    # Reversing pre-fitting transforms
    if (!is.na(lambda)) {
      pred_measurement <- sae::bxcx(pred_measurement, lambda, InverseQ = TRUE)
    }
    if(metric %in% c("SATG", "VATG")) {
      pred_measurement <- -pred_measurement
    }
    if(metric %in% c("SATRA", "VATRA")) {
      pred_measurement <- pred_measurement - 125
    }
    pred_measurement <- round(pred_measurement, digits = digits)

    if(verbose) print(paste0("For a ", age, "-yo ", sex, ", a z-score of ", z_score, " corresponds to a ", metric, " of ", pred_measurement))
  }
  else {
    assertthat::is.count(percentile)

    model <- .Get_reference_model(fitted_metric, sex, level)
    if (is.null(model)) return(NA)  # quiet error handling if data repository is not available

    pred_measurement <- gamlss::centiles.pred(model, xname = "age", xvalues = age, cent = percentile)[1, as.character(percentile)]

    # Reversing pre-fitting transforms
    if (!is.na(lambda)) {
      pred_measurement <- sae::bxcx(pred_measurement, lambda, InverseQ = TRUE)
    }
    if(metric %in% c("SATG", "VATG")) {
      pred_measurement <- -pred_measurement
    }
    if(metric %in% c("SATRA", "VATRA")) {
      pred_measurement <- pred_measurement - 125
    }

    pred_measurement <- round(pred_measurement, digits = digits)

    if(verbose) print(paste0("For a ", age, "-yo ", sex, ", a percentile of ", percentile, " corresponds to a ", metric, " of ", pred_measurement))
  }


  return(pred_measurement)

}


#' Get \% of expected value
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the ratio of actual and expected value (percentile 50) in percent
#'
#' @param metric character, body composition metric
#' @param sex character, "Female" or "Male"
#' @param level character, used vertebral level
#' @param age integer, age
#' @param measurement numeric, raw value of measurement
#' @param verbose logical, should messages be discplayed
#' @param digits integer, digits to round return value
#'
#' @return numeric, corresponding percentile
#'
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom gamlss centiles.pred
#'
#' @author J. Peter Marquardt
.Get_percent_predicted <- function(metric, sex, level, age, measurement, verbose=FALSE, digits=0) {
  assertthat::assert_that(metric %in% c("CSMA", "SMI", "SMRA", "SMG", "CSFA", "SATI", "SATRA", "SATG", "CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio"))
  assertthat::assert_that(sex %in% c("Female", "Male"))
  assertthat::assert_that(level %in% c("T5", "T8", "T10", "L3"))
  assertthat::is.count(age)
  assertthat::assert_that(38 <= age && age <= 80)
  assertthat::assert_that(is.logical(verbose))
  assertthat::is.count(digits)
  if (metric %in% c("CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio")) {
    assertthat::assert_that(level == "L3")
  }
  # Make sure measurement value is in the supported range
  if (metric %in% c("SATG", "VATG")) {
    if (measurement > -1) stop("Measurement must be <= -1")
  }
  if (metric %in% c("SATRA", "VATRA")) {
    if (measurement < -124) stop("Measurement must be >= -124")
  }
  else {
    if (measurement < 1) stop("Measurement must be >= 1")
  }


  expected_measurement <- .Get_reference_value(metric, sex, level, age, percentile = 50, digits = 5)
  percent_predicted <- round(measurement * 100 / expected_measurement, digits = digits)


  if(verbose) print(paste0("For a ", age, "-yo ", sex, ", a ", metric, " of ", measurement, " corresponds to ", percent_predicted, "% of the expected ", round(expected_measurement, digits = 2)))
  return(percent_predicted)

}


#' Get the lambda used to build a model
#'
#' For each reference LMSP model, get the lambdas used before fitting
#'
#' @param metric character, body composition metric
#' @param sex character, "Female" or "Male"
#' @param level character, used vertebral level
#'
#' @return lambda (numeric, range [-2, 2], increments of 0.1)
#'
#' @author J Peter Marquardt
.Get_lambda <- function(metric, sex, level=NA) {
  assertthat::assert_that(metric %in% c("CSMA", "SMI", "SMRA", "SMG", "CSFA", "SATI", "SATRA", "SATG", "CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio"))
  assertthat::assert_that(sex %in% c("Female", "Male"))
  assertthat::assert_that(level %in% c("T5", "T8", "T10", "L3"))
  if (metric %in% c("CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio")) {
    assertthat::assert_that(level == "L3")
  }

  if(metric %in% c("CSMA", "SMI", "SMRA", "SMG")) {
    lambda <- NA  # Muscle metrics were not transfomed before fitting
  }
  else if(metric == "SATI" & sex == "Male" & level %in% c("T8", "T10")) {
    lambda <- NA  # T5/8 SATI in males was not transformed before fitting
  }
  else if(metric %in% c("CSFA", "SATI", "SATRA", "SATG")) {
    lambda <- lambdas[[metric]][[paste0("Sex_", sex)]][[paste0("vertebral_level_", level)]]
  }
  else if(metric %in% c("CSVFA", "VATI", "VATRA", "VATG", "TAT", "TATI", "VAT_SAT_ratio")) {
    # L3 only
    lambda <- lambdas[[metric]][[paste0("Sex_", sex)]]
  }
  else stop("default elephant in Cairo")


  return(lambda)

}


#' Get percentile(s) for a given combination of model and measurement
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the corresponding percentiles for a given measurement
#'
#' @param metric character (vector), body composition metric
#' @param sex character (vector), "Female" or "Male"
#' @param level character (vector), used vertebral level
#' @param age integer (vector), age
#' @param measurement numeric (vector), raw value of measurement
#' @param verbose logical, should messages be discplayed
#' @param digits integer, digits to round percentile to
#'
#' @return numeric, corresponding percentile
#'
#' @examplesIf requireNamespace("adiposerefdata", quietly = TRUE)
#' reference_percentiles(metric=c("CSFA", "CSFA"), sex=c("Female","Male"),
#'                  level=c("T5","L3"), age=c(42,68), measurement=c(50,50))
#'
#' @seealso [bodycomp_reference()]
#'
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @author J. Peter Marquardt
reference_percentiles <- function(metric, sex, level, age, measurement, verbose=FALSE, digits=0) {
  assertthat::assert_that(length(metric) == length(sex)  &
                            length(metric) == length(level) &
                            length(metric) == length(age) &
                            length(metric) == length(measurement)
  )

  percentiles <- sapply(seq(1, length(metric)), function(r) .Get_reference_percentile(metric = metric[r], sex = sex[r], level = level[r],
                                                                                age = age[r], measurement = measurement[r],
                                                                                verbose = verbose, digits = digits))

  if(any(is.na(percentiles))) warning("Some values have returned NA values")


  return(percentiles)

}


#' Get z-scores for a given combination of model and measurement
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the corresponding z-scores for a given set of measurements
#'
#' @param metric character (vector), body composition metric
#' @param sex character (vector), "Female" or "Male"
#' @param level character (vector), used vertebral level
#' @param age integer (vector), age
#' @param measurement numeric (vector), raw value of measurement
#' @param verbose logical, should messages be displayed
#' @param digits integer, digits to round percentile to
#'
#' @return numeric, corresponding z-score
#'
#' @examplesIf requireNamespace("adiposerefdata", quietly = TRUE)
#' reference_z_scores(metric=c("CSFA", "CSFA"), sex=c("Female","Male"),
#'                  level=c("T5","L3"), age=c(42,68), measurement=c(50,50))
#'
#'
#' @seealso [bodycomp_reference()]
#'
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @author J. Peter Marquardt
reference_z_scores <- function(metric, sex, level, age, measurement, verbose=FALSE, digits=2) {
  assertthat::assert_that(length(metric) == length(sex)  &
                            length(metric) == length(level) &
                            length(metric) == length(age) &
                            length(metric) == length(measurement)
  )

  z_scores <- sapply(seq(1, length(metric)), function(r) .Get_reference_z_score(metric = metric[r], sex = sex[r], level = level[r],
                                                                          age = age[r], measurement = measurement[r],
                                                                          verbose = verbose, digits = digits))

  if(any(is.na(z_scores))) warning("Some values have returned NA values")


  return(z_scores)

}


#' Get reference values for a given combination of model and percentile/z-score
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the corresponding measurements to a given percentile/z-score
#'
#' @param metric character (vector), body composition metric
#' @param sex character (vector), ""Female" or "Male"
#' @param level character (vector), used vertebral level
#' @param age integer (vector), age
#' @param percentile numeric (vector), percentile to return value for. If both percentile and z_score are given, only percentile is evaluated
#' @param z_score numeric (vector), z score to return value for. If both percentile and z_score are given, only percentile is evaluated
#' @param verbose logical, should messages be displayed
#' @param digits integer, digits to round return value to
#'
#' @return numeric, corresponding percentile
#'
#' @examplesIf requireNamespace("adiposerefdata", quietly = TRUE)
#' reference_values(metric=c("CSFA", "CSFA"), sex=c("Female","Male"),
#'                  level=c("T5","L3"), age=c(42,68), percentile=c(50,50))
#'
#'
#' @seealso [bodycomp_reference()]
#'
#' @importFrom assertthat assert_that
#' @importFrom stats pnorm
#'
#' @export
#'
#' @author J. Peter Marquardt
reference_values <- function(metric, sex, level, age, percentile=NULL, z_score=NULL, verbose=FALSE, digits=0) {
  assertthat::assert_that(length(metric) == length(sex)  &
                            length(metric) == length(level) &
                            length(metric) == length(age)
  )

  if (is.null(percentile)) {
    assertthat::assert_that(length(z_score) == length(metric))
    percentile <- stats::pnorm(z_score) * 100
  }
  else {
    assertthat::assert_that(length(percentile) == length(metric))
  }
  ref_vals <- sapply(seq(1, length(metric)), function(r) .Get_reference_value(metric = metric[r], sex = sex[r], level = level[r],
                                                                        age = age[r], percentile = percentile[r],
                                                                        verbose = verbose, digits = digits))

  if(any(is.na(ref_vals))) warning("Some values have returned NA values")


  return(ref_vals)

}


#' Get \% of expected value
#'
#' For a given constellation of metric, sex, vertebral level, and age returns the ratio of actual and expected value (percentile 50) in percent
#'
#' @param metric character (vector), body composition metric
#' @param sex character (vector), ""Female" or "Male"
#' @param level character (vector), used vertebral level
#' @param age integer (vector), age
#' @param measurement numeric (vector), raw value of measurement
#' @param verbose logical, should messages be displayed
#' @param digits integer, digits to round return value to
#'
#' @return numeric, corresponding percentile
#'
#' @examplesIf requireNamespace("adiposerefdata", quietly = TRUE)
#' percent_predicted(metric=c("CSFA", "CSFA"), sex=c("Female","Male"),
#'                   level=c("T5","L3"), age=c(42,68), measurement=c(100,200))
#'
#'
#' @seealso [bodycomp_reference()]
#'
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @author J. Peter Marquardt
percent_predicted <- function(metric, sex, level, age, measurement, verbose=FALSE, digits=0) {
  assertthat::assert_that(length(metric) == length(sex)  &
                            length(metric) == length(level) &
                            length(metric) == length(age) &
                            length(metric) == length(measurement)
  )

  perc_pred <- sapply(seq(1, length(metric)), function(r) .Get_percent_predicted(metric = metric[r], sex = sex[r], level = level[r],
                                                                           age = age[r], measurement = measurement[r],
                                                                           verbose = verbose, digits = digits))

  if(any(is.na(perc_pred))) warning("Some values have returned NA values")


  return(perc_pred)

}


#' Get body composition reference values
#'
#' For a given constellation of metric, sex, vertebral level, and age returns either
#'  - the reference percentile
#'  - the reference z-score
#'  - the reference value
#'  - the percent of predicted value
#'
#'  Reference models are available for the following metrics:
#'    - CSMA: Cross-sectional muscle area [cm²]
#'    - SMI: Skeletal Muscle Index [cm²/m²]
#'    - SMRA: Skeletal Muscle Radioattenuation [Hounsfield Units (HU)]
#'    - SMG: Skeletal Muscle Gauge [cm² * HU/ m²]
#'    - CSFA: Cross-sectional (subcutaneous) fat area [cm²]
#'    - SATI: Subcutaneous Adipose Tissue Index [cm²/m²]
#'    - SATRA: Subcutaneous Adipose Tissue Radioattenuation [HU]
#'    - SATG: Subcutaneous Adipose Tissue Gauge [cm² * HU/ m²]
#'    - CSVFA: Cross-sectional Visceral Fat Area [cm²]
#'    - VATI: Visceral Adipose Tissue Index [cm²/m²]
#'    - VATRA: Visceral Adipose Tissue Radioattenuation [HU]
#'    - VATG: Visceral Adipose Tissue Gauge [cm² * HU/ m²]
#'    - TAT: Cross-sectional Total Adipose Tissue Area [cm²] (SATA + VATA)
#'    - TATI: Total Adipose Tissue Index [cm²/m²] (SATI + VATI)
#'    - VAT_SAT_ratio: VAT/SAT ratio []
#'
#'  Measurement values must be >= -124 for SATRA and VATRA, <= -1 for SATG and VATG, and >= 1 for all other metrics.
#'
#'  The reference values are based on LMSP models constructed from the Framingham Heart Study published in the following publications:
#'    - Tonnesen PE, Mercaldo ND, Tahir I, Dietrich ASW, Amari W, Graur A, Allaire B, Bouxsein ML, Samelson EJ, Kiel DP, Fintelmann FJ.
#'      Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment: The Framingham Heart Study.
#'      Investigative Radiology, 2023.
#'    - Marquardt JP, Tonnesen PE, Mercaldo ND, Graur A, Allaire B, Bouxsein ML, Samelson EJ, Kiel DP, Fintelmann FJ.
#'      Subcutaneous and Visceral adipose tissue Reference Values from Framingham Heart Study Thoracic and Abdominal CT.
#'      Under review by Investigative Radiology, 2024.
#'
#' @param metric character (vector), body composition metric.
#' @param sex character (vector), ""Female" or "Male"
#' @param level character (vector), used vertebral level
#' @param age integer (vector), age
#' @param type character, type of value to return, either of "percentile", "z_score", "reference_value", "percent_predicted"
#' @param measurement numeric (vector), raw value of measurement
#' @param percentile numeric (vector), percentile to return value for. If both percentile and z_score are given, only percentile is evaluated
#' @param z_score numeric (vector), z score to return value for. If both percentile and z_score are given, only percentile is evaluated
#' @param verbose logical, should messages be displayed
#' @param digits integer, digits to round return value to
#'
#' @return numeric, corresponding percentile
#'
#' @examplesIf requireNamespace("adiposerefdata", quietly = TRUE)
#' bodycomp_reference(metric=c("CSFA", "CSFA"), sex=c("Female","Male"),
#'                   level=c("T5","L3"), age=c(42,68), measurement=c(100,200),
#'                   type = "percentile")
#'
#'
#' @importFrom assertthat assert_that is.count is.number
#'
#' @export
#'
#' @author J. Peter Marquardt
bodycomp_reference <- function(metric, sex, level, age, type, measurement=NULL, percentile=NULL, z_score=NULL, verbose=FALSE, digits=2) {
  assertthat::assert_that(length(metric) == length(sex)  &
                            length(metric) == length(level) &
                            length(metric) == length(age)
  )
  assertthat::assert_that(type %in% c("percentile", "z_score", "reference_value", "percent_predicted"))

  if (type == "percentile") {
    return(reference_percentiles(metric = metric, sex = sex, level = level, age = age, measurement = measurement, verbose = verbose, digits = digits))
  }

  if (type == "z_score") {
    return(reference_z_scores(metric = metric, sex = sex, level = level, age = age, measurement = measurement, verbose = verbose, digits = digits))
  }

  if (type == "percent_predicted") {
    return(percent_predicted(metric = metric, sex = sex, level = level, age = age, measurement = measurement, verbose = verbose, digits = digits))  }

  if (type == "reference_value") {
    if (is.null(percentile)) {
      return(reference_values(metric = metric, sex = sex, level = level, age = age, z_score = z_score, verbose = verbose, digits = digits))
    }
    else {
      return(reference_values(metric = metric, sex = sex, level = level, age = age, percentile = percentile, verbose = verbose, digits = digits))
    }
  }
  stop("Default elephant in Cairo") # All cases should have already initiated a return

}
