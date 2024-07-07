# bodycompref
Calculate reference values for body composition

This R package allows batch calculation of reference values and derived metrics for skeletal muscle and subcutaneous and visceral adipose tissue.
Please also refer to our website including additional information and a graphical representation: https://bodycomp-metrics.mgh.harvard.edu

Reference values are based on the following publications:
Marquardt JP et al.(2025),
"Subcutaneous and Visceral adipose tissue Reference Values from Framingham Heart Study Thoracic and Abdominal CT",
*Investigative Radiology*,
<doi:10.1097/RLI.0000000000001104>
and
Tonnesen PE et al. (2023),
"Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment [column] The Framingham Heart Study",
*Investigative Radiology*,
<doi:10.1097/RLI.0000000000001012>.

## Installation

To download this package from GitHub, please use your standard method of installing non-CRAN packages. If you're installing packages from GitHub for the first time, you can try using the *devtools* package:

`install.packages("devtools")`

`devtools::install_github("p-mq/bodycompref")`


## Usage
The package contains [GAMLSS](https://www.gamlss.com) models to produce sex-, level-, metric-, and age-specific mappings of a measurement (raw value) to a corresponding z-score/percentile/percent of predicted value. With exception of percent predicted, the transformations are bidirectional.

You can use the same wrapper function (bodycomp_reference) for each transformation and specify the output or use the specific functions (reference_values, reference_percentiles, reference_z_scores, percent_predicted). See the [vignette](https://github.com/p-mq/bodycompref/tree/main/vignettes) for quick start and the individual function documentation for further information.


Code tested for compatibility with R 4.4.0
