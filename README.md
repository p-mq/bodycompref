# bodycompref
Calculate reference values for body composition

This R package allows batch calculation of reference values and derived metrics for skeletal muscle and subcutaneous and visceral adipose tissue.
Please also refer to aour website including additional information and a graphical representation: https://bodycomp-metrics.mgh.harvard.edu

Reference values are based on the following publications:
Marquardt JP et al.(2024),
"Subcutaneous and Visceral adipose tissue Reference Values from Framingham Heart Study Thoracic and Abdominal CT",
*Investigative Radiology*
and
Tonnesen PE et al. (2023),
"Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment [column] The Framingham Heart Study",
*Investigative Radiology*,
<doi:10.1097/RLI.0000000000001012>.

## Installation

To download this package from GitHub, please use your standard method of installing non-CRAN packages. If you're installing packages from GitHub for the first time, you can try using the *installr* package:
installr::install.github("https://github.com/p-mq/bodycompref")


## Usage
The package contains GAMLSS models to produce sex-, level-, metric-, and age-specific mappings of a measurement (raw value) to a corresponding z-score/percentile/percent of predicted value. With exception of percent predicted, the transformations are bidirectional.

You can use the same wrapper function (bodycomp_reference) for each transformation and specify the output or use the specific functions (reference_values, reference_percentiles, reference_z_scores, percent_predicted). See the vignette for quick start and the individual function documentation for further information. To open the vignette, use the graphic interface or use:
vignette("Vignette", package = "bodycompref")
