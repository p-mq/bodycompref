# bodycompref
Calculate reference values for body composition

This R package allows batch calculation of reference values and derived metrics for skeletal muscle and subcutaneous and visceral adipose tissue.
Please also refer to aour website including additional information and a graphical representation: https://bodycomp-metrics.mgh.harvard.edu

Reference values are based on the following publications:
Tonnesen PE, Mercaldo ND, Tahir I, Dietrich ASW, Amari W, Graur A, Allaire B, Bouxsein ML, Samelson EJ, Kiel DP, Fintelmann FJ. Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment: The Framingham Heart Study.
Investigative Radiology, 2023
Marquardt JP, Tonnesen PE, Mercaldo ND, Graur A, Allaire B, Bouxsein ML, Samelson EJ, Kiel DP, Fintelmann FJ.
Subcutaneous and Visceral adipose tissue Reference Values from Framingham Heart Study Thoracic and Abdominal CT
Under Review

## Usage
The package contains GAMLSS models to produce sex-, level-, metric-, and age-specific mappings of a measurement (raw value) to a corresponding z-score/percentile/percent of predicted value. With exception of percent predicted, the transformations are bidirectional.

You can use the same wrapper function (bodycomp_reference) for each transformation and specify the output or use the specific functions (reference_values, reference_percentiles, reference_z_scores, percent_predicted). See the package documentation on the individual functions for examples.
