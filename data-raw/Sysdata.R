# Code that produces internal data sets used in this package


# Lambdas were generated using this repository: https://github.com/FintelmannLabDevelopmentTeam/FHS_SAT
load("model_data/lambdas.RData")

# Make available as Sysdata.rda
usethis::use_data(lambdas, internal = TRUE, overwrite = TRUE)

