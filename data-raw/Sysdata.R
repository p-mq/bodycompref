# Code that produces internal data sets used in this package

# Get reference models for skeletal muscle
# Reference publication: [insert link]
# Download .Rda file https://github.com/FintelmannLabDevelopmentTeam/MuscleReferenceValuesFHS_ShinyApp/blob/main/resources/models_anonymized_2022-12-10.RData to model_data/muscle_models.RData
load("model_data/muscle_models.RData")
muscle_models <- models_anon

# Download fat models [link] to model_data/fat_models.RData
load("model_data/fat_models.RData")
fat_models <- models_anon
rm(models_anon)

# Download fat models [link] to model_data/fat_models.RData
load("model_data/lambdas.RData")

# Merge into one list of models
reference_models <- append(muscle_models, fat_models)

# Make available as Sysdata.rda
usethis::use_data(reference_models, lambdas, internal = TRUE, overwrite = TRUE)

