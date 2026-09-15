## run renv::restore() and state 'y' to intsall all packages missing, when complete remove line of code

# Runs scripts 00 to 16 in order
# Or Run all scripts 00 to 16 from this script run_all.r

#renv::restore()

scripts <- c("00_environment_setup.r", "01_data_import.r", "02_classification_hierarchy.r", "03_category_harmonisation.r",
             "04_geography_harmonisation.r", "05_segregation_indices.r", "06_null_model.r", "07_spatial_scale.r",
             "08_segregation_change.r", "09_hotspot_pairing.r", "10_settlement_classification.r", "11_classification_validation.r",
             "12_ethnicity_birthplace_crosstab.r", "13_arrival_generation.r", "14_change_clusters.r", "15_figures.r",
             "16_tables.r")

for (script in scripts) {
  message("Running ", script)
  if (system2("Rscript", script) != 0) stop(script, " failed", call. = FALSE)
}
