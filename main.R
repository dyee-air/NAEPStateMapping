source("./DataPrep2017.R")
source("./nsm_computeutils.R")

loadMappingData()

RESULTS <- list()
for (test_name in names(DATASETS)) {
  RESULTS[[test_name]] <- list()
  for (fips in unique(DATASETS[[test_name]]$naep_data$fips)) {
    lbl <- paste(fips, test_name, sep="_")
    print(paste("Computing results for", lbl))
    naep_df <- DATASETS[[test_name]]$naep_data[DATASETS[[test_name]]$naep_data$fips==fips, ]
    RESULTS[[test_name]][[lbl]] <- computeStateMapping(naep_df, DATASETS[[test_name]]$state_data, label=lbl)
  }
}