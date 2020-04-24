# Set wd to path containing .R files
setwd("G:/R Drive/NAEP State Mapping/Programs/R/NAEPStateMapping")
source("./DataPrep_Mega.R")
source("./nsm_computeutils.R")

loadMappingData()

RESULTS <- list()
for (test_name in names(DATASETS)) {
  RESULTS[[test_name]] <-
    computeStateMapping(DATASETS[[test_name]]$naep_data, DATASETS[[test_name]]$state_data, label =
                          test_name)
  
}