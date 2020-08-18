# Set wd to path containing .R files
setwd("G:/R Drive/NAEP State Mapping/Programs/R/NAEPStateMapping")
source("./DataPrep_Basic2015.R")
source("./nsm_computeutils.R")

loadMappingData()

RESULTS <- list()
for (test_name in names(DATASETS)) {
  RESULTS[[test_name]] <-
    computeStateMapping(DATASETS[[test_name]]$naep_data, DATASETS[[test_name]]$state_data, label =
                          test_name)
  
}

out <- t(sapply(RESULTS, function(result) result$results[c('cut.score', 'cut.se')]))
write.table(out, 'clipboard-1024')