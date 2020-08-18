# Set wd to path containing .R files
setwd("G:/R Drive/NAEP State Mapping/Programs/R/NAEPStateMapping")
source("./DataPrep2019.R")
source("./nsm_computeutils.R")

loadMappingData()

RESULTS <- list()
for (test_name in names(DATASETS)) {
  RESULTS[[test_name]] <- list()
  for (fips in unique(DATASETS[[test_name]]$naep_data$fips)) {
    lbl <- paste(fips, test_name, sep="_")
    print(paste("Computing results for", lbl))
    naep_df <- DATASETS[[test_name]]$naep_data[DATASETS[[test_name]]$naep_data$fips==fips, ]
    RESULTS[[test_name]][[lbl]] <- computeStateMapping(naep_df, DATASETS[[test_name]]$state_data, label=lbl, DEBUG = TRUE)
  }
}

TABLES <- list()
M4 <- t(sapply(RESULTS$M4, function(r) c(r$cut.score, r$cut.se)))
M8 <- t(sapply(RESULTS$M8, function(r) c(r$cut.score, r$cut.se)))
R4 <- t(sapply(RESULTS$R4, function(r) c(r$cut.score, r$cut.se)))
R8 <- t(sapply(RESULTS$R8, function(r) c(r$cut.score, r$cut.se)))