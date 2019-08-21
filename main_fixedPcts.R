# Set wd to path containing .R files
setwd("G:/R Drive/NAEP State Mapping/Programs/R/NAEPStateMapping")
source("./DataPrepFixedPcts.R")
source("./nsm_computeutils.R")

loadMappingData()

RESULTS <- list()
for (test_name in names(DATASETS)) {
  RESULTS[[test_name]] <- list()
  state_pcts <- DATASETS[[test_name]]$state_pcts
  naep_df <- DATASETS[[test_name]]$naep_data
  for (fips in unique(DATASETS[[test_name]]$naep_data$fips)) {
    lbl <- paste(fips, test_name, sep="_")
    print(paste("Computing results for", lbl))
    naep_state_df <- naep_df[naep_df$fips==fips, ]
    state_pct <- state_pcts$pctprof[state_pcts$fips==fips]
    point_est <- computePointEstimates(naep_state_df, pct.prof.custom=state_pct)
    std_errs <- computeStandardErrors(naep_state_df, cut.scores=point_est$cut.scores, pct.prof.custom=state_pct)
    RESULTS[[test_name]][[lbl]] <- list(pct.prof=state_pct, cut.mean=point_est$cut.mean, se=std_errs$se)
  }
}
