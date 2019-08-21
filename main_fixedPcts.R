# Set wd to path containing .R files
setwd("G:/R Drive/NAEP State Mapping/Programs/R/NAEPStateMapping")
source("./DataPrepFixedPcts.R")
source("./nsm_computeutils.R")

loadMappingData()

# DSY: Use Primer data until RUD access
# sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
# testdf <- getData(sdf, c('origwt', paste0('mrpcm', seq(5)), paste0('srwt', str_pad(seq(62), 2, pad=0))))
# testdf$fips <- as.factor('AL')

RESULTS <- list()
for (test_name in names(DATASETS)) {
  RESULTS[[test_name]] <- list()
  state_pcts <- DATASETS[[test_name]]$state_pcts
  naep_df <- DATASETS[[test_name]]$naep_data
  for (fips in unique(DATASETS[[test_name]]$state_pcts$st)) {
    lbl <- paste(fips, test_name, sep="_")
    print(paste("Computing results for", lbl))
    naep_state_df <- naep_df[naep_df$fips==fips, ]
    if (!NROW(naep_state_df)) {
      print("No rows found.")
      next
    }
    state_pct <- state_pcts$pctprof[state_pcts$st==fips]
    point_est <- computePointEstimates(naep_state_df, pct.prof.custom=state_pct)
    std_errs <- computeStandardErrors(naep_state_df, cut.scores=point_est$cut.scores, pct.prof.custom=state_pct)
    RESULTS[[test_name]][[lbl]] <- list(pct.prof=state_pct, cut.mean=point_est$cut.mean, se=std_errs$se)
  }
}
