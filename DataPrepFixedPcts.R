source("./nsm_datautils.R")

# Paths to NAEP data files
NAEP_PATHS <- list(
  "M4" = "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/M48NT1AT.dat",
  "M8" = "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/M48NT2AT.dat",
  "R4" = "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48RED/Data/R48NT1AT.dat",
  "R8" = "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48RED/Data/R48NT2AT.dat"
)

# File containing state proficiency proportions
PCTS_PATH <- "G:/R Drive/NAEP State Mapping/StatePcts2017.xlsx"

###############################################################################

DATASETS <- list()
# REWRITE TO RETURN OBJECT INSTEAD OF WRITING TO GLOBAL ENV
loadMappingData <- function() {
  
  state_pcts <- read_excel(PCTS_PATH)
  colnames(state_pcts) <- tolower(colnames(state_pcts))
  for (col in c('fips', 'grade', 'year', 'pctprof')) {
    state_pcts[[col]] <- as.numeric(state_pcts[[col]])
  }
  # Drop rows with pctprof == NA or 0
  state_pcts <- state_pcts[!(is.na(state_pcts$pctprof) | state_pcts$pctprof == 0.0), ]
  # Convert to proportions, if necessary
  if (max(state_pcts$pctprof) > 1.0) {
    state_pcts$pctprof <- state_pcts$pctprof / 100
  }
  
  
  for (test in names(NAEP_PATHS)) {
    cat("Loading data for test:", test, "\n")
    subject <- substr(test, 1, 1)
    grade <- as.numeric(substr(test, 2, 2))

    # Load NAEP data for this subject/grade
    # cat("-- Loading NAEP data...")
    # df_naep <- loadNaepData(NAEP_PATHS[[test]])
    # cat("done.\n")

    DATASETS[[test]] <<- list("naep_data" = 1,
    # DATASETS[[test]] <<- list("naep_data" = df_naep,
                              "state_pcts" = state_pcts[state_pcts$subject==subject & state_pcts$grade==grade, ])
    cat('-- Data saved to DATASETS["', test, '"].\n', sep = "")
  }

}
