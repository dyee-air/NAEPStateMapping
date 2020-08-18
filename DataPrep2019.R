source("./nsm_datautils.R")

# Paths to NAEP data files
NAEP_PATHS <- list(
  "M4" = "R:/data/NAEP DATA REVIEW/NAEP 2019/NAEP 2019 MRP_ Review/NAEP 2019 MRP_ Review 5/Y50MATREDPUR/Y50MAT/Data/M50NT1AT.dat",
  "M8" = "R:/data/NAEP DATA REVIEW/NAEP 2019/NAEP 2019 MRP_ Review/NAEP 2019 MRP_ Review 5/Y50MATREDPUR/Y50MAT/Data/M50NT2AT.dat",
  "R4" = "R:/data/NAEP DATA REVIEW/NAEP 2019/NAEP 2019 MRP_ Review/NAEP 2019 MRP_ Review 5/Y50MATREDPUR/Y50RED/Data/R50NT1AT.dat",
  "R8" = "R:/data/NAEP DATA REVIEW/NAEP 2019/NAEP 2019 MRP_ Review/NAEP 2019 MRP_ Review 5/Y50MATREDPUR/Y50RED/Data/R50NT2AT.dat"
)

# Directory containing state EdFacts data files
STATE_DIR <-
  "R:/project/State Mapping/2019/Data/State Data"

# Path to Excel file for NCESSCH remapping.  Sheet names should correspond to subject-grade combinations
# ('M4', 'M8', 'R4', and 'R8')
MAP_PATH <-
  "R:/project/State Mapping/2019/Data/School_Remap.xlsx"

# Consortium definitions.
CONSORTIA <- list(
  "LE" = list("fips" = 87,
              "states" = c("LA")),
  "PARCC" = list(
    "fips" = 88,
    "states" = c("CO", "DC", "IL", "MD", "NJ", "NM", "RI")
  ),
  # SBAC: Need to exclude NV in grade 8, NH in all grades
  "SBAC" = list(
    "fips" = 89,
    "states" = c(
      "CA",
      "CT",
      "DE",
      "HI",
      "ID",
      "MT",
      "ND",
      "NV",
      "OR",
      "SD",
      "VT",
      "WA",
      "WV"
    )
  ),
  "ACT" = list("fips" = 90,
               "states" = c("AR", "AL"))
)

###############################################################################

DATASETS <- list()

loadMappingData <- function() {
  cat("Loading state data from directory:", STATE_DIR, "\n")
  state_data <- loadStateData(STATE_DIR)
  
  # FOR 2017: Reorder columns, drop 'st' variable, and rename 'subj' to 'subject'
  state_data <-
    state_data[, c("state", "ncessch", "subj", "grade", "nt", "n3")]
  colnames(state_data)[colnames(state_data) == "subj"] <- "subject"
  
  
  for (test in names(NAEP_PATHS)) {
    cat("Loading data for test:", test, "\n")
    subject <- substr(test, 1, 1)
    grade <- as.numeric(substr(test, 2, 2))
    
    
    # Get subset of state data for this subject/grade
    df_state <-
      state_data[state_data$subject == subject &
                   state_data$grade == grade,]
    if (NROW(df_state) == 0) {
      warning(paste0("No state data found for test: ", test, "; skipping."))
      next
    }
    
    # Load ncessch remapping data
    df_remap <- loadSchMap(MAP_PATH, test)
    
    # Load NAEP data for this subject/grade
    cat("-- Loading NAEP data...")
    df_naep <- loadNaepData(NAEP_PATHS[[test]])
    cat("done.\n")

    # Remap ncessch values
    df_naep <- remapNcessch(df_naep, df_remap)
    
    
    # Add consortia
    for (grp_name in names(CONSORTIA)) {
      cat("-- Adding consortium '", grp_name, "'...\n", sep = "")
      grp_states <- CONSORTIA[[grp_name]]$states
      grp_fips <- CONSORTIA[[grp_name]]$fips
      
      # FOR 2017: Remove NV from SBAC if grade 8
      # if (grp_name == "SBAC" & grade == 8) {
      #   grp_states <- grp_states[grp_states != "NV"]
      # }
      
      df_naep <-
        addNaepGroup(
          df_naep,
          fips_list = grp_states,
          new_fips = grp_fips,
          label = grp_name
        )
    }
    
    
    DATASETS[[test]] <<- list("naep_data" = df_naep,
                             "state_data" = df_state)
    cat('-- Data saved to DATASETS["', test, '"].\n', sep = "")
  }
  
}
