source("./nsm_datautils.R")

(function() {
  # FOR BASIC: Load state files
  st13_m <- read_stata("R:/project/State Mapping/Grade-level Estimate/statedata/sy1213state_mth.dta")
  st13_r <- read_stata("R:/project/State Mapping/Grade-level Estimate/statedata/sy1213state_rla.dta")
  st13_m$subj = 'M'
  st13_r$subj = 'R'
  st_dta <- rbind(st13_m, st13_r)
  # Convert numbers
  st_dta$state <- as.numeric(st_dta$fipst)
  st_dta$grade <- as.numeric(st_dta$grade)
  # Copy/rename cols
  st_dta$nt <- st_dta$numvalid
  st_dta$n3 <- st_dta$num_base
  # Remove STATES-GRADES-SUBJECTS where numvalid == numbase
  ag <- aggregate(st_dta[, c('numvalid', 'num_base')], by=list(state = st_dta$state, grade = st_dta$grade, subj = st_dta$subj), FUN = sum)
  ag <- ag[ag$numvalid != ag$num_base, seq(3)]
  
  # Load & merge state exclusion data
  st_excl <- read_stata("G:/NAEP State Mapping/State_Exclusion_Data.dta")
  st_excl$state <- st_excl$fips
  included <- st_excl[st_excl$included==1, c('state', 'grade', 'subj')]

  orig_dta <<- st_dta 
  st_dta <- merge(st_dta, ag, by=c('state', 'grade', 'subj'))
  st_dta <<- merge(st_dta, included, by=c('state', 'grade', 'subj'))
  
  

})()

st_counts <-
  local({
    dt <- unique(st_dta[, c('grade', 'subj', 'stnam')])
    aggregate(dt,
              by = list(grade = dt$grade, subject = dt$subj),
              FUN = NROW)
  })

st_pcts <- 
  local({
    st_totals <- aggregate(orig_dta[, c('num_prof', 'num_base', 'numvalid')], by=list(state=orig_dta$stnam, grade=orig_dta$grade, subject=orig_dta$subj), FUN=sum)
    st_totals$below_base <- st_totals$numvalid - st_totals$num_base
    st_totals$p_prof <- st_totals$num_prof / st_totals$numvalid
    st_totals$p_base <- st_totals$num_base / st_totals$numvalid
    st_totals$p_below_base <- st_totals$below_base / st_totals$numvalid
    st_totals
  })

write.table(st_pcts, 'clipboard-1024')

# Paths to NAEP data files
NAEP_PATHS <- list(
  "M4" = "R:/data/NAEP/NAEP 2005-2013/PGP48/Y44MAT/Data/M44NT1AT.dat",
  "M8" = "R:/data/NAEP/NAEP 2005-2013/PGP48/Y44MAT/Data/M44NT2AT.dat",
  "R4" = "R:/data/NAEP/NAEP 2005-2013/PGP48/Y44RED/Data/R44NT1AT.dat",
  "R8" = "R:/data/NAEP/NAEP 2005-2013/PGP48/Y44RED/Data/R44NT2AT.dat"
)

# Path to Excel file for NCESSCH remapping.  Sheet names should correspond to subject-grade combinations
# ('M4', 'M8', 'R4', and 'R8')
MAP_PATH <-
  "R:/project/DSY/NAEP State Mapping/NCESSch_Mapping.xlsx"


###############################################################################

DATASETS <- list()

loadMappingData <- function() {
  # cat("Loading state data from directory:", STATE_DIR, "\n")
  # state_data <- loadStateData(STATE_DIR)
  state_data <- st_dta
  
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
    df_remap <- loadSchMap(MAP_PATH, test, 3, 4)
    
    # Load NAEP data for this subject/grade
    cat("-- Loading NAEP data...")
    df_naep <- loadNaepData(NAEP_PATHS[[test]])
    cat("done.\n")
    
    # Remap ncessch values
    df_naep <- remapNcessch(df_naep, df_remap)

    
    
    DATASETS[[test]] <<- list("naep_data" = df_naep,
                             "state_data" = df_state)
    cat('-- Data saved to DATASETS["', test, '"].\n', sep = "")
  }
  
}
