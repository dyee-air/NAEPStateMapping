# Load required libraries and functions -----------------------------------

library(haven)
library(readxl)
library(EdSurvey)
source("NaepStateMapperUtils.R")

# Source data ----------------------------------------------------------------
DEBUG <- TRUE
year <- 2017

# Path to NAEP data files
path_NAEP_M4 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/M48NT1AT.dat"
path_NAEP_M8 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/M48NT2AT.dat"
path_NAEP_R4 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48RED/Data/R48NT1AT.dat"
path_NAEP_R8 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48RED/Data/R48NT2AT.dat"

# Directory containing state EdFacts data files
dir_State <-
  "R:/project/DSY/NAEP State Mapping/Data/State Data 2017"

# Path to Excel file for NCESSCH remapping.  Sheet names should correspond to subject-grade combinations
# ('M4', 'M8', 'R4', and 'R8')
path_remap <-
  "R:/project/DSY/NAEP State Mapping/NAEP to EDFacts.xlsx"

NAEP_Paths <-
  c(path_NAEP_M4, path_NAEP_M8, path_NAEP_R4, path_NAEP_R8)
names(NAEP_Paths) <- c("M4", "M8", "R4", "R8")


# Initialize objects ------------------------------------------------------

state_df <- loadStateData(dir_State)

out.maps <- list()

for (subj in c("M", "R")) {
  for (grade in c(4, 8)) {
    test_type = paste0(subj, grade)
    
    # Load NAEP data for this subject/grade
    naep_df <- loadNaepData(NAEP_Paths[test_type])
    
    # FOR 2017: Remap first 7 chars for selected schools
    naep_df$orig_ncessch <- naep_df$ncessch
    substring(naep_df[substring(naep_df$ncessch, 1, 7) == '2612000', 'ncessch'], 1, 7) <-
      '2601103'
    
    # Remap NCESSCH values
    naep_df <-
      remapNcessch(naep_df, loadSchMap(path_remap, test_type))
    
    # Initialize state mapper
    if (DEBUG == TRUE) {
      print(
        paste(
          "=================== SUBJECT:",
          subj,
          ":: GRADE:",
          grade,
          "==================="
        )
      )
    }
    out.maps[[test_type]] <-
      computeStateMapping(naep_df, state_df[state_df$subj == subj &
                                              state_df$grade == grade, ])
  }
}