# Load required libraries and functions -----------------------------------

library(haven)
library(readxl)
library(EdSurvey)
source("NaepStateMapperUtils-refac.R")

### TO DO:
#       Add Puerto Rico
#       Revamp calling code:
#         List of FIPS vectors to be used for each estimation
#         Should include separate integer for identification? (e.g. PARCC = 88)

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

# PUERTO RICO
path_NAEP_PR_M4 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48PUR/Data/M48NT1PR.dat"
path_NAEP_PR_M8 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48PUR/Data/M48NT1PR.dat"

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

FIPS_LE <- list(name = "LE",
                fips = 87,
                states = c("LA"))
FIPS_PARCC <-
  list(
    name = "PARCC",
    fips = 88,
    states = c("CO",
               "DC",
               "IL",
               "MD",
               "NJ",
               "NM",
               "RI")
  )

# EXclude NV in grade 8, NH in all grades
FIPS_SBAC <-
  list(
    name = "SBAC",
    fips = 89,
    states = c(
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
  )

FIPS_ACT <- list(name = "ACT",
                 fips = 90,
                 states = c("AR",
                            "AL"))

consortia.maps <- list(FIPS_PARCC, FIPS_SBAC, FIPS_ACT)

# SETUP ------------------------------------------------------

# Data for all states
state_df <- loadStateData(dir_State)

out.maps <- list()

for (subj in c("M", "R")) {
  for (grade in c(4, 8)) {
    test_type = paste0(subj, grade)
    
    # Load NAEP data for this subject/grade
    naep_df <- loadNaepData(NAEP_Paths[test_type])
    
    # Create state consortia
    for (cmap in consortia.maps) {
      if (cmap$name == "SBAC" & grade == 8) {
        cmap$states <- cmap$states[cmap$states != "NV"]
      }
      
      naep_df <-
        addNaepGroup(naep_df, cmap$states, cmap$fips, cmap$name)
    }
    
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