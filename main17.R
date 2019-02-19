# Load required libraries and functions -----------------------------------

library(haven)
library(readxl)
library(EdSurvey)
source("NaepStateMapperUtils.R")
source("NaepStateMapper.R")

# Source data ----------------------------------------------------------------

year = 2017
subject = "M"
grade = 4

# Path to NAEP data files
path_NAEP_M4 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/M48NT1AT.dat"
path_NAEP_M8 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/M48N21AT.dat"
path_NAEP_R4 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/R48NT1AT.dat"
path_NAEP_R8 <-
  "R:/data/NAEP DATA REVIEW/NAEP 2017/NAEP 2017 MRP_Review 5/Y48MATREDPUR/Y48MAT/Data/R48NT2AT.dat"

# Directory containing state EdFacts data files
dir_State <-
  "R:/project/DSY/NAEP State Mapping/Data/State Data 2017"

# Path to Excel file for NCESSCH remapping.  Sheet names should correspond to subject-grade combinations
# ('M4', 'M8', 'R4', and 'R8')
path_remap <-
  "R:/project/DSY/NAEP State Mapping/NAEP to EDFacts.xlsx"



# Initialize objects ------------------------------------------------------

naep_df <- loadNaepData(path_NAEP)
state_df <- loadStateData(dir_State)

# Remap school IDs  -------------------------------------------------------

schmap <-
  read_excel(path_remap, sheet = paste0(subject, grade))
# Remove single-quotes from NCESSCH strings
for (col in colnames(schmap)) {
  schmap[, col] <- gsub("'", "", as.character(schmap[[col]]))
}
# Rename columns
colnames(schmap) <- c('ncessch', 'ncessch_new')

# FOR 2017: Remap first 7 chars for selected schools
naep_df$orig_ncessch <- naep_df$ncessch
substring(naep_df[substring(naep_df$ncessch, 1, 7) == '2612000', 'ncessch'], 1, 7) <-
  '2601103'

# Remap NCESSCH values
naep_df <- remapNcessch(naep_df, schmap)


# nsd <- NaepStateMapper(naep_df[naep_df$fips %in% c("Alabama", "Colorado", "Maryland"),], df.state)
nsd <- NaepStateMapper(naep_df, state_df[state_df$subj == "M" & state_df$grade == 4, ])

# Compute scores ----------------------------------------------------------
nsd$getResults()