

# Source data ----------------------------------------------------------------

# Path to NAEP data file
path_NAEP <- "R:/data/NAEP/Math/2015/Y46MAT/Data/M46NT1AT.dat"

# Data frame containing NAEP NCESSCH code remappings
sch_map <-
  read_excel("G:/NAEP State Mapping/NCESSch_Mapping.xlsx", sheet = "M4")
colnames(sch_map) <- tolower(colnames(sch_map))

# Data frame containing manual school (EdFacts) data overrides
sch_override <-
  read_excel("G:/NAEP State Mapping/NCESSch_Mapping.xlsx", sheet = "Override")



# Load required libraries and functions -----------------------------------

library(haven)
library(readxl)
library(EdSurvey)
source("NaepStateMapperUtils.R")
source("NaepStateMapper.R")


# Initialize objects ------------------------------------------------------

naep_df <- loadNaepData(path_NAEP, sch_map[, 3:4])
naep_mapper <-
  NaepStateMapper(naep_df, state.data = sch_override[sch_override$grade ==
                                                       4 & sch_override$subject == "M", c("ncessch", "nt", "n3")])

# Load data for all states
for (st in unique(naep_df$fips)) {
  if (is.na(st)) {
    next
  }
  
  st_abbr <- state.abb[match(st, state.name)]
  path_State <-
    paste0(
      "R:/project/DSY/NAEP State Mapping/Data/State Data 2015/",
      st_abbr,
      "_2015.sas7bdat"
    )
  
  if (!file.exists(path_State)) {
    next
  }
  
  naep_mapper$addStateData(createStateData(path_State))
  
}


# Compute scores ----------------------------------------------------------

naep_mapper$computeCutScores()