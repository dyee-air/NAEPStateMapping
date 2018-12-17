library(haven)
library(readxl)
library(EdSurvey)
source("R:/project/DSY/NAEP State Mapping/Programs/R/Utils.R")

## Test change

##################
# Load libraries & scripts
# Load NAEP data
# Load NAEP override data
# Load State override data
# For each state:
#   Load state data
#   Get unique school list
#   Match schools to (remapped) NAEP schools
#   Compute weighted pct prof for selected schools
#   For each PV:
#      Get cutoff score using ECDF for selected schools
#   Compute mean across all PVs
#   Get sq deviation from mean for each PV

path_NAEP <- "R:/data/NAEP/Math/2015/Y46MAT/Data/M46NT1AT.dat"
# path_State <- "R:/project/DSY/NAEP State Mapping/Data/State Data 2015/TN_2015.sas7bdat"
path_NCESSCH_MAP <- "R:/project/DSY/NAEP State Mapping/NCESSCH_Remap_M4.csv"

sch_map <- read_excel("G:/NAEP State Mapping/NCESSch_Mapping.xlsx", sheet = "M4")
colnames(sch_map) <- tolower(colnames(sch_map))
sch_override <- read_excel("G:/NAEP State Mapping/NCESSch_Mapping.xlsx", sheet = "Override")

# naep_data <- NAEPScoreData(path_NAEP)
naep_data <- createNaepData(path_NAEP, sch_map)
naep_data$data$ncessch_orig = naep_data$data$ncessch
naep_data$data$ncessch <- sapply(naep_data$data$ncessch_orig, getSchId)

for (st in unique(naep_data$data$fips)) {
  if (is.na(st)) {
    next
  }
  st_abbr <- state.abb[match(st, state.name)]
  path_State <- paste0("R:/project/DSY/NAEP State Mapping/Data/State Data 2015/", st_abbr, "_2015.sas7bdat")
  
  if (!file.exists(path_State)) {
    next
  }
  
  # state_data <- StateProfData(path_State)
  state_data <- createSta
  
  schs <- unique(state_data$data$ncessch)
  mdta <- merge(state_data$data, naep_data$getSchoolWeights(schs), by='ncessch')
  
  st_pctbelow <- 1-weighted.mean(mdta$pctprof, mdta$weight)
  
  naep_rows <- naep_data$getSchools(schs)
  
  inv_cdf_fun <- function(pv) {
    score_var <- paste0("mrpcm", pv)
    return(getInvCDF(st_pctbelow, getCDFTable(naep_rows[score_var], naep_rows$origwt)))
  }

  print(paste(st, ",", st_pctbelow, ",", mean(sapply((1:20), inv_cdf_fun))))
}