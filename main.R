library(haven)
library(readxl)
library(EdSurvey)
source("Utils.R")

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

outdf <- as.data.frame(unique(naep_data[, c("fips", "stateabbr")]))
outdf <- outdf[order(outdf$fips), ]
rownames(outdf) <- NULL

for (st in unique(naep_data$fips)) {
  if (is.na(st)) {
    next
  }
  st_abbr <- state.abb[match(st, state.name)]
  path_State <- paste0("R:/project/DSY/NAEP State Mapping/Data/State Data 2015/", st_abbr, "_2015.sas7bdat")
  
  if (!file.exists(path_State)) {
    next
  }
  
  # state_data <- StateProfData(path_State)
  state_data <- createStateData(path_State, sch_override)
  
  schs <- unique(state_data$ncessch)

  mdta <- merge(state_data, getSchoolWeights(naep_sch_rows), by='ncessch')
  mdta$pctprof = mdta$n3 / mdta$nt
  
  st_pctbelow <- 1-weighted.mean(mdta$pctprof, mdta$weight)
  
  inv_cdf_fun <- function(pv) {
    score_var <- paste0("mrpcm", pv)
    return(getInvCDF(st_pctbelow, getCDFTable(mdta[score_var], mdta$weight)))
  }

  print(paste(st, ",", st_pctbelow, ",", mean(sapply((1:20), inv_cdf_fun))))
}