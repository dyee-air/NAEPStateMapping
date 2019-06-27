# Helper Functions
library(EdSurvey)
library(haven)
library(readxl)
source("NaepStateMap.R")

################################################################################
# Utilities for loading and preparing NAEP and EdFacts data for state mapping
################################################################################

loadNaepData <- function(naep.path) {
  naep.input <- readNAEP(naep.path)
  
  naep.vars <- c("ncessch",
                 "fips",
                 "origwt",
                 "stypcls")
  score.vars <- searchSDF("rpcm", naep.input)$variableName
  jkwt.vars <- searchSDF("srwt", naep.input)$variableName
  
  df <- getData(naep.input, c(naep.vars, score.vars, jkwt.vars))
  colnames(df) <- tolower(colnames(df))
  df <- df[df$stypcls=="Public", ]
  
  df$stateabbr <- state.abb[match(df$fips, state.name)]
  df$stateabbr[df$fips==11] <- "DC"
  df$stateabbr[is.na(df$stateabbr)] <- as.character(df$fips[is.na(df$stateabbr)])
  
  df <- df[order(df$ncessch, df[score.vars[1]]), ]
  rownames(df) <- NULL
  
  df$fips <- lfactor(as.numeric(df$fips), levels=unique(as.numeric(df$fips)), labels=unique(df$stateabbr))
  
  return(df)
}

loadStateData <- function(state_dir) {
  file_list <-
    list.files(state_dir, pattern = '[[:alpha:]]{2}_20[[:digit:]]{2}.sas7bdat')
  
  df <- data.frame()

  for (file_name in file_list) {
    stabb <- toupper(substr(file_name, 1, 2))
    
    if (!(stabb %in% c(state.abb, "DC", "PR"))) {
      next
    }
    
    tmp_file <- read_sas(paste0(state_dir, '/', file_name))
    colnames(tmp_file) <- tolower(colnames(tmp_file))
    tmp_file$state <- stabb
    
    # For 2015 (and earlier?): Get rows for "Total" group only
    if ("group" %in% colnames(tmp_file)) {
      tmp_file <- tmp_file[tmp_file$group == 0, ]
    }
    
    df <- rbind(df, tmp_file)
  }
  
  return(df)
}

addNaepGroup <-
  function(naep_data,
           fips_list,
           new_fips,
           label = NULL) {

    ndf <- naep_data
    df <- ndf[ndf$fips %in% fips_list, ]
    
    if (is.null(label)) {
      label <- as.character(new_fips)
    }
        
    # Need to jump through hoops to work around R's awful handling of factor variables
    fac_lvls <- c(unique(as.numeric(ndf$fips)), new_fips)
    fac_lbls <- c(unique(as.character(ndf$fips)), label)
    
    ndf$fips <- as.numeric(ndf$fips)
    df$fips <- new_fips

    df <- rbind(ndf, df)

    df$fips <- lfactor(df$fips, fac_lvls, fac_lbls)

    return(df)
}

loadSchMap <- function(xls_path, sheet_name, col_old=1, col_new=2) {
  # Load Excel file (sheet_name typically in "M4", "M8", "R4", "R8")
  df <-
    as.data.frame(read_excel(xls_path, sheet = sheet_name))
  
  # Remove single-quotes from NCESSCH strings
  for (col in colnames(df)) {
    df[, col] <- gsub("'", "", as.character(df[[col]]))
  }
  # Rename columns
  df <- df[, c(col_old, col_new)]
  colnames(df) <- c('ncessch', 'ncessch_new')
  
  return(df)
}

remapNcessch <- function(naep_df, remap_df) {
  for (schid in remap_df$ncessch) {
    naep_df[naep_df$ncessch == schid, 'ncessch'] <-
      as.character(remap_df[remap_df$ncessch == schid, 'ncessch_new'])
  }
  
  return(naep_df)
  
}