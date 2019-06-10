# Helper Functions
library(EdSurvey)
library(haven)
source("NaepStateMap.R")

################################################################################
# Utilities for loading and preparing NAEP and EdFacts data for state mapping
################################################################################

loadNaepData <- function(naep.path, ncessch.map = NULL) {
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
  state_codes <- c(state.abb, "DC", "PR")
  
  df <-
    data.frame(
      state.abb = character(),
      ncessch = character(),
      subj = character(),
      grade = integer(),
      nt = integer(),
      n3 = integer()
    )
  
  for (st in state_codes) {
    path_State <-
      paste0(state_dir, "/",
             st,
             "_", year, ".sas7bdat")
    
    if (!file.exists(path_State)) {
      next
    }
    
    # NOTE: read_sas() returns a tibble, not a data.frame
    df.file <- read_sas(path_State)
    colnames(df.file) <- tolower(colnames(df.file))
    
    # For 2015 (and earlier?): Get rows for "Total" group only
    if ("group" %in% colnames(df.file)) {
      df.file <- df.file[df.file$group == 0,]
    }
    df.file$state.abb <- st
    df.file <- df.file[, c("state.abb", "ncessch", "subj", "grade", "nt", "n3")]
    
    df <- rbind(df, df.file)
    
  }
  
  return(df)
}

addNaepGroup <-
  function(naep.data,
           fips.list,
           new.fips,
           new.fips.label = NULL) {

    ndf <- naep.data
    df <- ndf[ndf$fips %in% fips.list, ]

    fips.label <- as.character(new.fips)
    
    if (!is.null(new.fips.label)) {
      fips.label <- new.fips.label
    }
        
    # Need to jump through hoops to work around R's awful handling of factor variables
    fac.lvls <- c(unique(as.numeric(ndf$fips)), new.fips)
    fac.lbls <- c(unique(as.character(ndf$fips)), new.fips.label)
    
    ndf$fips <- as.numeric(ndf$fips)
    df$fips <- new.fips

    df <- rbind(ndf, df)

    df$fips <- lfactor(df$fips, fac.lvls, fac.lbls)

    return(df)
}

loadSchMap <- function(xls.path, sheet.name) {
  # Load Excel file (sheet.name typically in "M4", "M8", "R4", "R8")
  df <-
    as.data.frame(read_excel(xls.path, sheet = sheet.name))
  
  # Remove single-quotes from NCESSCH strings
  for (col in colnames(df)) {
    df[, col] <- gsub("'", "", as.character(df[[col]]))
  }
  # Rename columns
  colnames(df) <- c('ncessch', 'ncessch_new')
  
  return(df)
}

remapNcessch <- function(naep_df, remap_df) {
  for (schid in remap_df$ncessch) {
    naep_df[naep_df$ncessch == schid, 'ncessch'] <-
      as.character(remap_df[remap_df$ncessch == schid, ncol(remap_df)])
  }
  
  return(naep_df)
  
}