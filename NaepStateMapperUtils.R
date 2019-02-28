# Helper Functions
library(EdSurvey)
library(haven)
source("NaepStateMap.R")

loadSchMap <- function(xls.path, sheet.name) {
  # Load Excel file (sheet.name typically in "M4", "M8", "R4", "R8")
  df <-
    read_excel(xls.path, sheet = sheet.name)
  
  # Remove single-quotes from NCESSCH strings
  for (col in colnames(df)) {
    df[, col] <- gsub("'", "", as.character(df[[col]]))
  }
  # Rename columns
  colnames(df) <- c('ncessch', 'ncessch_new')
  
  return(df)
}


computeStateMapping <- function(naep.data, state.data) {
  # Make sure state data are single subject & grade
  if (NROW(unique(state.data[, c('subj', 'grade')])) > 1) {
    stop("State data contains more than one subj/grade combination.")
  }
  
  # Init output object
  nsm <- NaepStateMap(naep.data, state.data)
  naep.data <- nsm$naep.data
  state.data <- nsm$state.data
  
  # Get PV column names and number of PVs
  pv.colnames <-
    colnames(naep.data)[substr(colnames(naep.data), 2, 5) == "rpcm"]
  num.pvs <- NROW(pv.colnames)
  
  # Get JK rep weights and number of reps
  jk.colnames <-
    colnames(naep.data)[substr(colnames(naep.data), 1, 4) == "srwt"]
  num.jkreps <- NROW(jk.colnames)
  
  pvcut.cols <- paste0("cut", seq(num.pvs))
  jkcut.cols <- paste0("cut", seq(num.jkreps))
  
  # Get results for each state
  for (st in unique(nsm$naep.data$fips)) {
    if (DEBUG == TRUE) {
      print(
        paste(
          "=========================== Calculating scores for state:",
          st,
          "==========================="
        )
      )
      cat("PV: ")
    }
    
    # State-specific row index for output dataframes
    curr.row <- nsm$output.data$fips == st
    
    # State-specific datasets
    df_naep <- nsm$naep.data[nsm$naep.data$fips == st, ]
    df_state <-
      nsm$state.data[nsm$state.data$ncessch %in% df_naep$ncessch, ]
    
    # COMPUTE POINT ESTIMATES
    
    # Get pct proficient
    pctprof.base <- getPctProf(df_naep, df_state)
    
    # Get cut scores for pct prof for each PV & save to pvcuts table
    for (pvnum in seq(num.pvs)) {
      if (DEBUG == TRUE) {
        cat(paste0(pvnum, ".."))
      }
      nsm$pvcuts.data[curr.row, paste0("cut", pvnum)] <-
        getCutScore(pctprof.base,
                    df_naep,
                    weight.name = "origwt",
                    pv = pvnum)
    }
    if (DEBUG == TRUE) {
      cat("Done\n")
    }
    
    # Mean of individual PV cut scores
    cut.base <-
      mean(as.numeric(nsm$pvcuts.data[curr.row, pvcut.cols]))
    
    # Save pct prof and mean cut score to main output
    nsm$output.data$pctprof[curr.row] <- pctprof.base
    nsm$output.data$cut[curr.row] <- cut.base
    
    # COMPUTE STANDARD ERRORS
    if (DEBUG == TRUE) {
      cat("JKREP: ")
    }
    # Save imputation variance estimate to output
    nsm$output.data$var.impute[curr.row] <-
      sum((nsm$pvcuts.data[curr.row, pvcut.cols] - nsm$output.data[curr.row, "cut"]) ^ 2) * (num.pvs +
                                                                                               1) / (num.pvs * (num.pvs - 1))
    
    
    for (jknum in seq(num.jkreps)) {
      if (DEBUG == TRUE) {
        cat(paste0(jknum, ".."))
      }
      pctprof.jk <-
        getPctProf(df_naep, df_state, weight.name = jk.colnames[jknum])
      
      cut.jk <-
        getCutScore(pctprof.jk, df_naep, weight.name = jk.colnames[jknum])
      
      nsm$jkpcts.data[curr.row, paste0("pctprof", jknum)] <-
        pctprof.jk
      nsm$jkcuts.data[curr.row, paste0("cut", jknum)] <- cut.jk
      
    }
    if (DEBUG == TRUE) {
      cat("Done\n")
    }
    nsm$output.data$var.sample[curr.row] <-
      sum(as.numeric(nsm$jkcuts.data[curr.row, jkcut.cols] - nsm$pvcuts.data$cut1[curr.row]) ^ 2)
    nsm$output.data$se[curr.row] <-
      sqrt(nsm$output.data$var.impute[curr.row] + nsm$output.data$var.sample[curr.row])
  }
  
  return(nsm)
  
}


loadNaepData <- function(naep.path, ncessch.map = NULL) {
  naep.input <- readNAEP(naep.path)
  
  naep.vars <- c("ncessch",
                 "fips",
                 "origwt")
  score.vars <- searchSDF("rpcm", naep.input)$variableName
  jkwt.vars <- searchSDF("srwt", naep.input)$variableName
  
  df <- getData(naep.input, c(naep.vars, score.vars, jkwt.vars))
  colnames(df) <- tolower(colnames(df))
  df$stateabbr <- state.abb[match(df$fips, state.name)]
  
  df <- df[order(df$ncessch, df[score.vars[1]]), ]
  rownames(df) <- NULL
  
  return(df)
}

loadStateData <- function(state_dir) {
  state_codes <- c(state.abb, "DC", "PR")
  
  df <-
    data.frame(
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
    
    df.file <- read_sas(path_State)
    colnames(df.file) <- tolower(colnames(df.file))
    
    # For 2015 (and earlier?): Get rows for "Total" group only
    if ("group" %in% colnames(df.file)) {
      df.file <- df.file[df.file$group == 0,]
    }
    
    df.file <- df.file[, c("ncessch", "subj", "grade", "nt", "n3")]
    
    df <- rbind(df, df.file)
    
  }
  
  return(df)
}


getSchoolWeights = function(naep.data, weight.name = "origwt") {
  df <-
    naep.data[, c("ncessch", weight.name)]
  
  agg.rows <-
    aggregate(df[, weight.name],
              by = list(df$ncessch),
              FUN = sum)
  
  colnames(agg.rows) <- c("ncessch", "weight")
  
  return(agg.rows)
}

getPctProf = function(naep.data, state.data, weight.name = "origwt") {
  df.weights <- getSchoolWeights(naep.data, weight.name)
  
  df <-
    merge(state.data[, c("ncessch", "nt", "n3")], df.weights, by = "ncessch")
  # df <- df[df$nt > 0, ]
  
  df$pctprof <- df$n3 / df$nt
  return(weighted.mean(df$pctprof, df$weight))
}

getCutScore = function(pct.prof,
                       naep.data,
                       weight.name = "origwt",
                       pv = 1) {
  pv.colname <-
    colnames(naep_df)[substring(colnames(naep_df), 2) == paste0("rpcm", pv)]
  
  return(getInvCDF(1 - pct.prof, getCDFTable(naep.data[, pv.colname], naep.data[, weight.name])))
}

getCDFTable <- function(scores, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, NROW(scores))
  }
  
  df <- as.data.frame(cbind(scores, weights))
  colnames(df) <- c("scores", "weights")
  
  # Remove rows with 0 weights
  df <- df[df$weights > 0, ]
  
  cdf_table <-
    aggregate(df$weights, by = list(df$scores), FUN = sum)
  colnames(cdf_table) <- c("score", "weight")
  
  tot_wt <- sum(cdf_table$weight)
  cdf_table['pmf'] <- cdf_table$weight / tot_wt
  cdf_table['cdf'] <- cumsum(cdf_table$pmf)
  
  return(cdf_table)
}

getInvCDF <- function(p, cdf_table) {
  row_lo <-
    unique(cdf_table[cdf_table$cdf == max(cdf_table[cdf_table$cdf <= p, 'cdf']), c('score', 'cdf')])
  row_hi <-
    unique(cdf_table[cdf_table$cdf == min(cdf_table[cdf_table$cdf >= p, 'cdf']), c('score', 'cdf')])
  
  # print(paste("row_lo:", row_lo$score, "| row_hi:", row_hi$score))
  
  if (row_lo$score == row_hi$score) {
    return(row_lo$score)
  }
  
  a <- (p - row_lo$cdf) / (row_hi$cdf - row_lo$cdf)
  
  return((1 - a) * row_lo$score + a * row_hi$score)
}

remapNcessch <- function(naep_df, remap_df) {
  for (schid in remap_df$ncessch) {
    naep_df[naep_df$ncessch == schid, 'ncessch'] <-
      as.character(remap_df[remap_df$ncessch == schid, ncol(remap_df)])
  }
  
  return(naep_df)
  
}