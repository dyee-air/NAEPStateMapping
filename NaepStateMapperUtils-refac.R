# Helper Functions
library(EdSurvey)
library(haven)
source("NaepStateMap.R")

computeStateMapping <- function(naep.data, state.data, label = "", DEBUG=FALSE) {
  
  # Init output object and data
  nsm <- NaepStateMap(naep.data, state.data, label=label, DEBUG=DEBUG)
  
  # Get common rows for computation
  state.df <- merge(state.data, unique(naep.data[, "ncessch", drop=FALSE]), by="ncessch")
  naep.df <- merge(naep.data, state.data[, "ncessch", drop=FALSE], by="ncessch")
  
  if (NROW(state.df[state.df$nt > 0, ])==0 | NROW(naep.df)==0) {
    if (label != "") {
      lab.txt <- paste(" for state:", label)
    }
    warning(paste0("No rows found", lab.txt, ".  Skipping"))
    return()
  }
  
  nsm$results$n.state.sch <- NROW(state.df)
  nsm$results$n.naep.sch <- NROW(unique(naep.df$ncessch))
  
  # Remove state rows with zero tested students
  state.df <- state.df[state.df$nt > 0, ]

  if (DEBUG == TRUE) {
    if (label == "") {
      print(
        paste(
          "=========================== Calculating scores: ==========================="
        )
      )
      
    } else {
      print(
        paste(
          "=========================== Calculating scores for state:",
          label,
          "==========================="
        )
      )
      
    }
    cat("PV: ")
  }
  
  # COMPUTE POINT ESTIMATES
  
  # Get pct proficient
  pctprof.base <- getPctProf(naep.df, state.df)
  
  # Get cut scores for pct prof for each PV & save to pvcuts table
  for (pvnum in seq(nsm$pv.count)) {
    if (DEBUG == TRUE) {
      cat(paste0(pvnum, ".."))
    }
    nsm$pvcuts.data[pvnum, 1] <-
      getCutScore(pctprof.base,
                  naep.df,
                  weight.name = "origwt",
                  pv = pvnum)
  }
  if (DEBUG == TRUE) {
    cat("Done\n")
  }
  
  # Mean of individual PV cut scores
  cut.base <-
    mean(nsm$pvcuts.data[, 1])
  
  # Save pct prof and mean cut score to main output
  nsm$results$pct.prof[1] <- pctprof.base
  nsm$results$cut.score[1] <- cut.base
  
  
  # COMPUTE STANDARD ERRORS
  
  # Compute and save imputation variance
  nsm$results$var.impute[1] <- sum((nsm$pvcuts.data[, 1] - cut.base)^2) * (nsm$pv.count + 1) / (nsm$pv.count * (nsm$pv.count - 1))
  
  # Get rep weight cut/pct estimates
  if (DEBUG == TRUE) {
    cat("JKREP: ")
  }
  
  for (jknum in seq(nsm$jk.count)) {
    if (DEBUG == TRUE) {
      cat(paste0(jknum, ".."))
    }
    pctprof.jk <-
      getPctProf(naep.df, state.df, weight.name = nsm$jk.cols[jknum])
    
    cut.jk <-
      getCutScore(pctprof.jk, naep.df, weight.name = nsm$jk.cols[jknum])
    
    nsm$jkpcts.data[jknum, 1] <-
      pctprof.jk
    nsm$jkcuts.data[jknum, 1] <- cut.jk
    
  }
  if (DEBUG == TRUE) {
    cat("Done\n")
  }
  
  # Compute and save sampling variance
  nsm$results$var.sample[1] <- sum((nsm$jkcuts.data[, 1] - nsm$pvcuts.data[1, 1])^2)
  
  nsm$results$cut.se[1] <- sqrt(nsm$var.impute + nsm$var.sample)
  
  # Compute frequencies
  nsm$results$n.naep <- NROW(naep.df)
  nsm$results$n.state.total <- sum(nsm$state.data$nt)
  nsm$results$n.state.prof <- sum(nsm$state.data$n3)
  # nsm$results$n.naep.sch <- NROW(unique(nsm$naep.data$ncessch))
  # nsm$results$n.state.sch <- NROW(unique(state.df$ncessch))
  
  nsm$cleanup()
  
  return(nsm)
  
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