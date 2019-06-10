# Helper Functions
source("NaepStateMap.R")

################################################################################
# Functions for state mapping results
################################################################################

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