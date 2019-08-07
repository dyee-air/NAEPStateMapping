# Helper Functions
source("NaepStateMap.R")

################################################################################
# Functions for state mapping results
################################################################################

computeStateMapping <-
  function(naep.data,
           state.data,
           label = "",
           DEBUG = FALSE) {
    # Init output object and data
    nsm <-
      NaepStateMap(naep.data, state.data, label = label, DEBUG = DEBUG)
    
    # Check for empty dataset
    if (NROW(nsm$state.data[nsm$state.data$nt > 0,]) == 0 |
        NROW(nsm$naep.data) == 0) {
      if (label != "") {
        lab.txt <- paste(" for state:", label)
      }
      warning(paste0("No rows found", lab.txt, ".  Skipping."))
      return()
    }
    
    # TEMP save freqs
    nsm$results$n.state.sch <- NROW(nsm$state.data)
    nsm$results$n.naep.sch <- NROW(unique(nsm$naep.data$ncessch))
    
    
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
    }
    
    # COMPUTE POINT ESTIMATES
    
    point.est <- computePointEstimates(nsm$naep.data, nsm$state.data)
    
    # Save pct prof and mean cut score to main output
    nsm$results$pct.prof[1] <- point.est$pct.prof
    nsm$results$cut.score[1] <- point.est$cut.mean
    nsm$pvcuts.data[1] <- point.est$cut.scores
    
    
    # COMPUTE STANDARD ERRORS
    
    err.est <-
      computeStandardErrors(
        nsm$naep.data,
        nsm$state.data,
        cut.scores = point.est$cut.scores
      )
    
    nsm$results$cut.se[1] <- err.est$se
    nsm$results$var.impute[1] <- err.est$var.impute
    nsm$results$var.sample[1] <- err.est$var.sample
    nsm$jkpcts.data[, 1] <- err.est$jk.pcts
    nsm$jkcuts.data[, 1] <- err.est$jk.cuts
    
    nsm$cleanup()
    
    return(nsm)
    
  }


computePointEstimates <-
  function(naep.data, state.data, pct.prof.custom = NULL) {
    # pct.prof.custom: If NULL, compute pct prof from data; otherwise, use supplied value
    
    pct.prof <- pct.prof.custom
    
    if (is.null(pct.prof)) {
      naep.data <- merge(naep.data, state.data[, 'ncessch', drop=FALSE], by = 'ncessch')
      
      if (NROW(naep.data) == 0) {
        stop('computePointEstimates: No rows found.')
      }
      
      pct.prof <- getPctProf(naep.data, state.data)
      
    }
    
    pv.count <-
      length(colnames(naep.data)[substr(colnames(naep.data), 2, 5) == "rpcm"])
    
    cuts <-
      sapply(seq(pv.count), function(x)
        getCutScore(pct.prof, naep.data, pv = x))
    
    return(list(
      pct.prof = pct.prof,
      cut.mean = mean(cuts),
      cut.scores = cuts
    ))
  }

computeStandardErrors <-
  function(naep.data,
           state.data,
           cut.scores = NULL,
           pct.prof.custom = NULL) {
    jk.cols <-
      colnames(naep.data)[substr(colnames(naep.data), 1, 4) == "srwt"]
    
    pct.prof <- pct.prof.custom
    
    if (is.null(pct.prof)) {
      naep.data <- merge(naep.data, state.data[, 'ncessch', drop=FALSE], by = 'ncessch')
      
      if (NROW(naep.data) == 0) {
        stop('computeStandardErrors: No rows found.')
      }
      
      pct.prof <- getPctProf(naep.data, state.data)
      pct.profs.jk <-
        sapply(seq(length(jk.cols)), function(x)
          getPctProf(naep.data, state.data, weight.name = jk.cols[x]))
      
    } else {
      pct.profs.jk <- rep(pct.prof, length(jk.cols))
    }
    
    if (is.null(cut.scores)) {
      point.est <- computePointEstimates(naep.data, NULL, pct.prof)
      cut.scores <- point.est$cut.scores
    }
    
    
    var.impute <-
      sum((cut.scores - mean(cut.scores)) ^ 2) * (length(cut.scores) + 1) / (length(cut.scores) * (length(cut.scores) - 1))
    
    cut.scores.jk <-
      sapply(seq(length(jk.cols)), function(x)
        getCutScore(pct.profs.jk[x], naep.data, weight.name = jk.cols[x]))
    
    var.sample <- sum((cut.scores.jk - cut.scores[1]) ^ 2)
    
    se <- sqrt(var.impute + var.sample)
    
    return(
      list(
        se = se,
        var.impute = var.impute,
        var.sample = var.sample,
        jk.pcts = pct.profs.jk,
        jk.cuts = cut.scores.jk
      )
    )
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
  
  if (NROW(df) == 0) {
    warning('No rows found.  Returning pct.prof = 0.5')
    return(0.5)
  }
  
  df$pctprof <- df$n3 / df$nt
  return(weighted.mean(df$pctprof, df$weight))
}

getCutScore = function(pct.prof,
                       naep.data,
                       weight.name = "origwt",
                       pv = 1) {
  pv.colname <-
    colnames(naep.data)[substring(colnames(naep.data), 2) == paste0("rpcm", pv)]
  
  return(getInvCDF(1 - pct.prof, getCDFTable(naep.data[, pv.colname], naep.data[, weight.name])))
}

getCDFTable <- function(scores, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, NROW(scores))
  }
  
  df <- as.data.frame(cbind(scores, weights))
  colnames(df) <- c("scores", "weights")
  
  # Remove rows with 0 weights
  df <- df[df$weights > 0,]
  
  cdf_table <-
    aggregate(df$weights, by = list(df$scores), FUN = sum)
  colnames(cdf_table) <- c("score", "weight")
  
  tot_wt <- sum(cdf_table$weight)
  cdf_table['pmf'] <- cdf_table$weight / tot_wt
  cdf_table['cdf'] <- cumsum(cdf_table$pmf)
  
  return(cdf_table)
}

getInvCDF <- function(p, cdf_table) {
  if (NROW(cdf_table)==0) {
    warning("CDF table is empty.  Aborting.")
    return()
  }
  
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