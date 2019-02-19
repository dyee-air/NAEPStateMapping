# Helper Functions
library(EdSurvey)
library(haven)

computeStateMapping <- function(naep.data, state.data) {
  if (NROW(unique(state.data[, c('subj', 'grade')])) > 1) {
    stop("State data contains more than one subj/grade combination.")
  }
  
  nsm <- NaepStateMap(naep.data, state.data)
  
  # Get PV column names and number of PVs
  pv.colnames <-
    colnames(naep.data)[substr(colnames(naep.data), 2, 5) == "rpcm"]
  num.pvs <- NROW(pv.colnames)
  
  # Get JK rep weights and number of reps
  jk.colnames <-
    colnames(naep.data)[substr(colnames(naep.data), 1, 4) == "srwt"]
  num.jkreps <- NROW(jk.colnames)
  
  for (st in unique(nsm$naep.data$fips)) {
    df_naep <- nsm$naep.data[nsm$naep.data$fips == st,]
    df_state <-
      nsm$state.data[nsm$state.data$ncessch %in% df_naep$ncessch,]
    
    pctprof.base <- getPctProf(df_naep, df_state)
    
    for (pvnum in 1:num.pvs) {
      nsm$pvcuts.data[nsm$pvcuts.data$fips == st, paste0("cut", pvnum)] <-
        getCutScore(pctprof.base,
                    df_naep,
                    weight.name = "origwt",
                    pv = pvnum)
    }
    
    cut.base <-
      as.numeric(rowMeans(nsm$pvcuts.data[nsm$pvcuts.data$fips == st, paste0("cut", 1:num.pvs)]))
    
    nsm$output.data$pctprof[nsm$output.data$fips == st] <-
      pctprof.base
    nsm$output.data$cut[nsm$output.data$fips == st] <- cut.base
    
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
  
  df <- df[order(df$ncessch, df$mrpcm1),]
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
      df.file <- df.file[df.file$group == 0, ]
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