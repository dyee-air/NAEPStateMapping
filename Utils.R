# Helper Functions
library(EdSurvey)
library(haven)


createNaepData <- function(naep_path, ncessch_map = NULL) {
  naep_input <- readNAEP(naep_path)
  
  naep_vars <- c("ncessch",
                 "fips",
                 "origwt")
  score_vars <- searchSDF("rpcm", naep_input)$variableName
  jkwt_vars <- searchSDF("srwt", naep_input)$variableName
  
  df <- getData(naep_input, c(naep_vars, score_vars, jkwt_vars))
  df$StateAbbr <- state.abb[match(df$fips, state.name)]
  
  if (!is.null(ncessch_map)) {
    for (sch_id in unique(ncessch_map$ncessch_old)) {
      df$ncessch[df$ncessch == sch_id] <-
        ncessch_map$ncessch_new[ncessch_map$ncessch_old == sch_id]
    }
  }
  
  return(df)
}

createStateData <- function(state_path, custom_data = NULL) {
  # NOTES:
  #   - Allow for multiple filetypes for import - or, ideally,
  #     externalize read function
  subject <- "M"
  grade <- 4
  group <- 0
  
  state_data <- read_sas(state_path)
  colnames(state_data) <- tolower(colnames(state_data))
  
  df <-
    state_data[state_data$subject == subject &
                 state_data$grade == grade &
                 state_data$group == group, c("ncessch", "nt", "n3")]
  
  if (!is.null(custom_data)) {
    
    colnames(custom_data) <- tolower(colnames(custom_data))
    
    cdata <- custom_data[custom_data$subject == subject &
                           custom_data$grade == grade, c("ncessch", "nt", "n3")]
    
    for (r in seq(NROW(cdata))) {
      if (cdata$ncessch[r] %in% unique(df$ncessch)) {
        df[df$ncessch == cdata$ncessch[r], c("nt", "n3")] <-
          cdata[r, c("nt", "n3")]
      }
      else {
        df <- rbind(df, cdata[r, c("ncessch", "nt", "n3")])
      }
    }
  }
  
  return(df)
}

getSchoolRows <- function(dataset, schids = NULL) {
  if (is.null(schids)) {
    schids <- unique(dataset$ncessch)
  }
  
  return(dataset[dataset$ncessch %in% schids, ])
}

getSchoolWeights = function(naep_rows) {
  agg_rows <-
    aggregate(naep_rows$origwt,
              by = list(naep_rows$ncessch),
              FUN = sum)
  colnames(agg_rows) <- c("ncessch", "weight")
  return(agg_rows)
}

NAEPScoreData <- setRefClass(
  "NAEPScoreData",
  fields = list(data = "data.frame"),
  methods = list(
    initialize = function(naep_path) {
      naep_file <- readNAEP(naep_path)
      naep_vars <- c("ncessch",
                     "fips",
                     "origwt")
      score_vars <- searchSDF("rpcm", naep_file)$variableName
      jkwt_vars <- searchSDF("srwt", naep_file)$variableName
      .self$data <-
        getData(naep_file, c(naep_vars, score_vars, jkwt_vars))
      
      .self$data$StateAbbr <-
        state.abb[match(.self$data$fips, state.name)]
      
    },
    
    getSchools = function(schids) {
      return(.self$data[.self$data$ncessch %in% schids,])
    },
    
    getSchoolWeights = function(schids) {
      sch_rows <- .self$getSchools(schids)
      sch_rows <-
        aggregate(sch_rows$origwt,
                  by = list(sch_rows$ncessch),
                  FUN = sum)
      colnames(sch_rows) <- c("ncessch", "weight")
      return(sch_rows)
    }
  )
)

StateProfData <- setRefClass(
  "StateProfData",
  fields = list(data = "data.frame"),
  methods = list(
    initialize = function(state_path,
                          subject = "M",
                          grade = 4,
                          group = 0) {
      state_data <- read_sas(state_path)
      colnames(state_data) <- tolower(colnames(state_data))
      
      state_data <-
        state_data[state_data$subject == subject &
                     state_data$grade == grade &
                     state_data$group == group, c("ncessch", "nt", "n3")]
      
      .self$data <- state_data
      .self$data$pctprof <- .self$data$n3 / .self$data$nt
    },
    
    getPctProf = function(schids, weights = NULL) {
      if (is.null(weights)) {
        weights <- rep(1, NROW(schids))
      }
      
      df <- .self$data[.self$data$ncessch %in% schids,]
      df$weight <- weights
      
      
    }
  )
)

getCDFTable <- function(scores, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, NROW(scores))
  }
  
  df <- as.data.frame(cbind(scores, weights))
  colnames(df) <- c("scores", "weights")
  
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
    cdf_table[cdf_table$cdf == max(cdf_table[cdf_table$cdf <= p, 'cdf']), c('score', 'cdf')]
  row_hi <-
    cdf_table[cdf_table$cdf == min(cdf_table[cdf_table$cdf >= p, 'cdf']), c('score', 'cdf')]
  
  if (row_lo$score == row_hi$score) {
    return(row_lo$score)
  }
  
  a <- (p - row_lo$cdf) / (row_hi$cdf - row_lo$cdf)
  
  return((1 - a) * row_lo$score + a * row_hi$score)
}


getSchId <- function(naep_schid) {
  if (naep_schid %in% sch_map$NCESSch_old) {
    return(sch_map[sch_map$NCESSch_old == naep_schid,]$NCESSch_new)
  }
  return(naep_schid)
}