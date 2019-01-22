# Helper Functions
library(EdSurvey)
library(haven)

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
  
  if (!is.null(ncessch.map)) {
    ncessch.map <- as.data.frame(ncessch.map)
    colnames(ncessch.map) <- c("ncessch.old", "ncessch.new")
    
    for (sch.id in unique(ncessch.map$ncessch.old)) {
      df$ncessch[df$ncessch == sch.id] <-
        ncessch.map$ncessch.new[ncessch.map$ncessch.old == sch.id]
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

getSchoolWeights = function(naep.rows, weight.name = "origwt") {
  colnames(naep.rows)[colnames(naep.rows) == weight.name] <- "wtvar"
  
  agg.rows <-
    aggregate(
      naep.rows$wtvar,
      by = list(naep.rows$ncessch, naep.rows$fips),
      FUN = sum
    )
  
  colnames(agg.rows) <- c("ncessch", "fips", "weight")
  
  return(agg.rows)
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
