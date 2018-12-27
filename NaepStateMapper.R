source("NaepStateMapperUtils.R")

NaepStateMapper <- setRefClass(
  "NaepStateMapper",
  fields = list(
    naep.data = "data.frame",
    state.data = "data.frame",
    weight.data = "data.frame",
    output.data = "data.frame",
    verbose = "logical"
  ),
  
  methods = list(
    initialize = function(naep.data,
                          state.data = NULL,
                          verbose = FALSE) {
      verbose <<- verbose
      
      setNaepData(naep.data)
      
      if (!is.null(state.data)) {
        setStateData(state.data)
      }
      
    },
    
    setNaepData = function(input.data) {
      naep.data <<- as.data.frame(input.data)
      naep.data <<- naep.data[order(naep.data$ncessch), ]
      rownames(naep.data) <<- NULL
      weight.data <<- getSchoolWeights(naep.data)
      initOutputData()
    },
    
    setStateData = function(input.data) {
      state.data <<- input.data
    },
    
    addStateData = function(input.data) {
      print(paste0(
        "Appending state data (fips: ",
        paste(unique(substr(
          input.data$ncessch, 1, 2
        )), collapse = ', '),
        ")"
      ))
      state.data <<- rbind(state.data, input.data)
    },
    
    getSchoolRows = function(fips.list = NULL) {
      if (is.null(fips.list)) {
        fips.list <- unique(naep.data$fips)
      }
      
      return(naep.data[naep.data$fips %in% fips.list, ])
    },
    
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
    },
    
    getPctProf = function(fips.list, weight.name = "origwt") {
      df.weights <-
        getSchoolWeights(naep.data[naep.data$fips %in% fips.list, c("ncessch", "fips", weight.name)], weight.name)
      
      df <- merge(state.data, df.weights, by = "ncessch")
      df$pctprof <- df$n3 / df$nt
      return(weighted.mean(df$pctprof, df$weight))
    },
    
    getCutScore = function(pct.prof,
                           fips.list,
                           weight.name = "origwt",
                           pv = 1) {
      df <-
        merge(naep.data[naep.data$fips %in% fips.list, ], state.data, by = "ncessch")
      
      return(getInvCDF(1 - pct.prof, getCDFTable(df[, paste0("mrpcm", pv)], df[, weight.name])))
    },
    
    computePctProf = function(fips.list = NULL,
                              weight.name = "origwt") {
      if (is.null(fips.list)) {
        fips.list <- unique(naep.data$fips)
      }
      
      for (st in fips.list) {
        output.data[output.data$fips == st, "pctprof"] <<-
          getPctProf(st, weight.name)
      }
    },
    
    computeCutScores = function(fips.list=NULL, weight.name = "origwt") {
      computePctProf(fips.list, weight.name)
      
      for (st in unique(output.data$fips[!is.na(output.data$pctprof)])) {
        for (i in seq(20)) {
          outcolname <- paste0("cut", i)
          
          output.data[output.data$fips == st, outcolname] <<-
            getCutScore(output.data$pctprof[output.data$fips == st], st, weight.name, pv =
                          i)
        }
      }
      
      output.data$cut.mean <<- NULL
      output.data$cut.mean <<-
        rowMeans(output.data[, grep("cut", colnames(output.data))])
    },
    
    computeStdErrs = function() {
      return(1)
    },
    
    initOutputData = function() {
      output.data <<-
        as.data.frame(unique(naep.data[, c("fips", "stateabbr")]))
      output.data <<-
        output.data[order(output.data$fips), ]
      rownames(output.data) <<- NULL
    }
  )
)