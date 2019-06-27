NaepStateMap <- setRefClass(
  "NaepStateMap",
  fields = list(
    results = "data.frame",
    cut.score = function(x) {
      return(results$cut.score)
    },
    cut.se = function(x) {
      return(results$cut.se)
    },
    pct.prof = function(x) {
      return(results$pct.prof)
    },
    var.impute = function(x) {
      return(results$var.impute)
    },
    var.sample = function(x) {
      return(results$var.sample)
    },
    n.naep = function(x) {
      return(results$n.naep)
    },
    n.state.total = function(x) {
      return(results$n.state.total)
    },
    n.state.prof = function(x) {
      return(results$n.state.prof)
    },
    n.naep.sch = function(x) {
      return(results$n.naep.sch)
    },
    n.state.sch = function(x) {
      return(results$n.state.sch)
    },
    naep.data = "data.frame",
    state.data = "data.frame",
    label = "character",
    pvcuts.data = "data.frame",
    jkpcts.data = "data.frame",
    jkcuts.data = "data.frame",
    pv.cols = "character",
    pv.count = "numeric",
    jk.cols = "character",
    jk.count = "numeric",
    DEBUG = "logical"
  ),
  
  methods = list(
    initialize = function(naep.data,
                          state.data,
                          label = "",
                          DEBUG = FALSE) {
      naep.data <<- naep.data
      state.data <<- state.data
      label <<- label
      DEBUG <<- DEBUG
      
      initOutputData()
      
    },
    
    initOutputData = function() {
      # Get PV columns
      pv.cols <<-
        colnames(naep.data)[substr(colnames(naep.data), 2, 5) == "rpcm"]
      pv.count <<- length(pv.cols)
      if (pv.count == 0) {
        stop("No plausible values found in naep.data")
      }
      
      # Get rep weight columns
      jk.cols <<-
        colnames(naep.data)[substr(colnames(naep.data), 1, 4) == "srwt"]
      jk.count <<- length(jk.cols)
      if (jk.count == 0) {
        stop("No replicate weights found in naep.data")
      }
      
      # Make sure NAEP data includes required fields
      for (col in c("ncessch", "origwt")) {
        if (!(col %in% colnames(naep.data)))
          stop(paste0("Required column '", col, "' not found in naep.data"))
      }
      
      # Make sure state data includes required fields
      for (col in c("ncessch", "nt", "n3")) {
        if (!(col %in% colnames(state.data)))
          stop(paste0("Required column '", col, "' not found in naep.data"))
      }
      
      # Make sure state dataset has no duplicates
      if (NROW(unique(state.data$ncessch)) != NROW(state.data)) {
        stop("State dataset contains duplicate 'ncessch' values.")
      }
      
      # Drop state rows with zero tested students
      state.data <<- state.data[state.data$nt > 0, ]
      
      # Keep common rows for computation
      state.data <<- merge(state.data, unique(naep.data[, "ncessch", drop=FALSE]), by="ncessch")
      naep.data <<- merge(naep.data, state.data[, "ncessch", drop=FALSE], by="ncessch")
      
      # Set up results data
      result.fields <- c(
        "cut.score",
        "cut.se",
        "pct.prof",
        "var.impute",
        "var.sample",
        "n.naep",
        "n.state.total",
        "n.state.prof",
        "n.naep.sch",
        "n.state.sch"
      )

      
      flist <- list()
      for (f in result.fields) {
        flist[f] <- NA
      }
      
      if (label != "") {
        flist["label"] <- label
      }
      
      results <<-
        data.frame(flist)

      pvcuts.data <<- data.frame(cut.scores = rep(NA, pv.count))
      jkcuts.data <<- data.frame(cut.scores = rep(NA, jk.count))
      jkpcts.data <<- data.frame(pct.profs = rep(NA, jk.count))
      
    },
    
    cleanup = function() {
      if (!DEBUG) {
        field.names <- c(
          "naep.data",
          "state.data",
          "pvcuts.data",
          "jkcuts.data",
          "jkpcts.data")

        for (f in field.names) {
          .self[[f]] <- data.frame()
        }
      }
    }
    
  )
)