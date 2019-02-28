source("NaepStateMapperUtils.R")

SUBJECT <- "m"

NaepStateMapper <- setRefClass(
  "NaepStateMapper",
  fields = list(
    naep.data = "data.frame",
    state.data = "data.frame",
    pvpcts.data = "data.frame",
    pvcuts.data = "data.frame",
    jkpcts.data = "data.frame",
    jkcuts.data = "data.frame",
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
    
    getSchoolWeights = function(ncessch.list, weight.name = "origwt") {
      df <-
        naep.data[naep.data$ncessch %in% ncessch.list, c("ncessch", weight.name)]
      
      # colnames(naep.rows)[colnames(naep.rows) == weight.name] <- "wtvar"
      
      agg.rows <-
        aggregate(df[, weight.name],
                  by = list(df$ncessch),
                  FUN = sum)
      
      colnames(agg.rows) <- c("ncessch", "weight")
      
      return(agg.rows)
    },
    
    getPctProf = function(ncessch.list, weight.name = "origwt") {
      df.weights <- getSchoolWeights(ncessch.list, weight.name)
      
      df <-
        merge(state.data[, c("ncessch", "nt", "n3")], df.weights, by = "ncessch")
      
      df$pctprof <- df$n3 / df$nt
      return(weighted.mean(df$pctprof, df$weight))
    },
    
    getCutScore = function(pct.prof,
                           ncessch.list,
                           weight.name = "origwt",
                           pv = 1) {
      pv.colname <- paste0(SUBJECT, "rpcm", pv)
      
      row.filter <- naep.data$ncessch %in% ncessch.list
      
      return(getInvCDF(1 - pct.prof, getCDFTable(naep.data[row.filter, pv.colname], naep.data[row.filter, weight.name])))
    },
    
    getResults = function(ncessch.list) {
      
      initOutputData()
      
      pv.colnames <-
        colnames(naep.data)[substr(colnames(naep.data), 1, 5) == "mrpcm"]
      num.pvs <- NROW(pv.colnames)
      
      jk.colnames <-
        colnames(naep.data)[substr(colnames(naep.data), 1, 4) == "srwt"]
      num.jkreps <- NROW(jk.colnames)
      
      for (st in output.data$fips) {
        school.list <- naep.data$ncessch[naep.data$fips == st]
        
        pctprof.base <- getPctProf(school.list, "origwt")
        
        for (pvnum in 1:num.pvs) {
          pvcuts.data[pvcuts.data$fips == st, paste0("cut", pvnum)] <<-
            getCutScore(pctprof.base,
                        school.list,
                        weight.name = "origwt",
                        pv = pvnum)
        }
        
        cut.base <-
          as.numeric(rowMeans(pvcuts.data[pvcuts.data$fips == st, paste0("cut", 1:num.pvs)]))
        
        output.data$pctprof[output.data$fips == st] <<- pctprof.base
        output.data$cut[output.data$fips == st] <<- cut.base
        
        
        
        for (jkwt in jk.colnames) {
          jk.pctprof <- getPctProf(school.list, jkwt)
          jk.cut <-
            getCutScore(jk.pctprof, school.list, weight.name = jkwt)
          
          jkpcts.data[jkpcts.data$fips == st, paste0("pctprof_", jkwt)] <<-
            jk.pctprof
          jkcuts.data[jkcuts.data$fips == st, paste0("cut1_", jkwt)] <<-
            jk.cut
          
        }
        
        var.impute <-
          rowSums((pvcuts.data[pvcuts.data$fips == st, paste0("cut", 1:num.pvs)] -
                     cut.base) ^ 2) * (num.pvs + 1) / (num.pvs * (num.pvs - 1))
        var.sample <-
          rowSums((jkcuts.data[jkcuts.data$fips == st , paste0("cut1_", jk.colnames)] -
                     pvcuts.data[pvcuts.data$fips == st, "cut1"]) ^ 2)
        
        output.data$var.impute[output.data$fips==st] <<- var.impute
        output.data$var.sample[output.data$fips==st] <<- var.sample
        output.data$stderr[output.data$fips==st] <<- sqrt(var.impute + var.sample)
        
      }
    },
    
    computePctProf = function(fips.list = NULL,
                              weight.name = "origwt") {
      if (is.null(fips.list)) {
        fips.list <- unique(naep.data$fips)
      }
      
      df <- as.data.frame(fips.list)
      colnames(df) <- "fips"
      
      for (st in fips.list) {
        df$pctprof[df$fips == st] <- getPctProf(st, weight.name)
      }
      
      return(df)
    },
    
    computeCutScores = function(fips.list = NULL,
                                weight.name = "origwt") {
      output.data <<-
        merge(
          output.data,
          computePctProf(fips.list, weight.name),
          by = "fips",
          all.x = TRUE
        )
      
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
    
    computeStdErrs = function(fips.list = NULL) {
      pv_cols <- paste0('cut', 1:20) # REMOVE HARDCODING AT SOME POINT
      output.data$var.impute <<-
        rowSums((output.data[, cols] - output.data[, "cut.mean"]) ^ 2) *
        21 / (20 * 19)
      
      srwt_cols <-
        paste0('srwt', 1:62) # REMOVE HARDCODING AT SOME POINT
      
      df_jkreps <- as.data.frame(unique(naep.data$fips))
      colnames(df_jkreps) <- "fips"
      df_jkreps <-
        merge(df_jkreps, output.data[, c("fips", "cut1")], by = "fips", all.x =
                TRUE)
      
      for (colname in srwt_cols) {
        df_pctprof <- computePctProf(weight.name = colname)
        
        for (st in unique(df_pctprof$fips[!is.na(df_pctprof$pctprof)])) {
          # print(paste("State: ", st, "| Weight:", colname, df_pctprof$pctprof[df_pctprof$fips == st]))
          ks <-
            getCutScore(df_pctprof$pctprof[df_pctprof$fips == st], st, weight.name =
                          colname)
          
          df_pctprof[df_pctprof$fips == st, "cut"] <-
            getCutScore(df_pctprof$pctprof[df_pctprof$fips == st], st, weight.name =
                          colname)
        }
        
        colnames(df_pctprof) <-
          c("fips",
            paste0("pctprof_", colname),
            paste0("cut1_", colname))
        
        df_jkreps <-
          merge(df_jkreps, df_pctprof, by = "fips", all.x = TRUE)
      }
      
      df_jkreps["var.jkrep"] <-
        rowSums((df_jkreps[, paste0("cut1_", srwt_cols)] - df_jkreps$cut1) ^ 2)
      
      output.data <<-
        merge(output.data, df_jkreps, by = "fips", all.x = TRUE)
    },
    
    initOutputData = function() {
      naep.data <<-
        merge(naep.data, state.data[, "ncessch"], by =
                "ncessch")
      
      output.data <<-
        as.data.frame(unique(naep.data[, c("fips", "stateabbr")]))
      output.data <<-
        output.data[order(output.data$fips),]
      rownames(output.data) <<- NULL
      
      # NOTE: Assigns copy, not reference
      pvcuts.data <<- output.data
      jkpcts.data <<- output.data
      jkcuts.data <<- output.data
    }
  )
)


NaepStateMap <- setRefClass(
  "NaepStateMap",
  fields = list(
    naep.data = "data.frame",
    state.data = "data.frame",
    pvpcts.data = "data.frame",
    pvcuts.data = "data.frame",
    jkpcts.data = "data.frame",
    jkcuts.data = "data.frame",
    output.data = "data.frame"
  ),
  
  methods = list(
    initialize = function(naep.data,
                          state.data) {
      naep.data <<- naep.data
      state.data <<- state.data
      
      initOutputData()
      
    },
    
    initOutputData = function() {
      # Drop state rows with zero tested students
      state.data <<- state.data[state.data$nt > 0, ]
      naep.data <<-
        merge(naep.data, state.data[, "ncessch"], by =
                "ncessch")
      
      fips_list <- unique(naep.data$fips)
      fips_list <- fips_list[order(fips_list)]
      
      output.data <<- data.frame(fips_list)
      colnames(output.data) <<- "fips"

      # NOTE: Assigns copy, not reference
      pvcuts.data <<- output.data
      jkpcts.data <<- output.data
      jkcuts.data <<- output.data
    }
    
  )
)