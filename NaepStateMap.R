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