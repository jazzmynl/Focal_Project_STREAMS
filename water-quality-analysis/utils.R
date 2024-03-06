SHOULD_RUN_ASSERTIONS = T

# Testing helper
assert = function(assertion, msg = msg) {
  if(SHOULD_RUN_ASSERTIONS) {
    assert_that(
      assertion,
      msg = msg
    )
  }
}

# analytes: list of analyte names
# replicates: dataframe with replicate ids in id_cols
filter_water_quality_by_analyte_and_replicate = function(
    analytes, 
    id_cols,
    replicates,
    water_quality
) { 
  
  inner_join(replicates, water_quality |> filter(analyte %in% analytes), by = id_cols)
}

# water_station_observations: each row is a station with a target time
# analytes
# - refined analytes, each station replicate has all analytes defined
# water_quality
# - assumptions: there is one result per analyte per station replicate
create_analyte_matrix_with_results = function(
    analytes, 
    water_station_observations,
    water_quality_for_replicates,
    id_cols
    ) {
  wide_water_quality = water_quality_for_replicates |> pivot_wider(
    id_cols = id_cols,
    names_from = analyte,
    values_from = result
    )
  
  start_col = length(id_cols)+1
  last_col = length(wide_water_quality)
  matrix = as.matrix(wide_water_quality[,start_col : last_col])
  
  rownames(matrix) = apply( 
    wide_water_quality[ , id_cols ], 
    1, 
    paste, 
    collapse = "-" 
    )
  
  matrix
}

select_analytes_and_replicates_by_reduce = function(
    optimized_water_quality
    ) {
  analytes_count = optimized_water_quality |>
    group_by(station_code) |> 
    count(analyte) 
  
  analytes_count_wide = analytes_count |>
    pivot_wider(
      id_cols = c(station_code),
      names_from = analyte,
      values_from = n
    )
  analytes_count_matrix = data.matrix(analytes_count_wide[,2:length(analytes_count_wide)])
  
  rownames(analytes_count_matrix) = analytes_count_wide$station_code
  
  analytes_count_matrix[is.na(analytes_count_matrix)] = 0
  analytes_count_matrix[analytes_count_matrix > 0] = 1
  
  # How many stations-timeframes have this analyte? 
  most_measured_analyte_num_stations = max(colSums(analytes_count_matrix))
  most_measured_station_num_analytes = max(rowSums(analytes_count_matrix))
  
  
  minNumStationsWithAnalyte = min(colSums(analytes_count_matrix))
  minNumAnalytesAtStation = min(rowSums(analytes_count_matrix)) 
  
  while(0 %in% analytes_count_matrix) {
    if ( minNumStationsWithAnalyte > 0 & 
         minNumAnalytesAtStation > 0 & minNumStationsWithAnalyte <= minNumAnalytesAtStation) {
      colsToKeep = colSums(analytes_count_matrix) != minNumStationsWithAnalyte
      
      print(paste('Dropping', length(colsToKeep[!colsToKeep]), 'of', length(colsToKeep), 'analytes:')) 
      print(names(colsToKeep)[!colsToKeep])
      
      analytes_count_matrix = analytes_count_matrix[,colsToKeep] 
    } else {
      rowsToKeep = rowSums(analytes_count_matrix) != minNumAnalytesAtStation
      
      print(paste('Dropping', length(rowsToKeep[!rowsToKeep]), 'of', length(rowsToKeep), 'stations:')) 
      print(names(rowsToKeep)[!rowsToKeep])
      
      analytes_count_matrix = analytes_count_matrix[rowsToKeep,]
    }
    
    
    minNumStationsWithAnalyte = min(colSums(analytes_count_matrix))
    minNumAnalytesAtStation = min(rowSums(analytes_count_matrix)) 
  }
  
  assert(
    sum(analytes_count_matrix != 1) == 0,
    msg = "Analytes count matrix should contain only 1s"
  )
  
  list("analytes" = colnames(analytes_count_matrix), "replicates" = data.frame("station_code" = rownames(analytes_count_matrix)))
}

library(jsonlite)