#' Get Core Date Origin
#' 
#' Given a path to a core ncdf file, returns the first timestamp as a nanotime
#' 
#' @param timeName default "Time" - what is the time column called in the core netcdf we are trying to get the date from? e.g in nitrates its "time"
#' 
#' @inheritParams read_faam_core
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_core_date_origin = function(filepath, timeName = "Time"){
  
  dat_meta = ncmeta::nc_meta(filepath)
    
  dateOrigin = dat_meta$attribute |> 
    dplyr::filter(.data$variable == timeName,
                  .data$name == "units") |> 
    tidyr::unnest(.data$value) |> 
    purrr::pluck("value") |> 
    nanotime::nanotime(format = "seconds since %Y-%m-%d %H:%M:%S %z")
  
  # 
  dateOrigin
}

