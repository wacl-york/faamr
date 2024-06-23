#' Get Core Date Origin
#' 
#' Given a path to a core ncdf file, returns the first timestamp as a nanotime
#' 
#' @param filepath path to file
#' 
#' @author W. S. Drysdale
#' 
#' @export

getCoreDateOrigin = function(filepath){
  
  dat_meta = ncmeta::nc_meta(filepath)
  
  dateOrigin = dat_meta$attribute |> 
    dplyr::filter(.data$variable == "Time",
                  .data$name == "units") |> 
    tidyr::unnest(.data$value) |> 
    purrr::pluck("value") |> 
    nanotime::nanotime(format = "seconds since %Y-%m-%d %H:%M:%S %z")
  
  # 
  dateOrigin
}

