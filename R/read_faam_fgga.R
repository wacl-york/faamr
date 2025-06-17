#' Read FAAM fgga
#' 
#' Reads the nasa ames files produced for the FAAM Fast Greenhouse Gas Analyser
#' 
#' @param applyFlagBeforeAverage If averageNanoString is supplied, should any data where flag != 0 be removed
#'                               default TRUE.
#' 
#' @inheritParams read_faam_core
#' 
#' @author W. S. Drysdale
#' 
#' @export


read_faam_fgga = function(filepath,
                          applyFlagBeforeAverage = TRUE,
                          averageNanoString = NULL){
  
  fggaHeader = read_nasa_ames_header(filepath)
  
  dateOrigin = as.POSIXct(fggaHeader$long_names[1], format = "Time (seconds since %Y-%m-%d %H:%M:%S UTC)", tz = "UTC")
  
  cols = c("date", "co2_value", "co2_flag", "ch4_value", "ch4_flag")
  
  fgga = nasaAmesR::read_nasa_ames_1001(filepath) |> 
    stats::setNames(cols) |> 
    janitor::clean_names() |> 
    dplyr::mutate(date = date+dateOrigin) |> 
    dplyr::mutate(date = nanotime::as.nanotime(date))
  
  if(!is.null(averageNanoString)){
    
    if(applyFlagBeforeAverage){
      
      fgga = fgga |> 
        tidyr::pivot_longer(-date,
                            names_sep = "_",
                            names_to = c("species","type")) |> 
        tidyr::pivot_wider(names_from = "type") |> 
        dplyr::filter(.data$flag ==  0) |> 
        tidyr::pivot_longer(c("value", "flag"), names_to = "type") |> 
        tidyr::pivot_wider(names_from = c("species", "type"), names_sep = "_")
      
    }

    fgga = fgga |> 
      dplyr::mutate(date = nanotime::nano_floor(.data$date, nanotime::as.nanoduration(averageNanoString))) |> 
      tidyr::pivot_longer(-date) |> 
      dplyr::group_by(.data$date, .data$name) |> 
      dplyr::summarise_all(mean, na.rm = T) |> 
      dplyr::ungroup() |>
      tidyr::pivot_wider() |> 
      dplyr::relocate(tidyselect::all_of(cols)) # use to preserve the order of the columns, otherwise the before and after column order is different
  }
  
  fgga |> 
    dplyr::rename("co2_drymole_ppm" = "co2_value",
                  "ch4_drymole_ppb" = "ch4_value")
  
}