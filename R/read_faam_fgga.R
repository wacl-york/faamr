#' Read FAAM fgga
#' 
#' Reads the nasa ames files produced for the FAAM Fast Greenhouse Gas Analyser
#' 
#' @param filepath path to file
#' 
#' @author W. S. Drysdale
#' 
#' @export


read_faam_fgga = function(filepath){
  fgga_header = read_nasa_ames_header(filepath)
  
  date_origin = as.POSIXct(fgga_header$long_names[1], format = "Time (seconds since %Y-%m-%d %H:%M:%S UTC)", tz = "UTC")
  
  fgga_header$long_names[1] = "date"
  
  fgga = nasaAmesR::read_nasa_ames_1001(filepath) |> 
    stats::setNames(fgga_header$long_names) |> 
    janitor::clean_names() |> 
    dplyr::mutate(date = date+date_origin |> 
                    nanotime::as.nanotime())
  
}