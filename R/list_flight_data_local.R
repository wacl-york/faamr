#' List Flight Data Local
#' 
#' Returns a tibble summarising the files that have been downloaded, which is useful for further processing
#' 
#' @param dirContainingFlights path to the directory that contains the flight folders. 
#'        i.e. \code{list.dirs(dirContainingFlights)} should return a vector of valid flight numbers
#'        These will be checked against the flight list (from \code{list_flights()}), unless \code{skipFlightListCheck}
#'        is set to TRUE.
#' @param skipFlightListCheck skips checking the listed directories against \code{list_flights}. 
#'        Useful when working without a connection to CEDA but may cause unexpeceted errors if 
#'        directories not downloaded using \code{flight_download()} are present. 
#'        
#' @author W. S. Drysdale
#' 
#' @export


list_flight_data_local = function(dirContainingFlights, skipFlightListCheck = FALSE){
  
  dirs = tibble::tibble(
    dirPath = list.dirs(dirContainingFlights, recursive = F), 
    flightNumber = basename(.data$dirPath)
  )
  
  if(!skipFlightListCheck){
    fl = list_flights() 
    dirs = dirs |> 
      dplyr::filter(.data$flightNumber %in% fl$flightNumber)
  }
  
  dirs |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      "name" = list.files(.data$dirPath, recursive = T, full.names = T) |> 
        list()) |> 
    tidyr::unnest("name") |> 
    assign_file_type() |> 
    dplyr::rename("filePath" = "name") |> 
    dplyr::left_join(faam_file_lookup(), by = "fileType") |> 
    dplyr::select(-"dirPath", -"fileRegex")
  
  
}
