#' Read Flight Summary
#' 
#' Reads the flight summary as a tibble. Only the .txt versions are currently supported
#' 
#' @param filepath path to flight summary
#' 
#' @author W. S Drysdale
#' 
#' @export

read_flight_summary = function(filepath){
  
  if(!stringr::str_ends(filepath, ".txt")){
    stop(cli::format_error("read_flight_summary() only supports .txt versions of the flight summary"))
  }
  
  # if statement in case future formats are added. 
  # .csv format has changed over time, compare c180 with c383
  if(stringr::str_ends(filepath, ".txt")){ 
    
    date = nanotime::as.nanotime(readLines(filepath,n = 3)[3], format = "Date: %d/%m/%Y", tz = "UTC")
    
    flightSum = utils::read.fwf(filepath,
                         widths = c(8,9,20,18,4,100),
                         skip = 9,
                         colClasses = "character"
    ) |> 
      rlang::set_names(c("start_time","end_time","event","height","hdg","comments")) |> 
      tibble::tibble() |> 
      dplyr::filter(!is.na(.data$start_time)) |>
      # dplyr::rowwise() |> 
      dplyr::mutate(
        dplyr::across(tidyselect::everything(), stringr::str_trim),
        start_time = nanotime::as.nanotime(.data$start_time, format = "%H%M%S"),
        dateStart = date + as.numeric(.data$start_time),
        end_time = ifelse(.data$end_time == "",NA, .data$end_time),
        end_time = nanotime::as.nanotime(.data$end_time, format = "%H%M%S"),
        dateEnd = date+as.numeric(.data$end_time)) |> 
      dplyr::select("dateStart", "dateEnd", "event", "height", "hdg", "comments")
  }
  
  flightSum
}
