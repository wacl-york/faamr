read_flight_summary = function(filepath){
  
  if(!stringr::str_ends(filepath, ".txt|.csv|.html")){
    stop(cli::format_error("read_flight_summary() only supports .txt, .csv and .html versions of the flight summary"))
  }
  
  if(stringr::str_ends(filepath, ".txt")){
    
    flightSum = read.fwf(filepath,
                          widths = c(8,9,20,18,4,100),
                          skip = 9,
                          colClasses = "character"
    ) |> 
      rlang::set_names(c("start_time","end_time","event","height","hdg","comments")) |> 
      tibble::tibble()
    
    date = as.POSIXct(readLines(filepath,n = 3)[3], format = "Date: %d/%m/%Y", tz = "UTC")
  }
  
  if(stringr::str_ends(filepath, ".csv")){
    flightSum = read.csv(filepath) |> 
  #    rlang::set_names(c("start_time","end_time","event","height","hdg","comments")) |> 
      tibble::tibble()
    
    
  }
  readLines(filepath)
  
  
  
}
