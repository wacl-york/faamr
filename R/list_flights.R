#' List Flights
#' 
#' Returns a data.frame containing the flight numner, date and location on CEDA 
#' of all FAAM flights stored there. 
#'
#' @param force For performance the flight list is cached in an environment variable \code{the$flightList}. Set true to force scraping CEDA.
#'
#' @export
#'
#' @author W. S. Drysdale

list_flights = function(force = F){
  
  if(is.null(the$flightList) | force){
    
    yearDirs = jsonlite::fromJSON(paste0(ceda_url(), "/badc/faam/data?json"))$items |> 
      tibble::as_tibble() |> 
      dplyr::filter(type ==  "dir")
    
    flightList = purrr::map_df(yearDirs$path, function(x) jsonlite::fromJSON(paste0(ceda_url(), x ,"?json"))$items |> 
                                 dplyr::mutate(yr = basename(x))) |> 
      tibble::as_tibble() |> 
      dplyr::filter(type == "dir") |> 
      dplyr::mutate(flightNumber = stringr::word(name, 1, sep = "-"),
                    date = as.POSIXct(paste(yr, stringr::word(name, 2, 3, sep = "-")),format = "%Y %b-%d")
      ) |> 
      dplyr::select(path, name, date, flightNumber)
    
    the$flightList = flightList
    
    return(flightList)
  }else{
    return(the$flightList)
  }
  
}