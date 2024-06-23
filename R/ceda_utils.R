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
    
    yearDirs = jsonlite::fromJSON(paste0(faamr:::ceda_url(), "/badc/faam/data?json"))$items |> 
      tibble::as_tibble() |> 
      dplyr::filter(type ==  "dir")
    
    flightList = purrr::map_df(yearDirs$path, function(x) jsonlite::fromJSON(paste0(faamr:::ceda_url(), x ,"?json"))$items |> 
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

#' List Flight Data
#' 
#' Checks for the presence of the core_raw, core_processed, non_core and mo_no_core folders, and the flight summary file.
#' Then lists the files in the folders. Returns information as a data.frame and when verbose == TRUE, prints the information to the console. 
#' When verbose is TRUE, the data.frame is returned invisibly. 
#' 
#' @param flight a character vector of FAAM flight numbers. 
#' @param verbose Default true, should the available files be detailed in the console. When TRUE the data.frame is returned invisibly
#' 
#' @author W. S. Drysdale
#' 
#' @export


list_flight_data = function(flight, verbose = TRUE){
  
  flight = tolower(flight)
  
  fl = list_flights() |> 
    dplyr::filter(flightNumber %in% flight)
  
  if(nrow(fl) == 0){
    stop("No flights found")
  }
  
  if(nrow(fl) != length(flight)){
    
    missing = flight[!flight %in% fl$flightNumber]
    
    warning(cli::format_warning(paste("Unable to find {length(missing)} flight{?s}", paste0(missing, collapse = ", "))))
    
  }
  
  
  out = purrr::map_df(flight, 
                      ~faamr::check_flight_data(.x, verbose) |> 
                        faamr:::tidy_flight_data_check() |> 
                        dplyr::mutate(flightNumber = .x)
  )
  
  if(verbose){
    invisible(out)
  }else{
    out
  }
  
}

#' Check Flight Data
#' 
#' Performs the actual checking for \code{list_flight_data()} - Which is probably the function most will want to use when working interactively. 
#' This returns a nested list with some additional checks (\code{tidy_flight_data_check()}) is used to turn it into the data.frame output 
#' This is exported as it might have some programmatic use
#' 
#' @inheritParams list_flight_data
#' 
#' @author W. S. Drysdale
#' 
#' @export


check_flight_data = function(flight, verbose = TRUE){
  
  cli::cli_h1(flight)
  
  fl = faamr::list_flights() |> 
    dplyr::filter(flightNumber %in% flight)
  
  flightFolder = jsonlite::fromJSON(paste0(faamr:::ceda_url(), fl$path,"?json"))$items |> 
    tibble::as_tibble() 
  
  flightDataCheck = list(flightSum = list(name =  "Flight summary",
                                          pattern = "/flight-sum",
                                          isDir = FALSE),
                         coreRaw = list(name = "Core raw",
                                        pattern = "/core_raw",
                                        isDir = TRUE),
                         coreProcessed = list(name = "Core Processed",
                                              pattern = "/core_processed",
                                              isDir = TRUE),
                         nonCore = list(name = "Non-core",
                                        pattern = "/non-core",
                                        isDir = TRUE),
                         moNonCore = list(name = "Met Office Non-core",
                                        pattern = "/mo-non-core",
                                        isDir = TRUE)) |> 
    lapply(function(x){
      
      x$check = sum(stringr::str_detect(flightFolder$path,x$pattern)) > 0
      
      if(x$check){
        
        if(x$isDir){
          x$dir = flightFolder$path[stringr::str_detect(flightFolder$path, x$pattern)]
          
          x$subFolder = jsonlite::fromJSON(paste0(faamr:::ceda_url(), x$dir,"?json"))$items |> 
            tibble::as_tibble() |> 
            dplyr::mutate(subFolder = x$name)
          
        }
        
      }
      
      x
      
    })
  
  if(verbose){
    print_flight_data(flightDataCheck)
    invisible(flightDataCheck)
  }else{
    flightDataCheck
  }
  
}

#' Print flight data.
#' 
#' Formats the output of \code{check_flight_data()} in a pretty fashion in the console.
#' 
#' @param flightDataCheck output of \code{check_flight_data()}
#' 
#' @author W. S. Drysdale

print_flight_data = function(flightDataCheck){
  
  sink = lapply(flightDataCheck, function(x){
    
    if(x$check){
      cli::cli_alert_success(x$name)
      if(x$isDir){
        
        cli::cli_ul()
        
        subFolder = x$subFolder |> 
          faamr:::filter_revision()
        
        
        split(subFolder, 1:nrow(subFolder)) |> 
          lapply(function(y){
            
            switch(y$type, 
                   file = cli::cli_ul(paste(cli::col_green("file:"),y$name)),
                   dir = cli::cli_ul(paste(cli::col_yellow("dir:"),y$name)),
                   link = cli::cli_ul(paste(cli::col_blue("link:"),y$name)),
                   cli::cli_ul(paste(cli::col_grey("other:"),y$name))
            )
            
          })
        
        cli::cli_end()
      }
      
    }else{
      cli::cli_alert_danger(x$name)
    }
    
  })
  
}

#' Tidy Flight Data Check
#' 
#' Turns the the output of \code{check_flight_data()} into a data.frame

tidy_flight_data_check = function(flightDataCheck){
  purrr::map_df(names(flightDataCheck), 
                ~purrr::pluck(flightDataCheck,.x,"subFolder")) |> 
    dplyr::select(path, name, type, subFolder) |> 
    faamr:::filter_revision() 
}


#' CEDA URL
#' Returns the root URL for the CEDA data repository
#' 

ceda_url = function(){
  "https://data.ceda.ac.uk"
}

#' File Revision
#' 
#' Gets the file revision number from file name.

file_revision = function(name){
  
  stringr::str_extract(name, stringr::regex(paste0("_r([0-9])_"))) |> 
    stringr::str_remove("_r") |> 
    stringr::str_remove("_") |> 
    as.numeric()
  
}

#' Filter Revision
#' 
#' given a flight folder data.frame, return the only the most recently revised versions of each file

filter_revision = function(folder){
  folder |> 
    dplyr::mutate(r = faamr:::file_revision(name),
                  r = ifelse(is.na(r),0, r), 
                  nameNoR = stringr::str_remove(name, stringr::regex("_r([0-9])_")),
                  nameNoR = ifelse(is.na(nameNoR), name, nameNoR)) |> 
    dplyr::group_by(nameNoR) |> 
    dplyr::filter(r == max(r))
  
}