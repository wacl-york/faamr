#' List Flights
#' 
#' Returns a data.frame containing the flight numner, date and location on CEDA 
#' of all FAAM flights stored there. 
#'
#' @param force For performance the flight list is cached in an environment variable \code{the$flightList}. Set true to force scraping CEDA.
#'
#' @export
#' @author W. S. Drysdale

list_flights = function(force = F){
  
  if(is.null(the$flightList) | force){
    
    yearDirs = jsonlite::fromJSON(paste0(ceda_url(), "/badc/faam/data?json"))$items |> 
      tibble::as_tibble() |> 
      dplyr::filter(.data$type ==  "dir")
    
    flightList = purrr::map_df(yearDirs$path, function(x) jsonlite::fromJSON(paste0(ceda_url(), x ,"?json"))$items |> 
                                 dplyr::mutate(yr = basename(x))) |> 
      tibble::as_tibble() |> 
      dplyr::filter(.data$type == "dir") |> 
      dplyr::mutate(flightNumber = stringr::word(.data$name, 1, sep = "-"),
                    date = as.POSIXct(paste(.data$yr, stringr::word(.data$name, 2, 3, sep = "-")),format = "%Y %b-%d")
      ) |> 
      dplyr::select(.data$path, .data$name, .data$date, .data$flightNumber)
    
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
#' @param force For performance previously queried flights are cached in an environment variable \code{the$ListofFlights}. Set true to force scraping CEDA.
#' 
#' @author W. S. Drysdale
#' 
#' @export


list_flight_data = function(flight, verbose = TRUE, force = FALSE){
  
  flight = tolower(flight)
  
  fl = list_flights() |> 
    dplyr::filter(.data$flightNumber %in% flight)
  
  if(nrow(fl) == 0){
    stop(cli::format_error("No flights found"))
  }
  
  if(nrow(fl) != length(flight)){
    
    missing = flight[!flight %in% fl$flightNumber]
    
    warning(cli::format_warning(paste("Unable to find {length(missing)} flight{?s}", paste0(missing, collapse = ", "))))
    
  }
  
  
  out = purrr::map_df(flight, 
                      ~faamr::check_flight_data(.x, verbose, force) |> 
                        tidy_flight_data_check() |> 
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


check_flight_data = function(flight, verbose = TRUE, force = FALSE){
  
  if(is.null(the$listOfFlights[[flight]]) | force){
    flight = tolower(flight)

    fl = faamr::list_flights() |> 
      dplyr::filter(.data$flightNumber %in% flight)
    
    flightFolder = jsonlite::fromJSON(paste0(ceda_url(), fl$path,"?json"))$items |> 
      tibble::as_tibble() 
    
    heading = flightFolder |> 
      dplyr::filter(.data$name == "00README") |> 
      purrr::pluck("content") |> 
      stringr::word(1, sep = "\\n") |> 
      stringr::str_trim()
    
    cli::cli_h1(heading)
    
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
            
            x$subFolder = jsonlite::fromJSON(paste0(ceda_url(), x$dir,"?json"))$items |> 
              tibble::as_tibble() |> 
              dplyr::mutate(subFolder = x$name)
            
          }else{
            x$fileName = flightFolder$name[stringr::str_detect(flightFolder$path, x$pattern) & stringr::str_detect(flightFolder$path, ".csv")]
          }
          
        }
        
        x
        
      })
    
    flightDataCheck$flightFolder = flightFolder
    the$listOfFlights[[flight]] = flightDataCheck
  }
  
  if(verbose){
    print_flight_data(the$listOfFlights[[flight]])
    invisible(the$listOfFlights[[flight]])
  }else{
    the$listOfFlights[[flight]]
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
  
  flightDataCheck = flightDataCheck[names(flightDataCheck) != "flightFolder"]
  
  sink = lapply(flightDataCheck, function(x){
    
    if(x$check){
      if(x$name == "Flight summary"){
        cli::cli_alert_success(paste0(x$name, 
                                      cli::col_grey(paste0(" - ", x$fileName))))
      }else{
        cli::cli_alert_success(x$name)
      }
      
      if(x$isDir){
        
        cli::cli_ul()
        
        subFolder = x$subFolder |> 
          filter_revision()
        
        
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
#' 
#' @param flightDataCheck output of \code{check_flight_data()}
#' 
#' @author W. S. Drysdale

tidy_flight_data_check = function(flightDataCheck){
  purrr::map_df(names(flightDataCheck), 
                ~purrr::pluck(flightDataCheck,.x,"subFolder")) |> 
    dplyr::bind_rows(flightDataCheck$flightFolder) |> 
    dplyr::select(tidyselect::all_of(c("path", "name", "type", "subFolder","ext"))) |> 
    filter_revision()
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
#' 
#' @param name file name to extract revision from
#' 
#' @author W. S. Drysdale

file_revision = function(name){
  
  stringr::str_extract(name, stringr::regex(paste0("_r([0-9])_"))) |> 
    stringr::str_remove("_r") |> 
    stringr::str_remove("_") |> 
    as.numeric()
  
}

#' Filter Revision
#' 
#' given a flight folder data.frame, return the only the most recently revised versions of each file
#' 
#' @param folder tibble representing the folder on CEDA
#' 
#' @author W. S. Drysdale

filter_revision = function(folder){
  
  folder |> 
    dplyr::mutate(r = file_revision(.data$name),
                  r = ifelse(is.na(.data$r),0, .data$r), 
                  nameNoR = stringr::str_remove(.data$name, stringr::regex("_r([0-9])_")),
                  nameNoR = ifelse(is.na(.data$nameNoR), .data$name, .data$nameNoR)) |> 
    dplyr::group_by(.data$nameNoR) |> 
    dplyr::filter(.data$r == max(.data$r)) |> 
    dplyr::ungroup() |> 
    dplyr::select(-.data$nameNoR)
  
}

#' Assign file Type
#' 
#' Assigns known file types from \code{file_lookup()} to files listed in a flight folder
#' such as that returned by \code{tidy_flight_data_check()}.
#' 
#' @param flightFolder data.frame describing a flight folder such as that returned by \code{tidy_flight_data_check()}.
#' @param lookup defaults to the internal list of files described by \code{faam_file_lookup()}, but an edited 
#' table can be provided if the user wishes
#' 
#' @export
#' 
#' @author W. S. Drysdale

assign_file_type = function(flightFolder, lookup = NULL){
  
  if(is.null(lookup)){
    lookup = faam_file_lookup()
  }
  
  flightFolder$fileType = purrr::map_chr(flightFolder$name, ~{
    y = lookup$fileType[stringr::str_detect(.x, lookup$fileRegex)]
    if(length(y) == 0){
      NA
    }else{
      y
    }
  })
  
  flightFolder
}