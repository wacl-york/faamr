
ceda_url = function(){
  "https://data.ceda.ac.uk"
}

file_revision = function(name, flight_number){
  
  stringr::str_extract(name, stringr::regex(paste0("_r([0-9])_",flight_number))) |> 
    stringr::str_remove("_r") |> 
    stringr::str_remove(paste0("_", flight_number)) |> 
    as.numeric()
  
}

filter_revision = function(folder){
  folder |> 
    dplyr::mutate(r = file_revision(name, "c179"),
                  r = ifelse(is.na(r),0, r), 
                  nameNoR = stringr::str_remove(name, stringr::regex("_r([0-9])_")),
                  nameNoR = ifelse(is.na(nameNoR), name, nameNoR)) |> 
    dplyr::group_by(nameNoR) |> 
    dplyr::filter(r == max(r))
  
}


list_flights = function(force = F){
  
  if(!is.null(the$flightList) | force){
    return(the$flightList)
  }
  
  yearDirs = jsonlite::fromJSON(paste0(ceda_url(), "/badc/faam/data?json"))$items |> 
    tibble::as_tibble() |> 
    dplyr::filter(type ==  "dir")
  
  flightList = purrr::map_df(yearDirs$path, function(x) jsonlite::fromJSON(paste0(ceda_url(), x ,"?json"))$items |> 
                               dplyr::mutate(yr = basename(x))) |> 
    tibble::as_tibble() |> 
    dplyr::filter(type == "dir") |> 
    dplyr::mutate(flightNumber = stringr::word(name, 1, sep = "-"),
                  date = as.POSIXct(paste(yr, stringr::word(name[1], 2, 3, sep = "-")),format = "%Y %b-%d")
    ) |> 
    dplyr::select(path, name, date, flightNumber)
  
  the$flightList = flightList
  
  flightList
}


check_flight_data = function(flightFolder, print = TRUE){
  
  flightDataCheck = list(flightSum = list(name =  "Flight summary",
                                          pattern = "flight-sum",
                                          isDir = FALSE),
                         coreRaw = list(name = "Core raw",
                                        pattern = "core_raw",
                                        isDir = TRUE),
                         coreProcessed = list(name = "Core Processed",
                                              pattern = "core_processed",
                                              isDir = TRUE),
                         nonCore = list(name = "Non-core",
                                        pattern = "non-core",
                                        isDir = TRUE)) |> 
    lapply(function(x){
      
      x$check = sum(stringr::str_detect(flightFolder$name, x$pattern)) > 0
      
      if(x$check){
        
        if(x$isDir){
          x$dir = flightFolder$path[stringr::str_detect(flightFolder$name, x$pattern)]
          
          x$subFolder = jsonlite::fromJSON(paste0(ceda_url(), x$dir,"?json"))$items |> 
            tibble::as_tibble() 
          
        }
        
      }
      
      x
      
    })
  
  if(print){
    print_flight_data(flightDataCheck)
    invisible(flightDataCheck)
  }else{
    flightDataCheck
  }
  
}

print_flight_data = function(flightDataCheck){
  
  sink = lapply(flightDataCheck, function(x){
    
    if(x$check){
      cli::cli_alert_success(x$name)
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



list_flight_data = function(flight){
  
  fl = list_flights() |> 
    dplyr::filter(flightNumber %in% flight)
  
  if(nrow(fl) == 0){
    stop("No flights found")
  }
  
  if(nrow(fl) != length(flight)){
    
    missing = flight[!flight %in% fl$flightNumber]
    
    warning(cli::format_warning(paste("Unable to find {length(missing)} flight{?s}", paste0(missing, collapse = ", "))))
    
  }
  
  
  flightFolder = jsonlite::fromJSON(paste0(ceda_url(), fl$path[1],"?json"))$items |> 
    tibble::as_tibble() 
  
  check_flight_data(flightFolder)
  
}

