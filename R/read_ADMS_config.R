#' Read ADMS Config
#' 
#' Reads and ADMS config file into a nested list. Level 1 are the headers preceeded 
#' &, level 2 are the names of the key value pairs. Adapted from a python version 
#' found \href{https://github.com/irene146/adms6/blob/main/modify\%20_apl_files/modelconfig.py}{here}.
#' 
#' 
#' @param path path to ADMS config file
#' 
#' @export

read_ADMS_config = function(path){
  lines = readLines(path)
  
  config = list()
  
  for(i in 1:length(lines)){
    
    line = lines[i]
    
    if(stringr::str_starts(line, "&")){ # If line contains an ampersand, its a section header
      headerName = stringr::str_remove(line, "&")
      next
    }
    
    if(stringr::str_detect(line, "/") | line == "\n"){
      next
    }
    
    if(stringr::str_detect(line, "=")){ # if line contains an equals, it defines a key:value
      keyName = stringr::word(line, 1, sep = "=") |> 
        stringr::str_trim("both")
      
      value = stringr::word(line, 2, sep = "=") |> 
        stringr::str_trim("both")
      
      if(headerName == "ADMS_POLLUTANT_DETAILS"){
        headerName = paste0("ADMS_POLLUTANT_DETAILS_", stringr::str_remove_all(value,'"'))
      }
      
      config[[headerName]][[keyName]] = value
      
    }else{ # if we've got this far, the line doesn't contain an &, = or /, so its a multiline value, lets store this as a vector
      value = line |> 
        stringr::str_trim("both")
      
      config[[headerName]][[keyName]] = append(config[[headerName]][[keyName]], value)
      
    }
  }
  
  #
  config
  
}