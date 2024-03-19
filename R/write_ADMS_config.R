#' Write ADMS Config
#' 
#' Writes an ADMS config file from a nested list. Level 1 are the headers preceeded 
#' level 2 are the names of the key value pairs. Adapted from a python version 
#' found \href{https://github.com/irene146/adms6/blob/main/modify\%20_apl_files/modelconfig.py}{here}.
#' 
#' Best used after editing a working ADMS config file that has been loaded using \code{read_ADMS_config()} 
#' 
#' @param path path to ADMS config file
#' 
#' @export

write_ADMS_config = function(config, path){
  
  lines = c()
  
  for(i in 1:length(config)){
    
    section = config[[i]]
    
    headerName = paste0("&",names(config)[i])
    if(stringr::str_detect(headerName,"ADMS_POLLUTANT_DETAILS")){
      headerName = "&ADMS_POLLUTANT_DETAILS"
    }
    
    lines = append(lines, headerName)
    
    for(j in 1:length(section)){
      
      keyName = names(section)[j]
      value = section[[j]]
      for(k in 1:length(value))
        if(k == 1){ # key with single value or first value of key
          
          keyValue = paste(keyName, "=", value[k])
          lines = append(lines, keyValue)
        
      }else{
        lines = append(lines, value[k])
      }
    }
    
    lines = append(lines, "/")
    
  }
  
  writeLines(lines, path)
  
}
