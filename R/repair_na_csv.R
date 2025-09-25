#' Repair .na.csv
#' 
#' Some NASA Ames files are confused and are also .csvs. This tries to tidy them up
#' 
#' The following steps are performed
#' \enumerate{
#' \item The header length is found and lines that are greater than this have spaces replaced with "T" to fix some time stamps.
#'       This is changed to header length + 1 when NComm_header == TRUE
#' \item Lines where there are quotes (") are isolated and processed while trying to preserve 'real' commas contained within
#' \item commas are replaced with spaces 
#' \item leading and trailing white space is trimmed
#' \item empty rows are removed
#' }
#' 
#' @param filepath path to file 
#' @param outpath path to write repaired file to 
#' @param NComm_header Logical default false. If the final line of normal comments contains the data header, set to true
#' 
#' @author W. S. Drysdale
#' 
#' @export

repair_na_csv = function(
    filepath,
    outpath,
    NComm_header = FALSE
){
  lns = readLines(filepath)
  headerLength = stringr::word(lns[1], 1, sep = ",") |>
    as.numeric()
  
  if(NComm_header){
    headerLength = headerLength+1
  }
  
  for(i in 1:length(lns)){
    
    if(i > headerLength){ # Data lines contain spaces in their date columns, replace this with T 
      lns[i] = stringr::str_replace_all(lns[i], " ", "T") 
    }
    
    if(stringr::str_detect(lns[i],'"')){
      positions = stringr::str_locate_all(lns[i],'"')|> 
        lapply(\(x) c(min(x), max(x))) |> 
        purrr::pluck(1)
      
      lns[i] = stringr::str_sub(lns[i], start = positions[1], positions[2])
    }else{
      
      lns[i] = stringr::str_replace_all(lns[i], ",", " ") |> 
        stringr::str_trim()
    }
  }
  
  lns = lns[lns != ""]
  
  writeLines(lns, outpath)
  
}