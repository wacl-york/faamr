#' Read York GC-FID Lab (WAS/SWAS)
#' 
#' Read the GC-FID runs of whole air samples
#' 
#' @param filepath path to file
#' @param filetype Is the file a .na or a .na.csv? 
#'  If its the latter \code{repair_na_csv} to attempt to recover the data. 
#'  By default attempts to detect from the filepath file extension
#'  
#' @author W. S. Drysdale
#' 
#' @export

read_york_gc_fid_lab = function(
    filepath, 
    filetype = NULL
){
  
  if(is.null(filetype)){
    if(stringr::str_detect(basename(filepath), ".csv")){
      filetype = ".na.csv"
    }else{
      filetype = ".na"
    }
  }
  
  if(filetype == ".na.csv"){
    tempFile = tempfile()

    repair_na_csv(filepath, tempFile)
    
    filepath = tempFile
    
  }
  
  header = read_nasa_ames_header(filepath)
  
  dat = nasaAmesR::read_nasa_ames_1001(filepath) |> 
    stats::setNames(header$long_names) |> 
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains("date & time"),
        \(x) nanotime::nanotime(x, format = "%d/%m/%YT%H:%M:%S")
      )
    ) |> 
    janitor::clean_names()
  
  if(filetype == ".na.csv"){
    file.remove(tempFile)
  }
  
  dat
}


"dev_data/c121/Non-core/faam-fgga_faam_20180914_r0_c121.na"