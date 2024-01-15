#' Read Faam Core
#' 
#' Reads either the high time resolution or 1 Hz faam core files
#' 
#' @param filepath path to file
#' @param startDate string that can be cohered to a nanotime via: \code{nanotime::nanotime(startDate, format = "seconds since %Y-%m-%d %H:%M:%S %z")}
#'                  used to filter \code{tidync::hyper_array()}
#' @param endDate string that can be cohered to a nanotime via: \code{nanotime::nanotime(endDate, format = "seconds since %Y-%m-%d %H:%M:%S %z")}
#'                used to filter \code{tidync::hyper_array()}
#' @param selectVar vector of varibale names to load - loading all of them can take a long time!
#' @param sps samples per second - one of 2, 4, 32, 64. default 32
#' @param averageNanoString string to pass to \code{nanotime::nano_floor(date, nanotime::as.nanoduration(averageNanoString)))} for resampling date
#' 
#' @author W. S. Drysdale
#' 
#' @export


read_famm_core = function(filepath, 
                          startDate = NULL, 
                          endDate = NULL,
                          selectVar = NULL,
                          sps = c(2,4,32,64)[3],
                          averageNanoString = NULL # to resample to 10 Hz, argument is "00:00:00.1"
){
  
  dat_meta = ncmeta::nc_meta(filepath)
  
  # get the date origin (midnight on date of flight)
  dateOrigin = dat_meta$attribute |> 
    dplyr::filter(variable == "Time",
                  name == "units") |> 
    tidyr::unnest(value) |> 
    purrr::pluck("value") |> 
    nanotime::nanotime(format = "seconds since %Y-%m-%d %H:%M:%S %z")
  
  dat_nc = tidync::tidync(filepath)
  
  if(!is.null(startDate) & !is.null(endDate)){
    startNano = nanotime::nanotime(startDate,format = "%Y-%m-%d %H:%M:%S")
    startSeconds = as.numeric(startNano-dateOrigin)/1e9
    
    endNano = nanotime::nanotime(endDate,format = "%Y-%m-%d %H:%M:%S")
    endSeconds = as.numeric(endNano-dateOrigin)/1e9
    
    dat_nc = tidync::hyper_filter(dat_nc, 
                                  Time = between(Time, startSeconds, endSeconds))
  }else{
    
    # set start seconds to the begining of the file so we can add use it in the timestamp, even if we arent filtering
    startSeconds = min(tidync::hyper_transforms(dat_nc)$Time$Time)
    
  }
  
  if(nrow(dat_meta$dimension) > 1){ # for raw file
    dimToActivate = dat_meta$dimension |> 
      dplyr::mutate(did = paste0("D",id)) |> 
      dplyr::filter(length == sps)
    
    gridName = paste0(dimToActivate$did,",D0")
    
    dat_nc = dat_nc |> 
      tidync::activate(gridName)
    
    datArray = dat_nc |> 
      tidync::hyper_array(select_var = selectVar)
    
    datList = list(datArray, along = 3)
    
    dat = do.call(getFromNamespace("abind","abind"),datList) |> 
      as.data.frame.table(responseName = "value",
                          stringsAsFactors = TRUE) |> 
      dplyr::tibble() |> 
      dplyr::mutate(subsecond = as.integer(Var1),
                    subsecond = (subsecond-1)/max(subsecond-1),
                    seconds_since_midnight = as.integer(Var2)+subsecond-1+startSeconds,
                    name = as.character(Var3),
                    date = (seconds_since_midnight*1e9)+dateOrigin) |> 
      dplyr::select(date ,seconds_since_midnight, name, value)
    
  }else{ # for the 1 hz file
    
    datArray = dat_nc |> 
      tidync::hyper_array(select_var = selectVar)
      
    datList = list(., along = 2)
    
    dat = do.call(getFromNamespace("abind","abind"),.) |> 
      as.data.frame.table(responseName = "value",
                          stringsAsFactors = TRUE) |> 
      dplyr::tibble() |> 
      dplyr::mutate(seconds_since_midnight = (as.integer(Var1)-1+startSeconds),
                    name = as.character(Var2),
                    date = (seconds_since_midnight*1e9)+dateOrigin) |>  
      dplyr::select(date ,seconds_since_midnight, name, value)
    
  }
  
  if(!is.null(averageNanoString)){
    dat = dat |> 
      mutate(date = nanotime::nano_floor(date, nanotime::as.nanoduration(averageNanoString))) |> 
      group_by(date, name) |> 
      summarise_all(mean, na.rm = T) |> 
      ungroup()
  }
  
  # 
  dat
  
}




















