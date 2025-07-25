#' Read Faam Core
#' 
#' Reads either the high time resolution or 1 Hz faam core files
#' 
#' @param filepath path to file
#' @param startDate string that can be coerced to a nanotime via: \code{nanotime::nanotime(startDate,format = "\%Y-\%m-\%d \%H:\%M:\%S")}
#'                  used to filter \code{tidync::hyper_array()}
#' @param endDate string that can be coerced to a nanotime via: \code{nanotime::nanotime(endDate,format = "\%Y-\%m-\%d \%H:\%M:\%S")}
#'                used to filter \code{tidync::hyper_array()}
#' @param selectVar vector of varibale names to load - loading all of them can take a long time!
#' @param sps samples per second - one of 2, 4, 32, 64. default 32
#' @param averageNanoString string to pass to \code{nanotime::nano_floor(date, nanotime::as.nanoduration(averageNanoString))} for resampling date
#'                          For example to resample to 10 Hz, argument is "00:00:00.1", for 1 hz it is "00:00:00"
#' 
#' @author W. S. Drysdale
#' 
#' @export


read_faam_core = function(filepath, 
                          startDate = NULL, 
                          endDate = NULL,
                          selectVar = NULL,
                          sps = c(2,4,32,64)[3],
                          averageNanoString = NULL 
){
  
  dat_meta = ncmeta::nc_meta(filepath)
  
  # get the date origin (midnight on date of flight)
  dateOrigin = get_core_date_origin(filepath)
  
  dat_nc = tidync::tidync(filepath)
  
  if(!is.null(startDate) & !is.null(endDate)){
    startNano = nanotime::nanotime(startDate,format = "%Y-%m-%d %H:%M:%S")
    startSeconds = as.numeric(startNano-dateOrigin)/1e9
    
    endNano = nanotime::nanotime(endDate,format = "%Y-%m-%d %H:%M:%S")
    endSeconds = as.numeric(endNano-dateOrigin)/1e9
    
    dat_nc = tidync::hyper_filter(dat_nc, 
                                  Time = dplyr::between(.data$Time, startSeconds, endSeconds))
  }else{
    
    # set start seconds to the begining of the file so we can add use it in the timestamp, even if we arent filtering
    startSeconds = min(tidync::hyper_transforms(dat_nc)$Time$Time)
    
  }
  
  if(nrow(dat_meta$dimension) > 1){ # for raw file
    dimToActivate = dat_meta$dimension |> 
      dplyr::mutate(did = paste0("D",.data$id)) |> 
      dplyr::filter(.data$length == sps)
    
    gridName = paste0(dimToActivate$did,",D0")
    
    dat_nc = dat_nc |> 
      tidync::activate(gridName)
    
    datArray = dat_nc |> 
      tidync::hyper_array(select_var = selectVar)
    
    datList = list(datArray, along = 3)

    dat = do.call(get("abind", getNamespace("abind")),datList) |> 
      as.data.frame.table(responseName = "value",
                          stringsAsFactors = TRUE) |> 
      dplyr::tibble() |> 
      dplyr::mutate(subsecond = as.integer(.data$Var1),
                    subsecond = .data$subsecond/max(.data$subsecond),
                    subsecond = .data$subsecond-min(.data$subsecond),
                    seconds_since_midnight = as.integer(.data$Var2) + .data$subsecond-1 + startSeconds,
                    "name" = as.character(.data$Var3),
                    date = (.data$seconds_since_midnight*1e9) + dateOrigin) |> 
      dplyr::select(tidyselect::all_of(c("date", "seconds_since_midnight", "name", "value")))
    
  }else{ # for the 1 hz file
    
    datArray = dat_nc |> 
      tidync::hyper_array(select_var = selectVar)
      
    datList = list(datArray, along = 2)
    
    dat = do.call(get("abind", getNamespace("abind")),datList) |> 
      as.data.frame.table(responseName = "value",
                          stringsAsFactors = TRUE) |> 
      dplyr::tibble() |> 
      dplyr::mutate(seconds_since_midnight = (as.integer(.data$Var1)-1 + startSeconds),
                    name = as.character(.data$Var2),
                    date = (.data$seconds_since_midnight*1e9) + dateOrigin) |>  
      dplyr::select(tidyselect::all_of(c("date", "seconds_since_midnight", "name", "value")))
    
  }
  
  flagVars = unique(dat$name)[stringr::str_detect(unique(dat$name), "_FLAG")]
  
  # Some flag columns have NA instead of 0 for good data (e.g SO2_TECO_FLAG), replace NAs in flag columns with 0 when we find this
  dat = dat |> 
    dplyr::mutate(
      "value" = ifelse(stringr::str_detect(.data$name, "_FLAG") & is.na(.data$value), 0, .data$value)
    )
  
  if(!is.null(averageNanoString)){
    dat = dat |> 
      dplyr::mutate(date = nanotime::nano_floor(.data$date, nanotime::as.nanoduration(averageNanoString))) |> 
      dplyr::group_by(.data$date, .data$name) |> 
      dplyr::summarise_all(mean, na.rm = T) |> 
      dplyr::ungroup()
  }
  
  # 
  dat
  
}




















