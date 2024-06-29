#' Read FAAM Nitrates
#' 
#' Reads faam core nitrates file
#' 
#' @inheritParams read_faam_core
#' 
#' @author W. S. Drysdale
#' 
#' @export

read_faam_nitrates = function(
  filepath,
  startDate = NULL,
  endDate = NULL, 
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
                                  time = dplyr::between(.data$time, startSeconds, endSeconds))
    }else{
      
    # set start seconds to the begining of the file so we can add use it in the timestamp, even if we arent filtering
    startSeconds = min(tidync::hyper_transforms(dat_nc)$time$time)
      
    }
  
    datArray = dat_nc |> 
      tidync::hyper_array(select_var = c("no_mr","no2_mr"))
    
    datList = list(datArray, along = 3)
  
    dat = do.call(get("abind", getNamespace("abind")),datList) |> 
      as.data.frame.table(responseName = "value",
                          stringsAsFactors = TRUE) |> 
      dplyr::tibble() |> 
      dplyr::mutate(subsecond = as.integer(.data$Var1),
                    subsecond = .data$subsecond/max(.data$subsecond),
                    subsecond = .data$subsecond-min(.data$subsecond),
                    seconds_since_midnight = as.integer(.data$Var2) + .data$subsecond-1 + startSeconds,
                    name = as.character(.data$Var3),
                    date = (.data$seconds_since_midnight*1e9) + dateOrigin) |> 
      dplyr::select(tidyselect::all_of(c("date", "seconds_since_midnight", "name", "value")))
  
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