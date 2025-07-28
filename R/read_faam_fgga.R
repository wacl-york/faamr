##' Read FAAM fgga
##' 
##' Reads the nasa ames files produced for the FAAM Fast Greenhouse Gas Analyser
##' 
##' @param allowExtrapolatedCal mole fraction error flags can take the values 0, 1 and 2.
##'                             \itemize{
##'                                \item 0 signifies valid data and is always retained. 
##'                                \item 1 signifies reduced accuracy data where the calibration has been extrapolated
##'                                \item 2 signifies missing data, and these are always removed
##'                                }
##'                             Setting this value to FALSE removed data where the flag is 1. Default TRUE
##'                                
##' @param requireHighFlow The FGGA is capable of 10 Hz measurements, but is required to be in High Flow mode. 
##'                        This is flagged by the instrument_flow column, where flag == 1 is high flow mode, suitable for 10 Hz data
##'                        when \code{requireHighFlow == TRUE}, all the data is filtered for flag == 1, otherwise, flag %in% c(0, 1) is allowed
##'                        default FALSE
##' 
##' @inheritParams read_faam_core
##' 
##' @author W. S. Drysdale
##' 
##' @export

read_faam_fgga = function(filepath,
                          allowExtrapolatedCal = TRUE,
                          requireHighFlow = FALSE,
                          averageNanoString = NULL){
  
  fggaHeader = read_nasa_ames_header(filepath)
  
  if(stringr::str_detect(fggaHeader$long_names[1], "fractional")){
    dateOrigin = nanotime::nanotime(fggaHeader$long_names[1], format = "Time (fractional seconds elapsed since %Y-%m-%d %H:%M:%E9S UTC)", tz = "UTC")
  }else{
    dateOrigin = nanotime::nanotime(fggaHeader$long_names[1], format = "Time (seconds since %Y-%m-%d %H:%M:%S UTC)", tz = "UTC")
  }
  
  fgga = nasaAmesR::read_nasa_ames_1001(filepath) 
  
  if(ncol(fgga) == 5){
    cols = c("date", "co2_value", "co2_flag", "ch4_value", "ch4_flag")
  }
  
  if(ncol(fgga) == 6){
    cols = c("date", "co2_value", "co2_flag", "ch4_value", "ch4_flag", "instrument_flow")
  }
  
  fgga = fgga |>  
    stats::setNames(cols) |> 
    janitor::clean_names() |> 
    dplyr::mutate(date = (date*1e9)+dateOrigin) 
  
  if("instrument_flow" %in% cols){
    
    if(requireHighFlow){
      allowedFlowFlags = 1
    }else{
      allowedFlowFlags = c(0,1)
    }
    
    fgga = fgga |> 
      dplyr::filter(.data$instrument_flow %in% allowedFlowFlags) |> 
      dplyr::select(-"instrument_flow")
    
    cols = cols[cols != "instrument_flow"]
    
  }
  
  if(allowExtrapolatedCal){
    allowedMoleFlags = c(0,1)
  }else{
    allowedMoleFlags = 0
  }
  
  fgga = fgga |> 
    tidyr::pivot_longer(-date,
                        names_sep = "_",
                        names_to = c("species","type")) |> 
    tidyr::pivot_wider(names_from = "type") |> 
    dplyr::filter(.data$flag %in% allowedMoleFlags) |> 
    tidyr::pivot_longer(c("value", "flag"), names_to = "type") |> 
    tidyr::pivot_wider(names_from = c("species", "type"), names_sep = "_")
  
  if(!is.null(averageNanoString)){

    fgga = fgga |> 
      dplyr::mutate(date = nanotime::nano_floor(.data$date, nanotime::as.nanoduration(averageNanoString))) |> 
      tidyr::pivot_longer(-date) |> 
      dplyr::group_by(.data$date, .data$name) |> 
      dplyr::summarise_all(mean, na.rm = T) |> 
      dplyr::ungroup() |>
      tidyr::pivot_wider() |> 
      dplyr::relocate(tidyselect::all_of(cols)) # use to preserve the order of the columns, otherwise the before and after column order is different
  }
  
  fgga |> 
    dplyr::rename("co2_drymole_ppm" = "co2_value",
                  "ch4_drymole_ppb" = "ch4_value")
  
}

##' Extract fGGA Uncertainties
##' 
##' The FAAM non-core fGGA instrument mena bias and uncertainty are stored in the NASA Ames header normal comments.
##' This function extracts the values and returns a tibble containing bias and uncertainty for CO2 and CH4.
##' 
##' @inheritParams read_faam_core
##' 
##' @author W. S. Drysdale
##' 
##' @export

extract_fgga_uncert = function(filepath){
  header = read_nasa_ames_header(filepath)
  uncertaintiesBegin = grep("The mean biases and 1-sigma overall uncertainties are therefore quoted as:", header$normal_comments)
  
  stat = c(header$normal_comments[uncertaintiesBegin+1],header$normal_comments[uncertaintiesBegin+2])
  
  tibble::tibble(
    bias = stat |>  
      stringr::word(1, sep = "\\+") |> 
      stringr::str_trim() |> 
      as.numeric(),
    uncert = stat |>  
      stringr::word(2, sep = "\\/") |> 
      stringr::word(2, sep = "\\s+") |> 
      as.numeric(),
    name = c("co2_ppm", "ch4_ppb")
  )
}