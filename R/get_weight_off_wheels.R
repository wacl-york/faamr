#' Get Weight Off Wheels
#' 
#' Given a path to a core ncdf file, return a dataframe of the WOW_IND (weight on wheels indicator), filtered for WOW_IND == 0 (in flight)
#' This is useful as WOW_IND is in the 1 Hz grid only, so the range can be supplied to \code{read_core_faam()} as \code{startDate} and \code{endDate}
#' 
#' @inheritParams read_faam_core
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_weight_off_wheels = function(filepath){
  
  dat_nc = tidync::tidync(filepath) |> 
    tidync::activate("D0")
  
  dateOrigin = get_core_date_origin(filepath)
  
  dat = tidync::hyper_tibble(dat_nc, "WOW_IND") |> 
    dplyr::mutate(
      Time = as.numeric(.data$Time),
      date = (.data$Time*1e9)+dateOrigin) |> 
    dplyr::select("date", "WOW_IND") |> 
    dplyr::filter(.data$WOW_IND == 0)
  
  dat
}