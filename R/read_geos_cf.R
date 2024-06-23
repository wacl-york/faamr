#' Read GEOS Chemical Forecast (CF) model files
#'
#' Reads and loads GEOS-CF model files.
#'
#' @name read_geos_cf
#' @param model_file_list  A list of file names
#'
#' @return A dataframe
#' @author Freya Squires
#' 
#' @export


read_geos_cf <- function (model_file_list){
  purrr::map_dfr(model_file_list, 
                 function(x){
                   file <- file
                   
                   df <- utils::read.csv(file)
                   
                   df <- dplyr::mutate(df,
                                       date = lubridate::ymd_hms(.data$Datetime),
                                       O3 = .data$O3*1e9,
                                       CO = .data$CO*1e9,
                                       SO2 = .data$SO2*1e9,
                                       NO = .data$NO*1e12,
                                       NO2 = .data$NO2*1e12,
                                       HCHO = .data$HCHO*1e9)
                   
                   return(df)
                   
                 })
}
