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


read_geos_cf <- function (model_file_list)
{
  purrr::map_dfr(model_file_list, read_geos_cf_worker)
}

read_geos_cf_worker <- function(file){

  file <- file

  df <- read.csv(file)

  df <- dplyr::mutate(df,
                      date = ymd_hms(df$Datetime),
                      O3 = O3*1e9,
                      CO = CO*1e9,
                      SO2=SO2*1e9,
                      NO=NO*1e12,
                      NO2=NO2*1e12,
                      HCHO=HCHO*1e9)
  
  return(df)
}
