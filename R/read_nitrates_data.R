#'Read FAAM nitrate files
#'
#'Reads and cleans core nitrates FAAM file.
#'
#'
#' @name read_nitrates_data
#' @param nitrates_file_list List containing file names
#' @param frequency Frequency in Hz to output the data at. FAAM nirate files can be outputted at 1 or 10 Hz.
#'
#' @return Tibble
#' 
#' @author Shona Wilde
#' @author Freya Squires
#'
#' @export
#'

read_nitrates_data <- function(nitrates_file_list, frequency = 1) {
  
  purrr::map_dfr(nitrates_file_list, read_nitrates_worker, frequency = frequency)
}

read_nitrates_worker <- function(file, frequency) {
  
  # Connection to ncdf file
  nc <- ncdf4::nc_open(file)
  
  # get meta data from ncdf
  meta_data <- nc %>% 
    capture.output() 
  
  # Get and format dates
  date_start <- nc$dim$time$units %>% 
    stringr::str_remove("seconds since ") %>% 
    lubridate::ymd_hms(tz = "UTC")
  
  date <- date_start + as.vector(nc$dim$time$vals) 
  
  # get flight number
  flight_no <- meta_data[stringr::str_which(meta_data, "flight_number")] %>% 
    stringr::str_split(":", simplify = F)  %>% 
    purrr::map_chr(`[` (2)) %>% 
    stringr::str_remove(" ") %>% 
    stringr::str_to_lower()
  
  # Get variable names
  variables <- names(nc$var)
  
  # Extract all data
  list_nc <- variables %>% 
    purrr::set_names(variables) %>% 
    purrr::map(~ncdf4::ncvar_get(nc, varid = .))
  
  class_list_nc <- list_nc %>% 
    purrr::set_names(variables) 
  
  # Function to change colnames
  change_colname_value <- function(df, new_colname){
    df %>% dplyr::rename(!!new_colname := value)
  }
  
  # extract all 10 Hz matrix elements
  nc_matrix <- list_nc %>% 
    purrr::keep(is.matrix)
  
  # get matrix names
  names_matrix <- names(nc_matrix)
  
  # clean and bind all 10 Hz data  
  data_10hz <- nc_matrix %>% 
    purrr::map(reshape2::melt) %>% 
    purrr::map(as_tibble) %>% 
    purrr::map2(names_matrix, change_colname_value) %>% 
    purrr::map(select, -1:-2) %>%  
    purrr::reduce(bind_cols) %>% 
    dplyr::mutate(date = rep(date, each = 10)) 
  
  # extract all 1 Hz elements
  nc_array <- list_nc %>% 
    purrr::discard(is.matrix)
  
  # get array names
  names_array <- names(nc_array)
  
  # close connection
  ncdf4::nc_close(nc)
  
  # clean and bind 1 Hz data
  data_1hz <- nc_array %>% 
    purrr::map(as_tibble) %>% 
    purrr::map(mutate, date = date) %>% 
    purrr::map2(names_array, change_colname_value) %>% 
    purrr::reduce(left_join, "date") 
  
  # merge all data - output data at user determined frequency
  
  if(frequency == 10)
    data_all <- data_10hz %>% 
    dplyr::left_join(data_1hz, "date") %>% 
    dplyr::mutate(flight_no = flight_no) %>% 
    dplyr::select(date, flight_no, everything())
  
  
  if(frequency == 1)
    data_all <- data_10hz %>%
    dplyr::group_by(date) %>% 
    dplyr::summarise_all(mean) %>% 
    dplyr::left_join(data_1hz, "date") %>% 
    dplyr::mutate(flight_no = flight_no) %>% 
    dplyr::select(date, flight_no, everything())
  
  
  return(data_all)
  
}
