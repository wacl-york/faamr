#' Read FAAM WAS files
#'
#' Reads and loads WAS NASA Ames files formatted as described in:
#' http://cedadocs.ceda.ac.uk/73/4/index.html
#'
#'
#' @name read_faam_was
#' @param was_file_list  A list of WAS NASA Ames file names
#'
#' @return A dataframe
#' @author Freya Squires
#'
#' @export

read_faam_was <- function(was_file_list) {
  map_dfr(was_file_list, read_faam_was_worker)
}

read_faam_was_worker <- function(file){

  file <- file

  #get header names, file date and missing flag information
  meta <- ReadNasaAmes(file)

  #get flight number from file name - NOTE this may change for different age of files
  flight_no <- str_sub(basename(file), start = 34, end = 37) %>%
    tolower()

  df <- read.table(file,
                   header = F,
                   skip = meta$header_length,
                   sep = "")

  #evaluate the date stamp
  df$V1 <- meta$date + df$V1
  df$V2 <- meta$date + df$V2
  
  #apply column names - the user will probably want to tidy these up...
  names(df) <- meta$long_names
  
  #create centre date stamp (mid point between start and stop sample times)
  #this assumes that the first 2 rows correspond to time!
  df$date <- (as.numeric(df[,1]) + as.numeric(df[,2]))/2
  df$date <- as.POSIXct(df$date, tz = "UTC", origin = "1970-01-01")
  
  #create start and stop sample time columns (again assumes cols 1 and 2 are time columns! NOT ROBUST!!)
  #Leaving in original names and duplicating cols so user can check this.
  df$start_samp_time <- df[,1]
  df$end_samp_time <- df[,2]

  #add flight number column
  df$flight_no <- flight_no

  #return
  df

}

