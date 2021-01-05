#' Read York AQD NOx files
#'
#' Reads and loads York NOx AQD files formatted as described in:
#' http://cedadocs.ceda.ac.uk/73/4/index.html
#'
#'
#' @name read_yorknox_data
#' @param nox_file_list  A list of York AQD NOx file names
#'
#' @return A dataframe
#' @author Freya Squires
#'
#' @export


read_yorknox_data <- function(nox_file_list){
  map_dfr(nox_file_list, read_yorknox_data_worker)
}

read_yorknox_data_worker <- function(file){

  file <- file

  #get header names, file date and missing flag information
  meta <- ReadNasaAmes(file)

  #get flight number from the file name
  flight_no <- str_sub(basename(file), start = 35, end = 38) %>%
    tolower()

  #for older flights the header length given is wrong and is too short so does not omit
  #the file headers. Add 1 to the header length when reading in. This will in newer
  #cases remove the first data point.

  df <- read.table(file,
                   header = F,
                   skip = meta$header_length+1,
                   sep = "")

  #evaluate the date stamp
  df$V1 <- meta$date + df$V1

  #apply column names - the user will probably want to tidy these up...
  names(df) <- meta$long_names
  colnames(df)[str_detect(colnames(df), pattern = "seconds since")] <- "date"

  df$flight_no <- flight_no

  #return
  df

}
