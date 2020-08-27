#'Calculate upper and lower quantiles
#'
#'Calculates 25th and 75th quantiles (using percentile from stats) so it can be easily used with 'dplyr' type functions.
#'
#'
#' @name quantile25
#' @param x A vector 
#' @param na.rm True or False
#'
#' @return Value
#' 
#' @author Freya Squires
#'
#' @export
#'


quantile25 <- function(x,na.rm) quantile(x,probs = 0.25,na.rm = na.rm)
quantile75 <- function(x,na.rm) quantile(x,probs = 0.75,na.rm = na.rm)