#' Calculate Normalised Mean Bias
#'
#' Calculates the normalised mean bias (NMB) of modelled and measured
#' values according to formula in Emery 2017 (doi: 10.1080/10962247.2016.1265027)
#'
#' @param modelled  Column of modelled values
#' @param observations Column of measured values
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
#'
#' @return NMB
#'
#' @author Freya Squires
#'
#' @export

normalised_mean_bias <- function(modelled, observations, na.rm){

  mod <- modelled
  obs <- observations

  NMB <- (sum(mod - obs, na.rm = na.rm)/sum(obs, na.rm = na.rm))*100
  NMB

}
