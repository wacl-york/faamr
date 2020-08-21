#' Calculate Normalised Mean Bias
#'
#' Calculates the normalised mean bias (NMB) of modelled and measured
#' values according to formula in Emery 2017 (doi: 10.1080/10962247.2016.1265027)
#'
#' @name NormalisedMeanBias
#' @param modelled  Column of modelled values
#' @param observations Column of measured values
#'
#' @return NMB
#'
#' @author Freya Squires
#'
#' @export

NormalisedMeanBias <- function(modelled, observations, na.rm){

  mod <- modelled
  obs <- observations

  NMB <- (sum(mod - obs, na.rm = na.rm)/sum(obs, na.rm = na.rm))*100
  NMB

}
