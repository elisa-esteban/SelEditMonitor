#' Derivada de Logit(x) para cada uno de los elementos x del vector de entrada.
#'
#' \code{DerivLTA} Calcula la derivada de Logit(x) para cada uno de los elementos x del vector.
#'
#' @param p \code{Vector} de tipo \code{numeric} sobre el que se calculará la función.
#' 
#' @param adjust \code{Vector} de tipo \code{numeric} de longitud 1. Por defecto toma el valor
#' \code{10^-15}.
#' 
#' @return \code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{p} con
#' el valor de la función derivada del Logit para cada uno de los elementos del vector.
#'
#' @examples
#' dontrun{
#' 
#'}
#'
#' @export
DerivLTA <- function(p, adjust = 1e-15, unit = 'degrees'){
  
  if (length(adjust) !=1) stop('[DerivLTA] El parametro adjust debe tener longitud 1.')
  if (!unit %chin% c('degrees', 'radians')) stop('[DerivLTA] Los valores validos para el parametro unit son: degrees, radians.')
  if (adjust <= 0 | adjust > 0.5) stop('[DerivLTA] El parametro adjust debe ser un numero positivo y no mayor que 0.5') ## (0 <= a < 1)
  
  p <- as.numeric(p)
  a <- 1 - 2 * adjust
  
  output <- - (2 * a * cos(p) * sin(p) / ((1 - a) / 2 + a * cos(p)^2) / ((1 + a) / 2 - a * cos(p)^2))
  if (unit == 'degrees') output <- output * 180 / pi
  
  return(output)
}