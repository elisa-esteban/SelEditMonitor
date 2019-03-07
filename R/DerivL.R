#' Derivada de log(1+x) para cada uno de los elementos x del vector de entrada.
#'
#' \code{DerivL} Calcula la derivada de log(1+x), es decir, 1 / (1 + x), para cada uno de los
#' elementos x del vector.
#'
#' @param p \code{Vector} de tipo \code{numeric} sobre el que se calculará la función.
#' 
#' @return \code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{p} con
#' el valor que toma la función: 1 / (1 + x) para cada uno de los elementos del vector.
#'
#' @examples
#' dontrun{
#' 
#'}
#'
#' @export
DerivL <- function(p){

  return(1 / (1 + as.numeric(p)))
}