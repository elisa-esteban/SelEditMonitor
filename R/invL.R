#' Inversa de log(1+x) para cada uno de los elementos x del vector de entrada.
#'
#' \code{invL} Calcula la inversa de log(1+x), es decir, exp(x) - 1, para cada uno de los elementos
#' x del vector.
#'
#' @param p \code{Vector} de tipo \code{numeric} sobre el que se calculará la función.
#' 
#' @return \code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{p} con
#' el valor que toma la función: exp(x) - 1 para cada uno de los elementos del vector.
#'
#' @examples
#' dontrun{
#' 
#'}
#'
#' @export
invL <- function(p){
  
  return(exp(as.numeric(p)) - 1)
}