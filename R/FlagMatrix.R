#' Construcción de matrices indicadoras de controles activos
#' 
#' \code{FlagMatrix} devuelve una \code{\link{data.table}} con las variables 
#' binarias que indican para cada unidad estadística (fila) qué controles 
#' (columnas) se han activado.
#' 
#' Esta función lee el fichero FC cuyo nombre está especificado en 
#' \code{ed}, con diseño de registro especificado en el parámetro 
#' \code{Schema} y devuelve su contenido en una \code{data.table} con estructura
#'  matricial (unidades en filas y variables en columnas). Los nombres de las 
#' variables se asignan de acuerdo con el diseño de registro especificado.
#' 
#' 
#' @param ed objeto de clase \linkS4class{StQ} del que se obtienen las unidades
#' para las que se quiere obtener la tabla de controles activos.
#' 
#' @param Edit.List lista los nombres de los controles cuya activación quiere 
#' consultarse.
#' 
#' @param flagData \linkS4class{data.table} con el fichero de controles activados 
#' a partir del cual se quiere obtener la tabla de salida.
#' 
#' @param TypeEd Vector \code{character} indicando si los controles han saltado
#' en primera ('P') o en última depuación ('U').
#' 
#' @param by Vector \code{character} con los nombres de las variables de 
#' agrupación para el recuento de controles activos. Si no se especifica, se 
#' toma por defecto el valor del IDQual de los datos en el parámetro de entrada
#' ed.
#' 
#' @return \code{\link{data.table}} con la matriz indicadora de activación de 
#' controles. 
#' 
#' @examples
#' \dontrun{
#' data(FC)
#' }
#' 
#' @export
FlagMatrix <- function(flagData, TypeEd = 'P'){
  
  trim <- function(x){gsub("^\\s+|\\s+$", "", x, useBytes = T)}
  
  if (TypeEd == 'P'){
    
    queriedEdits <- 'ErroresPrimeraDepu'
    
  } else if (TypeEd == 'U') {
    
    queriedEdits <- 'ErroresUltimaDepu'
    
  } else {
    
    stop()
  }
  
  flagDT <- flagData[, (queriedEdits) := trim(get(queriedEdits))]
  
  editNames <- sort(unique(unlist(lapply(flagDT[[queriedEdits]], function(x){unlist(strsplit(x, split = ' ')[[1]])}))))

  flagDT[, (editNames) := logical(.N)]
  flagDT[, (editNames) := lapply(editNames, grepl, ErroresPrimeraDepu, fixed = TRUE)]
  flagDT[, ErroresPrimeraDepu := NULL][
    , ErroresUltimaDepu := NULL]
  return(flagDT[])  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# FlagMatrix <-  function(ed, Edit.List, flagData, TypeEd = 'P', by){
  
  
  trim <- function(x){gsub("^\\s+|\\s+$", "", x, useBytes = T)}
  
  IDQual <- getIDQual(ed, 'MicroData')
  
  if (missing(by)){
    
    by <- IDQual
  }
  
  Units <- getUnits(ed)
  
  if (TypeEd == 'P'){
    
    editNames <- 'ErroresPrimeraDepu'
    
  } else {
    
    editNames <- 'ErroresUltimaDepu'
    
  }
  
  out <- merge(Units, flagData[, c(IDQual, editNames), with = FALSE], all = TRUE, by = by)
  CodeList <- strsplit(out[[editNames]], " ")
  CodeList <- lapply(CodeList, trim)
  EditCodes <- sort(unique(unlist(Edit.List)))
  
  out <- Units[, Mes := unique(flagData)[['Mes']]]
  for (EditCode in EditCodes){
    out <- out[, EditCode := unlist(lapply(CodeList, function(x){(EditCode %in% x) * 1L})), with = F]
  }
  
  return(out)
}

