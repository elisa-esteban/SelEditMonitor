#' Monitorización de la fase transversal de la depuración selectiva.
#'
#' \code{CheckFT} obtiene los parámetros de la fase transversal de la 
#' depuración selectiva
#' 
#' @param variables \code{vector} de tipo \code{character} con los nombres de las
#' variables para los que se realiza la monitorización.
#' 
#' @param units \linkS4class{data.table} con los identificadores de las unidades a
#' monitorizar.
#' 
#' @param edData objeto de clase \code{StQList} con los microdatos depurados de 
#' la encuesta utilizados para la construcción de los intervalos de validación
#' del periodo de referencia especificado en Periodo.
#' 
#' @param varParam objeto de clase \code{contObsPredModelParam} con información
#' de la fase transversal.
#' 
#' @param score \linkS4class{data.table} con el valor de la función score para
#' cada unidad en la muestra.
#' 
#' @param selInfo objeto de clase \code{StQ} con los datos del fichero FT obtenido
#' en la fase transversal del periodo de referencia especificado en Periodo.
#' 
#' @param invTrans.List \code{lista} con los nombres de las funciones inversas, si se ha hecho
#' transformación (en otro caso con el valor NULL), para cada uno de los edits.
#' 
#' @param dTrans.List \code{lista} con los nombres de las funciones derivadas, si se ha hecho
#' transformación (en otro caso con el valor NULL), para cada uno de los edits.
#' 
#' @param lang \code{vector} de tipo \code{character} y longitud uno con el 
#' idioma en que se quiere obtener la salida. Sus posibles valores son: 'EN'
#' (English) y 'SP' (Servicio Promotor) y su valor por defecto es 'EN'.
#'
#' @return La función devuelve una lista con dos componentes:
#' \itemize{
#'  \item \code{edTS}: data.table con la serie de valores de las unidades para las 
#'  variables especificadas.
#'  \item \code{Param}: data.table con los parámetros utilizados en la fase transversal.
#' }
#' 
#' @import data.table StQ
#' 
#' @examples
#' dontrun{
#' 
#' }
#' @export
CheckFT <- function(variables, units, edData, varParam, score, selInfo,
                    invTrans.List, dTrans.List, lang = 'EN') {
  
  if (!lang %in% c('EN', 'SP')) stop ('Valid values for lang parameter are \"EN\" and \"SP\"')
  
  ### Preparacion de los datos para la extraccion de los parametros a mostrar
  variables <- unlist(variables)
  invTrans <- lapply(names(variables), function(x){invTrans.List[[x]]})
  dTrans <- lapply(names(variables), function(x){dTrans.List[[x]]})
  names(invTrans) <- variables
  names(dTrans) <- variables
  varParam.Data <- varParam@Data
  DD <- getDD(varParam.Data)
  IDQual <- getIDQual(varParam.Data)
  IDQual <- intersect(names(getData(varParam.Data)), IDQual)
  setnames(units, IDQual)
  varParam.Data <- varParam.Data[units]
  varParam.dt <- dcast_StQ(varParam.Data)

  
  #### Obtencion de datos historicos de las variables
  if (lang == 'SP') cat('Obteniendo el historico de las variables...\n\n')
  else cat('Obtaining the history of the variables...\n\n')

  variables_R <- UnitToIDDDNames(variables, DD)
  FF <- StQListToStQ(edData)
  FF <- FF[units]
  FF.values <- dcast_StQ(FF, ExtractNames(variables_R))
  FF.values <- FF.values[, c(IDQual, 'Period', variables_R), with = FALSE]
  setnames(FF.values, variables_R, variables)
  FF.values[, Order := orderRepoTime(Period)]
  setorderv(FF.values, c((IDQual), 'Order'))
  FF.values[, Order := NULL]

  ### Obtencion de los parametros a mostrar
  Param.values <- list()
  unit.output <- lapply(units[[IDQual]], function(unit) {
    
      if (unit %in% selInfo[[IDQual]]) {
        
        selInfo <- selInfo[unit]
        selInfo.dt <- dcast_StQ(selInfo)
        setnames(selInfo.dt, IDDDToUnitNames(names(selInfo.dt), DD))
      }
      
      
      output <- lapply(seq(along = variables), function(i) {
        
        if(lang == 'SP') cat('Obteniendo los parametros a mostrar para la variable ', variables[i], '...\n\n')
        else cat('Obtaining the parameters to show for the variable ', variables[i], '...\n\n')
        
        out <- varParam.dt[get(IDQual) == unit, c(IDQual, variables_R[i], paste0('Pred', variables_R[i]),
                                                      paste0('PredErrorSTD', variables_R[i]),
                                                      paste0('ObsErrorSTD', variables_R[i]),
                                                      paste0('DesignW', variables_R[i]),
                                                      paste0('ErrorProb', variables_R[i])), with = FALSE]
            
        if (unit %in% selInfo[[IDQual]]) {
              
          CuantMom <- selInfo.dt[get(Edit) == names(variables)[i]][['CuantMom']]
          out[, paste0('CuantMom', variables[i]) := CuantMom]
              
        } else {
              
          out[, paste0('CuantMom', variables[i]) := '']
            
        }
         
        return(out)
      })
      output <- Reduce(merge, output)
      
      fixedNames <- names(output)[grep('CuantMom', names(output))]
      if (unit %in% score[[IDQual]]) {
        
        output <- merge(output, score)
        fixedNames <- c(fixedNames, setdiff(names(score), IDQual))
        setnames(output, setdiff(names(output), fixedNames), IDDDToUnitNames(setdiff(names(output), fixedNames), DD))
        PositionScore <- which(score[[IDQual]] == unit)
        output[, PositionScore := PositionScore]
        
      } else {
        
        setnames(output, setdiff(names(output), fixedNames), IDDDToUnitNames(setdiff(names(output), fixedNames), DD))
        output[, GlobalScore := 0]
        output[, PositionScore := '']
      }
      
      return(output)
  })
  Param.values <- rbindlist(unit.output)


  for (var in variables) {
    
    if (!is.null(invTrans[[var]]) & !is.null(dTrans[[var]])) {
      
      FF.values[, (paste0('f(', var, ')')) := invTrans[[var]](as.numeric(get(var)))]
      Param.values[, (paste0('f(', var, ')')):= invTrans[[var]](get(var))]
      Param.values[, (paste0('f(Pred', var, ')')):= invTrans[[var]](get(paste0('Pred', var)))]
      Param.values[, (paste0('f(PredErrorSTD', var, ')')):= get(paste0('PredErrorSTD', var)) / sqrt(abs(dTrans[[var]](invTrans[[var]](get(paste0('Pred', var))))))]
      Param.values[, (paste0('f(ObsErrorSTD', var, ')')):= get(paste0('ObsErrorSTD', var)) / sqrt(abs(dTrans[[var]](invTrans[[var]](get(paste0('Pred', var))))))]
    }

  }
  
  setcolorder(Param.values, c(setdiff(names(Param.values), c('GlobalScore', 'PositionScore')), c('GlobalScore', 'PositionScore')))
    
  output <- list(edTS = FF.values[], Param = Param.values[])
  return(output)
  
}