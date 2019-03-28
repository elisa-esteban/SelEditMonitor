#' Validación de la fase longitudinal de la depuración selectiva.
#'
#' \code{Validate} comprueba si debería haber saltado o no cada control, en función de los valores
#' informados y los intervalos obtenidos para cada control en la fase longitudinal de la depuración
#' selectiva.
#' 
#' @param Data objeto de clase \code{StQ} con los microdatos de la encuesta para los que se quiere
#' realizar la consulta.
#' 
#' @param intervalData objeto de clase \code{StQ} con los datos del fichero FL obtenido en la fase
#' longitudinal del periodo al que pertenecen los datos del parámetro \code{Data}.
#' 
#' @param variables \code{lista} con tres componentes: VarAnalisis, con los nombres de las variables
#' implicadas en la contrucción de los controles de validación de la operación estadística
#' considerada; VarAsociadas, con las variables que intervienen en el cálculo de cada variable de
#' análisis y VarAuxiliares, con los nombres de aquellas variables que quieran incluirse en la
#' salida de la función.
#' 
#' @param lang \code{vector} de tipo \code{character} y longitud uno con el idioma en que se quiere
#' obtener la salida. Sus posibles valores son: 'EN'(English) y 'SP' (Servicio Promotor) y su valor
#' por defecto es 'EN'.
#'
#' @return La función devuelve una \code{data.table} con los intervalos de validación para cada
#' control de cada variable de análisis, el valor informado de ésta y TRUE o FALSE en la columna
#' Flag dependiendo de si el control debería haberse activado o no, así como otra información
#' complementaria.
#' 
#' @import data.table StQ validate
#' 
#' @examples
#' dontrun{
#' Variables.list <- list(VarAnalisis = list(LCN = 'logv9' ,
#'                                           LEX = 'logvexis',
#'                                           LRCNPT = 'logRatioCNPT',
#'                                           LTACNCCAA = 'logitAngleCNCCAA',
#'                                           LTACNCNAE = 'logitAngleCNCNAE',
#'                                           LTAPTCCAA = 'logitAnglePTCCAA',
#'                                           LTACNPTCCAA = 'logitAngleCNPTCCAA'),
#'                       VarAsociadas = list(LCN = 'v9' ,
#'                                           LEX = 'vexis',
#'                                           LRCNPT = c('v9', 'vexis'),
#'                                           LTACNCCAA = c(vecCNCCAA, lagVecCNCCAA),
#'                                           LTACNCNAE = c(vecCNCNAE, lagVecCNCNAE),
#'                                           LTAPTCCAA = c(vecPTCCAA, lagVecPTCCAA),
#'                                           LTACNPTCCAA = c(vecCNCCAA, vecPTCCAA)),
#'                       VarAuxiliares = list(INCIDENC = 'incidenc'))
#'                  
#' output <- Validate(Data = FF, 
#'                    intervalData = FL,
#'                    variables = Variables.list,
#'                    lang = 'SP')
#'}
#'
#' @export
Validate <- function(Data, intervalData, variables, lang = 'EN') {
  
  
  if (!lang %in% c('EN', 'SP')) stop ('[SelEditMonitor Validate] Valid values for lang parameter are \"EN\" and \"SP\"')
  
  DD.intData <- getDD(intervalData)
  IDQuals.intData <- getIDQual(DD.intData, 'ParaData')
  IDQuals.intData.unit <- IDDDToUnitNames(IDQuals.intData, DD.intData)
  
  intData <- dcast_StQ(intervalData)
  setnames(intData, names(intData), IDDDToUnitNames(names(intData), DD.intData))
  intData[, NombreVariable := ifelse(!is.na(NombreVariable), IDDDToUnitNames(NombreVariable, DD.intData), NombreVariable), by = 'NombreVariable']
  
  if (dim(getData(Data))[[1]] > 0) {
    
    DD.Data <- getDD(Data)
    IDQuals.Data <- IDDDToUnitNames(getIDQual(DD.Data, 'MicroData'), DD.Data)  
    DT <- dcast_StQ(Data)
    setnames(DT, names(DT), IDDDToUnitNames(names(DT), DD.Data))
    units <- DT[, IDQuals.Data, with = FALSE]
    intData <- merge(units, intData, by.x = IDQuals.Data, by.y = IDQuals.intData.unit, all.x = TRUE)
    varAnalisis <- unlist(variables$VarAnalisis)
    varAuxiliares <- unlist(variables$VarAuxiliares)
    varNames <- intersect(c(varAnalisis, varAuxiliares), names(DT))

    output.list <- lapply(varNames, function(varName){
      
      DT.aux <- DT[, c(IDQuals.Data, varName), with = FALSE]
      intData.aux <- intData[NombreVariable == varName]
      if (nrow(intData.aux) == 0) return(DT.aux)
      setnames(DT.aux, varName, 'varName')
      intData.aux <- merge(intData.aux, DT.aux, by = IDQuals.Data, all = TRUE)
      rule <- validator(round(as.numeric(varName), 5) <= round(as.numeric(LimSup), 5) &
                          round(as.numeric(varName), 5) >= round(as.numeric(LimInf), 5))
      flag <- confront(dat = intData.aux, x = rule)
      intData.aux[, Flag := !values(flag)]
      intData.aux[Condicion == 0, Flag := FALSE]
      setnames(intData.aux, 'varName', 'ValorVariable')
      return(intData.aux)
    })
    names(output.list) <- varNames
    
    output <- output.list[names(output.list) %in% varAnalisis]
    output <- rbindlist(output)
    output.aux <- output.list[names(output.list) %in% varAuxiliares]
    output.aux <- rbindlist(output.aux)
    if (dim(output.aux)[1] > 0) output <- merge(output, output.aux, by = IDQuals.intData.unit)
    
    
  } else {
    
    varNames <- character(0)
    output <- intData[, ValorVariable := NA]
    output <- intData[, Flag := NA]
    
  }

  changeCols <- c('LimInf', 'LimSup', 'HRFactor')
  output[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  
  if (varAuxiliares %in% names(output))
    setcolorder(output, c(IDQuals.intData.unit, 'NombreVariable', 'NombreEdit', 'Flag', 'Condicion',  'LimInf', 'LimSup', 'ValorVariable', 'HRFactor', varAuxiliares))
  else
    setcolorder(output, c(IDQuals.intData.unit, 'NombreVariable', 'NombreEdit', 'Flag', 'Condicion',  'LimInf', 'LimSup', 'ValorVariable', 'HRFactor'))
  
  output <- output[!is.na(NombreEdit)]
  setkeyv(output, IDQuals.intData.unit)
  
  if (lang == 'EN') {
    
    setnames(output,
             c('NombreVariable', 'NombreEdit', 'Condicion',  'LimInf', 'LimSup', 'ValorVariable'),
             c('VariableName', 'EditName', 'Condition', 'LowBound', 'UppBound', 'VariableValue'))
  }
  
  return(output)  
}

