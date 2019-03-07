#' Monitorización de la fase longitudinal de la depuración selectiva.
#'
#' \code{CheckInterval} obtiene los parámetros de la fase longitudinal de la 
#' depuración selectiva
#' 
#' @param variables \code{lista} con tres componentes: VarAnalisis, con los nombres 
#' de las variables implicadas en la contrucción de los controles de validación 
#' de la operación estadística considerada; VarAsociadas, con las variables 
#' que intervienen en el cálculo de cada variable de análisis y VarAuxiliares, con los
#' nombres de aquellas variables que quieran incluirse en la salida de la función.
#' 
#' @param units \linkS4class{data.table} con los identificadores de las unidades a
#' monitorizar.
#' 
#' @param edit.list lista de vectores de tipo \code{character} con los nombres de
#' los controles de validación de la operación estadística considerada.
#' 
#' @param edits \code{vector} de tipo \code{character} con los nombres de los
#' controles de validación que se quieren consultar.
#' 
#' @param edData objeto de clase \code{StQ} con los microdatos depurados de la
#' encuesta para el periodo consultado.
#' 
#' @param rawData objeto de clase \code{StQ} con los microdatos sin depurar de la 
#' encuesta para el periodo consultado.
#' 
#' @param edTS objeto de clase \code{StQ} con los microdatos depurados de la
#' encuesta utilizados para la construcción de los intervalos de validación del
#' periodo de referencia especificado en Periodo.
#' 
#' @param intervalData objeto de clase \code{StQ} con los datos del fichero FL obtenido
#' en la fase longitudinal del periodo de referencia especificado en Periodo.
#' 
#' @param flagData \code{lista} de \linkS4class{data.table} con la información
#' de los ficheros de errores.
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
#' \item edTS: data.table con la serie de valores de las unidades para las 
#' variables implicadas en los edits.
#' \item edits: data.table con los intervalos de validación de cada Edit
#' consultado y los parámetros utilizados en su construcción: Pred (centro del
#' intervalo), Pred_error (error de predicción de la variable), HRFactor (factor
#' de amplitud) y Condicion (indicador de la aplicación o no del control.)
#' 
#' @import data.table StQ
#' 
#' @examples
#' dontrun{
#' Variables <- list(VarAnalisis = list(LCN = 'logv9' ,
#'                                      LEX = 'logvexis',
#'                                      LRCNPT = 'logRatioCNPT',
#'                                      LTACNCCAA = 'logitAngleCNCCAA',
#'                                      LTACNCNAE = 'logitAngleCNCNAE',
#'                                      LTAPTCCAA = 'logitAnglePTCCAA',
#'                                      LTACNPTCCAA = 'logitAngleCNPTCCAA'),
#'                   VarAsociadas = list(LCN = 'v9' ,
#'                                       LEX = 'vexis',
#'                                       LRCNPT = c('v9', 'vexis'),
#'                                       LTACNCCAA = c(vecCNCCAA, lagVecCNCCAA),
#'                                       LTACNCNAE = c(vecCNCNAE, lagVecCNCNAE),
#'                                       LTAPTCCAA = c(vecPTCCAA, lagVecPTCCAA),
#'                                       LTACNPTCCAA = c(vecCNCCAA, vecPTCCAA)),
#'                   VarAuxiliares = list(INCIDENC = 'incidenc'))
#' Suffixes <- c('W_1', 'W_3', 'noW_3')
#' Edit.List <- list(LCN = paste0('LCN_', Suffixes),
#'                   LEX = paste0('LEX_', Suffixes),
#'                   LRCNPT = paste0('LRCNPT_', Suffixes),
#'                   LTACNCCAA = paste0('LTACNCCAA_', Suffixes),
#'                   LTACNCNAE = paste0('LTACNCNAE_', Suffixes),
#'                   LTAPTCCAA = paste0('LTAPTCCAA_', Suffixes),
#'                   LTACNPTCCAA = paste0('LTACNPTCCAA_', Suffixes))  
#'                  
#' output <- CheckInterval(units = list(NOrden = '4602232599M'), 
#'                         edits = c('LCN','LTACNCNAE'), 
#'                         variables = Variables, 
#'                         edit.List = Edit.list, 
#'                         edData = FF.StQList[[Periodo]],
#'                         intervalData = FL.StQList)
#'}
#'
#' @export
CheckInterval <- function(variables, units, edit.List, edits = names(edit.list),
                          edData, rawData, edTS, intervalData, flagData, 
                          invTrans.List, dTrans.List, lang = 'EN') {
  

  if (!lang %in% c('EN', 'SP')) stop ('Valid values for lang parameter are \"EN\" and \"SP\"')
  
  if (!all(edits %in% names(edit.List))) {
    
    ifelse (lang == 'SP', stop('[ChekInterval] Los edits válidos para la encuesta ', Encuesta, 'son ', names(edit.List), '.\n\n'), 
                          stop('[ChekInterval] Valid edits for survey ', Encuesta, 'are ', names(edit.List), '.\n\n'))
  }
  
  Edits <- toupper(edits)
  
  DD <- getDD(edTS)
  IDQuals <- setdiff(getIDQual(DD, 'MicroData'), c('Period'))
  if (dim(edTS.StQ[get(IDQuals) %in% Units[[names(Units)]]])[[1]] == 0) {
    
    if (lang == 'SP') stop('[ChekInterval] Ninguna unidad especificada en el parámetro units pertenece a los datos del parámetro edTS.')
    if (lang == 'EN') stop('[ChekInterval] No unit specified in the units parameter belongs to the data of the edTS parameter.')
    
  }
 
  if (!identical(names(units), as.character(IDDDToUnitNames(IDQuals, DD)))) {
    
    if (lang == 'SP') stop('[ChekInterval] Compruébense los nombres de las variables de identificación en el parámetro units.')
    if (lang == 'EN') stop('[ChekInterval] Check the names of the identification variable in the units parameter.')
    
  }
 

  #### Serie historica de las variables implicadas en los edits consultados 
  units_R <- copy(units)
  IDQuals_units <- UnitToIDDDNames(names(units_R), DD)
  setnames(units_R, IDQuals_units)

  Vars <- unlist(variables[['VarAnalisis']][names(variables[['VarAnalisis']]) %in% Edits])
  Vars.aux <- unlist(variables[['VarAuxiliares']])
  if (is.null(Vars.aux)) Vars.aux <- ''

  invTrans <- lapply(Edits, function(x){invTrans.List[[x]]})
  dTrans <- lapply(Edits, function(x){dTrans.List[[x]]})
  names(invTrans) <- Vars
  names(dTrans) <- Vars  
  
  Vars <- c(Vars, Vars.aux)
  Vars_R <- UnitToIDDDNames(Vars, DD)
  names(Vars) <- Vars_R
  
  EdTS.dt <- edTS[IDDD %in% StQ::ExtractNames(Vars_R)]
  EdTS.dt <- dcast_StQ(EdTS.dt, StQ::ExtractNames(Vars_R))
  setkeyv(EdTS.dt, IDQuals)
  EdTS.dt <- merge(EdTS.dt, units_R, by = IDQuals)
  EdTS.dt[, Order := orderRepoTime(Period), by = IDQuals]
  setorderv(EdTS.dt, c(IDQuals, 'Order'))
  Vars.EdTS_R <- intersect(Vars_R, names(EdTS.dt))
  EdTS.dt <- EdTS.dt[, .SD, .SDcols = c(IDQuals, 'Period', Vars.EdTS_R)]
  setnames(EdTS.dt, c(IDQuals, Vars.EdTS_R), c(IDQuals_units, Vars[Vars.EdTS_R]))
  setcolorder(EdTS.dt, c(IDQuals_units, 'Period', Vars[Vars.EdTS_R]))
  
  for (col in names(EdTS.dt)) {

      if (class(EdTS.dt[[(col)]]) == 'numeric') EdTS.dt[, (col) := round(get(col), 2)]
  }

  
  #### Intervalos y parametros de los edits consultados
  intData <- dcast_StQ(intervalData)
  setkeyv(intData, IDQuals)
  intData <- intData[units_R]
  setnames(intData, IDDDToUnitNames(names(intData), DD))
  editnames <- unlist(edit.List[Edits])
  intData <- intData[NombreEdit %chin% editnames]
  intData[, LimInf := as.numeric(LimInf)]
  intData[, LimSup := as.numeric(LimSup)]
  intData[, HRFactor := as.numeric(HRFactor)]
  intData[, Pred := (LimInf + LimSup) / 2]
  intData[, Pred_Error := (LimSup - Pred)/HRFactor]
  intData[, NombreVariable := IDDDToUnitNames(NombreVariable, DD)]

  IDQuals_Unit <- IDDDToUnitNames(IDQuals, DD)
  setcolorder(intData, c(IDQuals_Unit, 'NombreEdit', 'NombreVariable', 'Condicion', 'LimInf', 'LimSup', 'Pred', 'Pred_Error', 'HRFactor'))

  if (nrow(getData(edData)) > 0) {
    
    edData <- edData[IDDD %in% ExtractNames(Vars_R)]
    edData.dt <- dcast_StQ(edData)

    edData.dt <- merge(edData.dt, units, by.x = IDQuals, by.y = IDQuals_Unit, all.y = TRUE)
    cols.aux.dt <- names(Vars[Vars == Vars.aux])
    cols.edData.dt <- c(IDQuals, setdiff(names(Vars), cols.aux.dt))
    cols.aux.dt <- c(IDQuals, intersect(cols.aux.dt, names(edData.dt)))
    aux.dt <- edData.dt[, ..cols.aux.dt]
    setnames(aux.dt, cols.aux.dt, c(names(units), Vars[names(Vars) %in% cols.aux.dt]))
    edData.dt <- edData.dt[, ..cols.edData.dt]
    edData.dt <- melt(edData.dt, id.vars = IDQuals, variable.name = 'NombreVariable', value.name = 'FF')
    newVar <- IDDDToUnitNames(setdiff(cols.edData.dt, cols.aux.dt), DD)
    edData.dt[, NombreVariable := newVar[NombreVariable]]
    setnames(edData.dt, IDQuals, names(units))
    output <- merge(intData, edData.dt, by = intersect(names(intData), names(edData.dt)))

  } else {
    
    if (!identical(names(units), as.character(IDQuals_Unit))) {
      
      ifelse (lang == 'SP', stop('[ChekInterval] Compruébense los nombres de las variables de identificación en el parámetro units.'),
                            stop('[ChekInterval] Check the names of the identification variable in the units parameter.'))
    }
    aux.dt <- units
    edData.dt <- melt(units, id.vars = IDQuals_Unit, variable.name = 'variable')
    setnames(edData.dt, StQ::UnitToIDDDNames(names(edData.dt), DD))

    for (var in names(Vars[!Vars %in% Vars.aux])){
        
        edData.dt[, (var) := NA_real_]
    }
    newVar <- IDDDToUnitNames(setdiff(names(edData.dt), IDQuals), DD)
    edData.dt <- melt(edData.dt, id.vars = IDQuals, variable.name = 'NombreVariable', value.name = 'FF')
    edData.dt[, NombreVariable := newVar[NombreVariable]]
    setnames(edData.dt, IDQuals, names(units))
    output <- merge(intData, edData.dt, by = intersect(names(intData), names(edData.dt)))
  }

  if (nrow(getData(rawData)) > 0) {
      
    rawData <- rawData[IDDD %in% StQ::ExtractNames(Vars_R)]
    rawData.dt <- dcast_StQ(rawData)
    rawData.dt <- merge(rawData.dt, units, by.x = IDQuals, by.y = IDQuals_Unit, all.y = TRUE)
    newVar <- IDDDToUnitNames(setdiff(names(rawData.dt), IDQuals), DD)
    rawData.dt <- melt(rawData.dt, id.vars = IDQuals, variable.name = 'NombreVariable', value.name = 'FG')
    rawData.dt[, NombreVariable := newVar[NombreVariable]]
    setnames(rawData.dt, IDQuals, names(units))
    output.raw <- merge(output, rawData.dt, by = intersect(names(output), names(rawData.dt)))
    if (nrow(output.raw) == 0) {
      
      output <- merge(output, rawData.dt, by = names(units), allow.cartesian = TRUE)
      setnames(output, c('NombreVariable.x', 'NombreVariable.y'), c('NombreVariable', 'NombreVariable.FG'))
    
    } else {
      
      output <- output.raw
    }
      
  } else {
      
      if (names(units) != IDQuals_Unit) stop('Compruébense los nombres de las variables de identificación en el parámetro units.')
      rawData.dt <- melt(units, id.vars = IDQuals_Unit, variable.name = 'variable')
      setnames(rawData.dt, UnitToIDDDNames(names(rawData.dt), DD))
      for (var in names(Vars[!Vars %in% Vars.aux])){
          
          rawData.dt[, (var) := NA_real_]
      }
      newVar <- IDDDToUnitNames(setdiff(names(rawData.dt), IDQuals), DD)
      rawData.dt <- melt(rawData.dt, id.vars = IDQuals, variable.name = 'NombreVariable', value.name = 'FG')
      rawData.dt[, NombreVariable := newVar[NombreVariable]]
      setnames(rawData.dt, IDQuals, names(units))
      output <- merge(output, rawData.dt, by = intersect(names(intData), names(rawData.dt)))
      
  }

  
  flagData.Names <- names(flagData)

  for (name in flagData.Names){
    DT <- flagData[[name]]
    
    if (!is.null(DT)){
    
      setkeyv(DT, IDQuals)
      
      if ('TipoRegistro' %in% names(DT)){
        
        cols <- c(IDQuals, 'TipoRegistro', 'ErroresPrimeraDepu', 'ErroresUltimaDepu')
      
      } else {
        
        cols <- c(IDQuals, 'ErroresPrimeraDepu', 'ErroresUltimaDepu')  
      }
        
      flagsUnits <- DT[units_R][, .SD, .SDcols = cols]
      if (name == 'FlagsIRIA' & nrow(merge(DT, units_R)) == 0) flagsUnits[, setdiff(cols, IDQuals) := '']
      
    } else {
      
      
      flagsUnits <- copy(units)[
        , ErroresPrimeraDepu := NA][
        , ErroresUltimaDepu := NA]
      
    }
    
    setnames(flagsUnits, c(IDQuals, 'ErroresPrimeraDepu', 'ErroresUltimaDepu'), 
             c(IDQuals_Unit, paste0(c('ErrPrimDepu.', 'ErrUltimDepu.'), substr(name, nchar('Flags') + 1, nchar(name)))))

    output <- merge(output, flagsUnits, by = IDQuals_Unit)
    
  }
  

  if (var %in% names(invTrans)) {
    
    if (!is.null(invTrans[[var]]) & !is.null(dTrans[[var]])) {
      
      EdTS.dt[, (paste0('f(', var, ')')) := invTrans[[var]](as.numeric(get(var)))]

    
      for (col in c('LimInf', 'LimSup', 'Pred', 'Pred_Error', 'FF', 'FG')) {
        
        if (col == 'Pred_Error') {
          
          output[, (paste0('f(', col, ')')) := as.numeric(get(col)) / sqrt(abs(dTrans[[var]](invTrans[[var]](as.numeric(Pred)))))]
          
        } else {
          
          output[, (paste0('f(', col, ')')) := invTrans[[var]](as.numeric(get(col)))]
        }  
      }
    }
  }
  
  output <- merge(output, aux.dt, by = names(units))
  
  for (col in names(output)) {

    if (class(output[[(col)]]) == 'numeric') output[, (col) := round(get(col), 2)]
  }
  
  
  # output <- output[, names(output) := lapply(.SD, function(x) {x[is.na(x)] <- "" ; x})] #Al hacer esto, las columnas numericas se convierten en caracter
  
  
  if (lang == 'en') {
    
    setnames(output, 
             c('NombreVariable', 'NombreEdit', 'Condicion', 'LimInf', 'LimSup', 'Pred', 'Pred_Error', 'FF', 'FG'), 
             c('VariableName', 'EditName', 'Condition', 'LowBound', 'UppBound', 'Pred', 'Error_Pred', 'ed', 'raw'))
    if ('TipoRegistro' %in% names(output)) setnames(output, 'TipoRegistro', 'RegType')
    if ('NombreVariable.FG' %in% names(output)) setnames(output, 'NombreVariable.FG', 'VariableName.FG')
    if (!is.null(invTrans)) setnames(output,
                                     paste0('f(', c('LimInf', 'LimSup', 'Pred', 'Pred_Error', 'FF', 'FG'), ')'),
                                     paste0('f(', c('LowBound', 'UppBound', 'Pred', 'Error_Pred', 'ed', 'raw'), ')'))
    
  }
  

  out <- list(edTS = EdTS.dt[], edits = output[])  
  return(out)
}
