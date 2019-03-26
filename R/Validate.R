Validate <- function(Data, intervalData, variables) {
  
  DD.intData <- getDD(intervalData)
  IDQuals.intData <- getIDQual(DD.intData, 'ParaData')
  IDQuals.intData.unit <- IDDDToUnitNames(IDQuals.intData, DD.intData)
  
  intData <- StQ::dcast_StQ(intervalData)
  setnames(intData, names(intData), IDDDToUnitNames(names(intData), DD.intData))
  intData[, NombreVariable := ifelse(!is.na(NombreVariable), IDDDToUnitNames(NombreVariable, DD), NombreVariable), by = 'NombreVariable']
  
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
      rule <- validate::validator(round(as.numeric(varName), 5) <= round(as.numeric(LimSup), 5) &
                          round(as.numeric(varName), 5) >= round(as.numeric(LimInf), 5))
      flag <- validate::confront(dat = intData.aux, x = rule)
      intData.aux[, Flag := !validate::values(flag)]
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
    setnames(output, IDQuals.intData.unit, IDQuals.intData)
    
  }

  changeCols <- c('LimInf', 'LimSup', 'HRFactor')
  output[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  if (varAuxiliares %in% names(output))
    setcolorder(output, c(IDQuals.intData.unit, 'NombreVariable', 'NombreEdit', 'Flag', 'Condicion',  'LimInf', 'LimSup', 'ValorVariable', 'HRFactor', varAuxiliares))
  else
    setcolorder(output, c(IDQuals.intData.unit, 'NombreVariable', 'NombreEdit', 'Flag', 'Condicion',  'LimInf', 'LimSup', 'ValorVariable', 'HRFactor'))
  output <- output[!is.na(NombreEdit)]
  setkeyv(output, IDQuals.intData.unit)
  return(output)  

}

