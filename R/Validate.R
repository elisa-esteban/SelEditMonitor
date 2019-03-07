Validate <- function(Data, intervalData) {
  
  DD.intData <- getDD(intervalData)
  IDQuals.intData <- getIDQual(DD.intData, 'ParaData')
  IDQuals.intData.unit <- IDDDToUnitNames(IDQuals.intData, DD.intData)
  
  intData <- StQ::dcast_StQ(intervalData)
  setnames(intData, names(intData), IDDDToUnitNames(names(intData), DD.intData))
  intData[, NombreVariable := ifelse(!is.na(NombreVariable), IDDDToUnitNames(NombreVariable, DD), NombreVariable), by = 'NombreVariable']
  
  if (dim(getData(Data))[[1]] > 0){
    
    DD.Data <- getDD(Data)
    IDQuals.Data <- IDDDToUnitNames(getIDQual(DD.Data, 'MicroData'), DD.Data)  
    DT <- dcast_StQ(Data)
    setnames(DT, names(DT), IDDDToUnitNames(names(DT), DD.Data))
    units <- DT[, IDQuals.Data, with = FALSE]
    intData <- merge(units, intData, by.x = IDQuals.Data, by.y = IDQuals.intData.unit, all.x = TRUE)
    varNames <- setdiff(names(DT), IDQuals.Data)

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
   
    ncols <- unlist(lapply(output.list, ncol))
    output.list <- split(output.list, ncols)
    output <- output.list[[setdiff(names(output.list), '2')]]
    output <- rbindlist(output)
    output.aux <- output.list[['2']]
    if (!is.null(output.aux)) {
      
      output.aux <- Reduce(merge, output.aux)
      output <- merge(output, output.aux)
      
    }
    
  }else {
    
    varNames <- character(0)
    output <- intData[, ValorVariable := NA]
    output <- intData[, Flag := NA]
    setnames(output, IDQuals.intData.unit, IDQuals.intData)
    
  }

  changeCols <- c('LimInf', 'LimSup', 'HRFactor')
  output[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  setcolorder(output, c(IDQuals.intData.unit, 'NombreVariable', 'NombreEdit', 'Flag', 'Condicion',  'LimInf', 'LimSup', 'ValorVariable', 'HRFactor', intersect(names(output), varNames)))
  output <- output[!is.na(NombreEdit)]
  return(output)  

}

