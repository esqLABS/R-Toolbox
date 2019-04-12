setTableParameter <- function(xVals = c(), yVals = c(), path_id, options = {}, DCI_Info = {}, restartSolver = c())
{
  if (length(options) == 0)
  {
    options <- list(Type = "variable", Index = numeric(0))
  }
  if (length(grep("Type",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- "variable"
    names(options)[length(options)] <- "Type"
  }
  if (length(grep("Index",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- numeric(0)
    names(options)[length(options)] <- "Index"
  }
  if (length(DCI_Info) == 0)
  {
    stop("No DCI_Info provided.")
  }
  if (is.character(path_id) && path_id == "")
  {
    stop("Empty path_id provided.")
  }
  if (is.numeric(path_id) & length(path_id) != 1)
  {
    stop("Changing values of only one parameter at a time is supported, but multiple path_ids are provided.")
  }
  if (length(xVals) == 0){
    stop("No values provided.")
  }
  if (length(xVals) != length(yVals)){
    stop("xVals and yVals must have of the same length, but they are not!"); 
  }
  if (typeof(xVals) != "double" || typeof(yVals) != "double"){
    stop("xVals and yVals must contain numerical entries only."); 
  }
  if (length(restartSolver != 0) && typeof(restartSolver) != "double"){
    stop("restartSolver must contain entries 0 (for FALSE) and 1 (for TRUE) only!"); 
  }
  if (length(restartSolver) != 0){
    if (length(xVals) != length(restartSolver)){
      stop("xVals and restartSolver must have of the same length, or restartSolver must be empty"); 
    }
  }

  if (!(toupper(options$Type) %in% c("VARIABLE", "REFERENCE"))	)
  {
    stop("Invalid type provided. Use one of the following: variable, reference.")
  }
  
  table = createTable(xVals, yVals, path_id, DCI_Info, restartSolver);

  iTab <- which(names(DCI_Info$InputTab) == "VariableParameters")
  iParameterTable <- which(names(DCI_Info$InputTab) == "VariableTableParameters")
  
  if (toupper(options$Type) != "REFERENCE")
  {
    Table <- DCI_Info$InputTab[[iTab]]
    parameterTable <- as.data.frame(DCI_Info$InputTab[[iParameterTable]])
  } else {
    Table <- DCI_Info$ReferenceTab[[iTab]]
    parameterTable <- as.data.frame(DCI_Info$ReferenceTab[[iParameterTable]])
  }
  
  if (length(which(unique(table$ID %in% Table$ID[which(Table$ParameterType != "Table")])))) {
    stop("There are time profiles for ids which are no table parameters.")
  }
            
  #delete entries
  newEntries <- which(parameterTable$ID %in% table$ID)
  if (length(newEntries) == 0) {
    stop("No parameters would be effected!")
  }
  
  #delete current entries
  parameterTable <- parameterTable[-newEntries,]
  
  #sort new entries
  ordering <- order(table$ID, table$Time)
  sortedTable <- as.data.frame(table)[ordering,]
  
  #append new entries
  parameterTable <- rbind(parameterTable, sortedTable)

  #take over attributes
  parameterTable <- as.list(parameterTable)
  attributes(parameterTable$ID) <- attributes(table$ID)
  attributes(parameterTable$Time) <- attributes(table$Time)
  attributes(parameterTable$Value) <- attributes(table$Value)
  attributes(parameterTable$RestartSolver) <- attributes(table$RestartSolver)
  
  #write new table to info object
  if (toupper(options$Type) != "REFERENCE")
  {
    DCI_Info$InputTab[[iParameterTable]] <- parameterTable
  } else {
    DCI_Info$ReferenceTab[[iParameterTable]] <- parameterTable
  }		
  return(DCI_Info)
}

createTable = function(xVals, yVals, path_id, DCI_Info, restartSolver){
  ID = c();
  time = c();
  value = c();
  RestartSolver = c();
  
  #Check for existance of the provided parameter.
  if(!existsParameter(path_id = path_id, DCI_Info = DCI_Info)$isExisting){
    stop(paste("Parameter with path_id", path_id, "does not exist for specified type variable"));
  }
  path_id = existsParameter(path_id = path_id, DCI_Info = DCI_Info)$ID;
  
  nrOfEntries = length(xVals);
  #Fill the ID vector
  if (is.numeric(path_id)){
    ID = rep(path_id, nrOfEntries);
  }
  
  #Fill the time vector
  time = xVals;
  
  #Fill the value vector
  value = yVals;
  
  RestartSolver = restartSolver
  #Fill the restartSolver vector
  if (length(restartSolver) == 0){
    RestartSolver = rep(0, nrOfEntries);
  }
  
  table = list(ID = ID, Time = time, Value = value, RestartSolver = RestartSolver);
  return(table);
}
