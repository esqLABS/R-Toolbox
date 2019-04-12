#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")
xVals = c(1,2,3);
yVals = c(4,5,6);
defPathID = 5738;


test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(setTableParameter(xVals = xVals, yVals = yVals, path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(setTableParameter(xVals = xVals, yVals = yVals, path_id = defPathID, DCI_Info = {}))
}

test.CheckTypes <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id = defPathID)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)

  options <- list(Type="readonly")
  checkException(setTableParameter(xVals = xVals, yVals = yVals, path_id = defPathID, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  checkException(setTableParameter(xVals = xVals, yVals = yVals, path_id = defPathID, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getTableParameter(path_id=defPathID, DCI_Info = dci_info, options=options)
  dci_info_check <- setTableParameter(xVals = parameter$Time, yVals = parameter$Value, path_id = defPathID, DCI_Info = dci_info, options=options)
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == defPathID)
  idx2 <- which(parameter$ID == defPathID)
  checkEquals(dci_info_check$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info_check$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info_check$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info_check$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="reference")
  parameter <- getTableParameter(path_id=defPathID, DCI_Info = dci_info, options=options)
  dci_info_check <- setTableParameter(xVals = parameter$Time, yVals = parameter$Value, path_id = defPathID, DCI_Info = dci_info, options=options)
  idx <- which(dci_info$ReferenceTab$VariableTableParameters$ID == defPathID)
  idx2 <- which(parameter$ID == defPathID)
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}

#Test for new values applied in the simulation
test.CheckNewValuesSameSize = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);

  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  new_time = c(0, 30, 250);
  new_values = c(2, 1.5, 7);
  #Update the table of a parameter
  myDCI = setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI);
  
  #Simulate with new table values
  myDCI = processSimulation(DCI_Info = myDCI);

  #Check the new table parameter
  tableParam_new = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  checkEqualsNumeric(tableParam_new$Time, new_time);
  checkEqualsNumeric(tableParam_new$Value, new_values);
}

#Test for new table being shorter than the original
test.CheckNewValuesShorter = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  new_time = c(0, 30);
  new_values = c(2, 1.5);
  #Update the table of a parameter
  myDCI = setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI);
  
  #Simulate with new table values
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #Check the new table parameter
  tableParam_new = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  checkEqualsNumeric(tableParam_new$Time, new_time);
  checkEqualsNumeric(tableParam_new$Value, new_values);
}

#Test for new table being larger than the original
test.CheckNewValuesLarger = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  new_time = c(0, 30, 250, 500);
  new_values = c(2, 1.5, 5, 8);
  #Update the table of a parameter
  myDCI = setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI);
  
  #Simulate with new table values
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #Check the new table parameter
  tableParam_new = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  checkEqualsNumeric(tableParam_new$Time, new_time);
  checkEqualsNumeric(tableParam_new$Value, new_values);
}

test.CheckLengthXValsYValsDiffer = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  new_time = c(0, 30, 250);
  new_values = c(2, 1.5, 5, 8);
  #Update the table of a parameter
  checkException(setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI));
}
test.YXValsNonNumeric = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  new_time = c(0, "30", 250, 500);
  new_values = c(2, 1.5, 5, 8);
  #Update the table of a parameter
  checkException(setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI));
  
  #Change the table
  new_time = c(0, 30, 250, 500);
  new_values = c(2, "1.5", 5, 8);
  #Update the table of a parameter
  checkException(setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI));
}
test.RestartSolverNonNumeric = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);
  
  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  new_time = c(0, 30, 250, 500);
  new_values = c(2, 1.5, 5, 8);
  restartSolver = c(0, "FALSE", 0, 0);
  #Update the table of a parameter
  checkException(setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI, restartSolver = restartSolver));
}