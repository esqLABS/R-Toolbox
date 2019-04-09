getPathToPKSimInstallDir = function(){
  OSPSuiteVersion = 7.4;
  SuiteInstallationSubfolder = "Open Systems Pharmacology";
  
  #Try to get the path from windows registry.
  pathToPKSimInstallDir = getpathToPKSimInstallDirFromRegistry(OSPSuiteVersion);
  #If failed, find the default installation directory
  if (pathToPKSimInstallDir == ""){
    pathToPKSimInstallDir = getpathToPKSimInstallDirFromFileSystem(OSPSuiteVersion, SuiteInstallationSubfolder);
  }
  
  return(pathToPKSimInstallDir);
}

#Get the path from windows registry.
getpathToPKSimInstallDirFromRegistry = function(OSPSuiteVersion){
  pathToPKSimInstallDir = "";
  #Only of running in windows environment.
  if (.Platform$OS.type == "windows"){
    regEntries = readRegistry(paste0("SOFTWARE\\Open Systems Pharmacology\\PK-Sim\\", OSPSuiteVersion), "HLM");
    pathToPKSimInstallDir = regEntries$InstallDir;
  }
  
  return(pathToPKSimInstallDir);
}

getpathToPKSimInstallDirFromFileSystem = function(OSPSuiteVersion, SuiteInstallationSubfolder){
  pathToPKSimInstallDir = "";
  tmpDir = file.path(Sys.getenv("ProgramFiles"), SuiteInstallationSubfolder, "PK-Sim");
  if (dir.exists(tmpDir)){
    pathToPKSimInstallDir = tmpDir;
  }
  
  return(pathToPKSimInstallDir);
}