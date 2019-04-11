#' Creates physiology parameters of an mean individual with given characteristics.
#'
#' @param species (String) Human, Beagle, Dog, Minipig, Monkey, Mouse, Rat
#' @param population Either a string or an integer: European_ICRP_2002 = 0, WhiteAmerican_NHANES_1997 = 1, BlackAmerican_NHANES_1997 = 2
#' MexicanAmericanWhite_NHANES_1997 = 3, Asian_Tanaka_1996 = 4, Preterm = 5, Japanese_Population = 6. European_ICRP_2002 by default.
#' Only valid in combination with the species "Human"
#' @param gender Either a string or an integer: MALE = 1, FEMALE = 2
#' Only valid in combination with the species "Human"
#' @param age Age of the individual in years
#' @param weight Weight of the individual in kg
#' @param height Height of the individual in dm
#' @param BMI Body mass index of the individual in kg/dm^2. If weight and height are provided, a BMI value is calculated from those values and compared with the provided here. If the difference is bigger
#' than 0.005, an error is thrown.
#' @param DCI_Info If an initialized simulation is provided, surface area calculation method and ontogeny information will be extracted from the simulation.
#' If empty, default surface area calculation is applied and no ontogenies are generated
#' @param useDistribution FALSE by default
#' @param gestational_age Gestational age at birth in weeks. 40 by default
#' Only valid for the Preterm population
#'
#' @return A list containing generated parameters.
#' If 'useDistribution' is set to FALSE, the list contains the columns ParameterPath with the full path of the parameter, and ParameterValue with the values of the parameters

#' @export
#'
#' @examples
PKSimCreateIndividual = function(species = "", population = "", gender = 3, age, weight = NA, height = NA, BMI = NA,
                                 DCI_Info = {}, useDistribution = FALSE, gestational_age = 40){
  require(rDotNet);
  
  exceptionString = "PKSimCreateIndividual:"
  
  #List of accepted species
  speciesList = c("Beagle", "Dog", "Human", "Minipig", "Monkey", "Mouse", "Rat", "Rabbit");
  #List of accepted populations
  populationList = c("European_ICRP_2002", "WhiteAmerican_NHANES_1997", "BlackAmerican_NHANES_1997",
                      "MexicanAmericanWhite_NHANES_1997", "Asian_Tanaka_1996", "Preterm", "Japanese_Population", "Pregnant");
  #List of accepted gender values
  genderList = c("MALE", "FEMALE", "UNKNOWN");
  
  #Default endothelial surface area calculation method.
  endothelialSurfaceAreaCalculationMethod = "SurfaceAreaPlsInt_VAR1";
  #Empty ontogenies by default
  ontoPath = c();
  
  #Check for correct value of "useDistribution"
  if (!(useDistribution == FALSE || useDistribution == TRUE)){
    stop(paste(exceptionString, "invalid value for parameter 'useDistribution':", useDistribution, "! Only values TRUE and FALSE are accepted."));
  }
  
  #Check for correct species
  if (is.na(match(species, speciesList))){
    stop(paste(exceptionString, "invalid value for parameter 'species':", species));
  }
  
  #Check for correct population
  #Population only matter for the human species
  if (species == "Human"){
    #If no population is defined, use the first population in the populationList as default.
    if (population == ""){
      population = populationList[1];
    }
    #if population is given by an integer
    else if (is.numeric(population)){
      #Check for correct boundaries
      if (population < 0 && population > 7){
        stop(paste(exceptionString, "invalid value for parameter 'population':", population, "please provide either a string representation of the population or a number between 0 and 7."));
      }
      population = populationList[population + 1];
    }
    #Population is given by its string representation
    else{
      if (is.na(match(population, populationList))){
        stop(paste(exceptionString, "invalid value for parameter 'population':", population, "please provide either a string representation of the population or a number between 0 and 7."));
      }
    }
  }
  #If species is other than human, use species' name as population name.
  else{
    population = species;
    gender = "UNKNOWN";
  }
  
  #Check for correct gender.
  if (is.numeric(gender)){
    if (gender < 1 && gender > 3){
      stop(paste(exceptionString, "invalid value for parameter 'gender':", gender, "please provide either a stirng representation of the population or a number between 1 and 3."));
    }
    gender = genderList[gender];
  }
  else{
    if (is.na(match(gender, genderList))){
      stop(paste(exceptionString, "invalid value for parameter 'gender':", gender, "please provide either a stirng representation of the population or a number between 1 and 3."));
    }
  }
  
  #Check if age is a numeric value
  if (!is.numeric(age)){
    stop(paste(exceptionString, "invalid value for parameter 'age':", age, "please provide a number."));
  }
  
  #Check for correct height, weight, and BMI inputs
  if (sum(is.na(c(weight, height, BMI))) > 1){
    stop(paste(exceptionString, "Please provide at least 2 out of the parameters height, weight, and BMI!"));
  }
  if (sum(is.na(c(weight, height, BMI))) == 0){
    BMI_calc = weight / (height^2);
    if (abs(BMI_calc - BMI) > 0.005){
      stop(paste(exceptionString, "The combination of provided weight, height, and BMI are inconsisten! Calculated BMI:", BMI_calc,
                 "One of the values of weight, height, or BMI can be left empty and will be calculated automatically."));
    }
  }
  else if (is.na(height)){
    height = sqrt(weight / BMI);
  }
  else if (is.na(weight)){
    weight = BMI * (height^2);
  }
  else if (is.na(BMI)){
    BMI = weight / (height^2);
  }
  
  #If a simulation is provided, retrieve endothelial surface area calculation methods and ontogenies info.
  #WILL NOT WORK AS LONG "IsFormula" BIG IS NOT FIXED!
  #DON'T LIKE THIS HARD-CODED PART!
  if (length(DCI_Info) != 0){
    path_id = "*|Neighborhoods|*_pls_*_int|Surface area (plasma/interstitial)";
    if (existsParameter(path_id = path_id, options = list(Type = "readonly"), DCI_Info = DCI_Info)$isExisting){
      params = getParameter(path_id = path_id, options = list(Type = "readonly", Property = "Formula"), DCI_Info = DCI_Info);
      endoFormula = params$Value[1];
      if (endoFormula == "k * f_vas_org * V_org"){
        endothelialSurfaceAreaCalculationMethod = "SurfaceAreaPlsInt_VAR1";
      }
      else if (endoFormula == "k * Q_org ^ beta"){
        endothelialSurfaceAreaCalculationMethod = "SurfaceAreaPlsInt_VAR3";
      }
    }
    
    #TO DO
    path_id = "*Ontogeny factor GI";
    if (existsParameter(path_id = path_id, options = list(Type = "readonly"), DCI_Info = DCI_Info)$isExisting){
      
    }
  }
  
  #Create individual parameters
  loadPKSimMatlabDLL();
  
  paramPaths = c();
  paramValues = c();
  #For distributed parameters only
  paramMeans = c();
  paramStds = c();
  paramDistributionTypes = c();
  
  IndividualFactory = .cnew("PKSim.Matlab.MatlabIndividualFactory");
  
  #Create and fill OriginData
  OriginData = .cnew("PKSim.Core.Snapshots.OriginData");
  
  OriginData$Set("Species", species);
  OriginData$Set("Population", population);
  OriginData$Set("Gender", gender);
  paramAge =  parameterFrom(age, 'year(s)');
  OriginData$Set("Age", paramAge);
  paramAge = NULL;
  paramGestationalAge = parameterFrom(gestational_age, 'week(s)');
  OriginData$Set("GestationalAge", paramGestationalAge);
  paramGestationalAge = NULL;
  paramWeight = parameterFrom(weight, 'kg');
  OriginData$Set("Weight", paramWeight);
  paramWeight = NULL;
  paramHeight = parameterFrom(height, 'dm');
  OriginData$Set("Height", paramHeight);
  paramHeight = NULL;
  calculationMethods = endothelialSurfaceAreaCalculationMethod;
  OriginData$AddCalculationMethods(endothelialSurfaceAreaCalculationMethod);
  
  if (useDistribution){
    if (length(ontoPath) == 0){
      netParams = IndividualFactory$DistributionsFor(OriginData);
    }
    else{
      netParams = IndividualFactory$DistributionsFor(OriginData, ontoPath);
    }
    for (netParam in netParams){
      paramPaths = c(paramPaths, netParam$Get("ParameterPath"));
      paramValues = c(paramValues, netParam$Get("Value"));
      paramMeans = c(paramMeans, netParam$Get("Mean"));
      paramStds = c(paramStds, netParam$Get("Std"));
      paramDistributionTypes = c(paramDistributionTypes, netParam$Get("DistributionType")$Get("DisplayName"));
    }
    outputList = list(ParameterPath = paramPaths, Parametervalue = paramValues, ParameterMean = paramMeans, ParameterStd = paramStds, ParameterDistributionType = paramDistributionTypes);
  }
  else{
    if (length(ontoPath) == 0){
      netParams = IndividualFactory$CreateIndividual(OriginData);
    }
    else{
      netParams = IndividualFactory$CreateIndividual(OriginData, ontoPath);
    }
    
    for (netParam in netParams){
      paramPaths = c(paramPaths, netParam$Get("ParameterPath"));
      paramValues = c(paramValues, netParam$Get("Value"));
    }
    outputList = list(ParameterPath = paramPaths, Parametervalue = paramValues);
  }
  
  IndividualFactory = NULL;
  
  return(outputList);
}

loadPKSimMatlabDLL = function(){
  PKSimDir = getPathToPKSimInstallDir();
  .cinit(dll = c(paste0(PKSimDir, "/PKSim.Matlab.dll"), paste0(PKSimDir, "/PKSim.Core.dll")));
}

parameterFrom = function(value, unit){
  parameter = .cnew("PKSim.Core.Snapshots.Parameter");
  parameter$Set("Value", value);
  parameter$Set("Unit", unit);
  return(parameter);
}
