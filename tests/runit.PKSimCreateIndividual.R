#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)

#Fails if no species provided.
test.speciesEmpty = function(){
  checkException(PKSimCreateIndividual(population = 1, gender = 1, age = 30, weight = 70, height = 16.8));
}

#Fails if species provided is unknown.
test.speciesUnknown = function(){
  checkException(PKSimCreateIndividual(species = "Unknown", population = 1, gender = 1, age= 30, weight = 70, height = 16.8));
  checkException(PKSimCreateIndividual(species = 1, population = 1, gender = 1, age= 30, weight = 70, height = 16.8));
}

#Fails if population provided is unknown.
test.populationUnknown = function(){
  checkException(PKSimCreateIndividual(species = "Human", population = "Unknown", gender = 1, age= 30, weight = 70, height = 16.8));
}

test.populationHuman = function(){
  #population unknown - error
  #population empty - first population
  #population known - provided value
  #population numeric - check boundaries, <0 and > 7
}

#If the species is not a human, population should not matter. Emtpy or wrong entries should be allowed.
test.popullationNonHuman = function(){
}

#Fails if no gender provided.
test.genderEmpty = function(){
}

#Fails if gender provided is unknown.
test.genderUnknown = function(){
  #Population human, gender unknown - what happens?
}

#Fails if no age provided.
test.ageEmpty = function(){
}

test.ageNonNumeric = function(){
}

#Fails if age provided is unknown.
test.ageUnknown = function(){
}

#Test for valid Weigh/Height/BMI combinations
test.weight_height_BMI = function(){
  #BMI calculated differs from provided
  #<2 provided
  #BMI and height
  #BMI and weight
  #height and weight
}

#Test when no DCI is provided
test.noDCI = function(){
}

test.DCI = function(){
}

#Check for valid useDistribution values
test.useDistributionInput = function(){
}

test.creatIndividualUseDistributionTrue = function(){
}

test.creatIndividualUseDistributionFalse = function(){
}

test.rDotNet_notInstalled = function(){
}

test.gestationalAge = function(){
}

#Test with a simulation that has the parameter "Ontogeny factor GI".
test.DCI_WithOnto = function(){
}