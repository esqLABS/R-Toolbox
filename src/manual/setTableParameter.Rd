\name{setTableParameter}
\alias{setTableParameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets Time Profiles for a Table Parameter
}
\description{
This function sets time profiles for one table parameter.
}
\usage{
setTableParameter(xVals = {}, yVals = {}, path_id = "*", options = {}, DCI_Info = {}, restartSolver = c{})
}
\arguments{
  \item{xVals}{
Numerical vector with the time values of the table parameter.
}

\item{yVals}{
Numerical vector with the y-values of the table parameter.
}

\item{restartSolver}{
Numerical vector containing information about whether the solver should be restarted at the respective timepoint. 0 for FALSE (default for each point), 1 for TRUE.
}

  \item{path_id}{
The path_id can be "*" or a string or a numerical vector. 
If a string is provided the parameters which pathes do match the string are set. A wildcard ("*") can be used at any place.
If a numerical vector is provided all parameters which id is in the vector are set.
}
  \item{options}{
Following options are supported: 

Type ("variable", "reference"). Default is "variable".

}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
The DCI Info structure containing the DCI handle and all settings with the newly setted table parameters.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getTableParameter}}
}
\examples{
# sets the time profiles of parameter with ID 5837.
  new_time = c(0, 30, 250, 500);
  new_values = c(2, 1.5, 5, 8);
  restartSolver = c(0, "FALSE", 0, 0);
  #Update the table of a parameter
  checkException(setTableParameter(xVals = new_time, yVals = new_values, path_id = paramPath, DCI_Info = myDCI, restartSolver = restartSolver));
dci_info <- setTableParameter(table = timeProfiles, 
							  options=list(Type="variable"), 
							  DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
