\name{IsReg.files}
\alias{IsReg.files}

\title{
Check regulatiry of time series in a folder
}
\description{
This function checks the regulatiry of the time series which are contained in files 
in a specific folder.
}

\usage{IsReg.files(path, sep = "-", pattern = ".tif", format = "\%y\%m\%d\%H\%M")}

\arguments{
  \item{path}{
    \code{character}, path to the folder which contains the files with the imagery.}
  \item{sep}{
    \code{character}, separation character for spliting the file names (e.g. "-").}
  \item{pattern}{
    \code{character}, extension of the files to check (e.g. .tif).}
  \item{format}{
    \code{character}, time format in the file name (e.g. "\%y\%m\%d\%H\%M").}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\value{Object of class \code{"list"}. This object contains 2 elements, 
    the first one contains a character string "_TSregular" if the xts object created is strict regular, 
    or "_TSirregular" if it is strict irregular. More details can be found in the "is.regular" function of the
    "zoo" package.}
    
%\references{
  %% ~put references to the literature/web site here ~
    %}
\author{
  J.A. Torres-Matallana
}
%\note{
%  
%}

\section{Warning }{This function was specifically created for file names following the
naming nomenclature of the German Weather Service (DWD) for radar imagery 
e.g. "raa01-rw_10000-1112160050-dwd---bin.tif" for RW products or 
"raa01-ry_10000-1112160000-dwd---bin.tif" for RY products.}
  
\seealso{
    See also \code{\link{IsReg.ts}}.}
  
%\source{
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

%\examples{
%%%
%}


\keyword{Time series}