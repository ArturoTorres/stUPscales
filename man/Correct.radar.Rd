\name{Correct.radar}
\alias{Correct.radar}

\title{
Correction of precipitation radar imagery from the German Weather Service (DWD)
}
\description{
Given a set of calibrated radar imagery for precipitation (RW product, one hour temporal
resolution) in format .tif, this function corrects precipitation radar imagery 
non-calibrated (RY product, five minutes temporal resolution) in format .tif for the 
same spatial extend. 
This function aggregates temporally the non-calibrated data to 10 minutes and makes 
a simple temporal linear disaggregation of the calibrated radar data to match the required temporal resolution of the non-calibrated data (10 minutes in this case). 
It is specifically designed for the radar imagery from the
German Weather Service (DWD).
}
\usage{Correct.radar(path.calibrated, path.non.calibrated, time.ini.calib,
                              format = "\%y\%m\%d\%H\%M", n, NAcutoff = 250)}

\arguments{
  \item{path.calibrated}{
    A \code{character}, which defines the path to the folder which contains the calibrated
    imagery (RW product, one hour temporal resolution) in .tif format.}
  \item{path.non.calibrated}{
    A \code{character}, path to the folder which contains the non-calibrated imagery 
    (RY product, five minutes temporal resolution) in .tif format.}
  \item{time.ini.calib}{
    A \code{character}, initial time for calibration e.g. "1112160055".}
  \item{format}{
    A \code{character}, initial time format e.g. "\%y\%m\%d\%H\%M".}
  \item{n}{
    A \code{numeric}, number of images to process including time.ini.calib.}
  \item{NAcutoff}{
    A \code{numeric}, cutoff value for NA assignation in individual image (file).}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\value{
  A \code{STFDF} which contains the corrected data.
}
%\references{
  %% ~put references to the literature/web site here ~
    %}
\author{
  J.A. Torres-Matallana
}
%\note{
%  
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
%\seealso{
%    See also \code{\link{setup-class}}.
%}
  
%\source{
%\url{https://www.dwd.de/}
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

%\examples{
%data(Correct.radar)
%## maybe str(Correct.radar) ; plot(Correct.radar) ...
%}

\keyword{Radar}