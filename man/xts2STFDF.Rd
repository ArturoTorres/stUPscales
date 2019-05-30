\name{xts2STFDF}
\alias{xts2STFDF}

\title{
Creation of STFDF object from xts and SpatialPointsDataFrame objects
}
\description{
Given one or more time series as an xts object from xts packakge and its associated 
geospatial domain represented as a SpatialPointsDataFrame from package sp, this function
creates a spacetime full data.frame (STFDF) object as defined by package spacetime.
}

\usage{xts2STFDF(data.xts, point)}

\arguments{
  \item{data.xts}{
    \code{xts} object which defines the time series.}
  \item{point}{
    \code{SpatialPointsDataFrame}, point geospatial domain of data.xts.}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\value{
  A \code{STFDF} object.
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
%    See also \code{\link{setup-class}}}.
%}
  
%\source{
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\examples{
data("Lux_stations")

data("Lux_precipitation_2010_2011")
data.xts <- Lux_precipitation_2010_2011

my.stfdf <- xts2STFDF(data.xts, point = stations)

library(spacetime)
stplot(my.stfdf[,1:16,"data"])
stplot(my.stfdf[,"2011-12-16", drop = FALSE], mode = "ts")

data("Lux_boundary")
library(sp)
spplot(my.stfdf@sp, "elev_luref", 
       sp.layout=list(sp.polygons = boundary.Lux),
       scales=list(arrows=FALSE),
       xlim=bbox(boundary.Lux)[1,],
       ylim=bbox(boundary.Lux)[2,],
       key.space = "right")
}


\keyword{Utilities}