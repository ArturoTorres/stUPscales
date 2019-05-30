\name{Smooth.ts}
\alias{Smooth.ts}

\title{
Kernel smoothing for time series
}
\description{
Given one or more time series in xts format, this function applies a Daniel kernel function
for smoothing the time series.
}

\usage{Smooth.ts(data.xts, kernel = kernel("daniell", c(1, 1)), sm.threshold = 1)}

\arguments{
  \item{data.xts}{
    \code{xts} object with the time series to smooth.}
  \item{kernel}{
    \code{tskernel} object which defines the smoothing kernel to apply.}
  \item{sm.threshold}{
    \code{numeric}, defines the threshold value to apply the Kernel smoothing.
    Only values lower than sm.threshol are smoothed.}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%}

\value{
  A \code{xts} object with the time series smoothed.
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
  
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\examples{
data("Lux_precipitation_2010_2011")

data.xts <- Lux_precipitation_2010_2011

my.smoothed.ts <- Smooth.ts(data.xts, kernel = kernel("daniell", c(1, 1)), 
                            sm.threshold = 1)

# plot
par(mfrow=c(2,1))
plot(data.xts['2011-06-20 14:00/2011-06-22 16:00'])
plot(my.smoothed.ts['2011-06-20 14:00/2011-06-22 16:00'])
}

\keyword{Time series}