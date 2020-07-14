\name{Goe_catchment}
\alias{Goe_catchment}
\docType{data}
\title{
A SpatialPolygonsDataFrame for the boundaries of the catchment Goesdorf (Goe)
}
\description{
A SpatialPolygonsDataFrame for the boundaries of the catchment Goesdorf (Goe), located in the 
catchment Haute-Sûre in the North-West of the Grand-Duchy of Luxembourg.
}
\usage{data("Goe_catchment")}
\format{
  The format is: \cr
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots \cr
  ..@ data       :'data.frame':	10 obs. of  3 variables: \cr
  .. ..$ cat  : int [1:10] 2 1 3 4 6 7 5 8 9 10 \cr
  .. ..$ value: int [1:10] 14 10 16 12 18 8 6 4 8 2 \cr
  .. ..$ label: Factor w/ 0 levels: NA NA NA NA NA NA NA NA NA NA \cr
  ..@ polygons   :List of 10 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 64972 110572 \cr
  .. .. .. .. .. .. ..@ area   : num 334500 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:332, 1:2] 64570 64645 64645 64650 64650 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 64972 110572 \cr
  .. .. .. ..@ ID       : chr "0" \cr
  .. .. .. ..@ area     : num 334500 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 63815 110371 \cr
  .. .. .. .. .. .. ..@ area   : num 769525 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:453, 1:2] 64570 64570 64575 64575 64580 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 63815 110371 \cr
  .. .. .. ..@ ID       : chr "1" \cr
  .. .. .. ..@ area     : num 769525 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 65293 110235 \cr
  .. .. .. .. .. .. ..@ area   : num 378100 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:410, 1:2] 64840 64840 64835 64835 64840 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 65293 110235 \cr
  .. .. .. ..@ ID       : chr "2" \cr
  .. .. .. ..@ area     : num 378100 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 64435 109996 \cr
  .. .. .. .. .. .. ..@ area   : num 516350 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:557, 1:2] 64585 64585 64590 64590 64595 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 64435 109996 \cr
  .. .. .. ..@ ID       : chr "3" \cr
  .. .. .. ..@ area     : num 516350 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 64962 109448 \cr
  .. .. .. .. .. .. ..@ area   : num 922000 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:613, 1:2] 65100 65110 65110 65115 65115 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 64962 109448 \cr
  .. .. .. ..@ ID       : chr "4" \cr
  .. .. .. ..@ area     : num 922000 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 63795 109872 \cr
  .. .. .. .. .. .. ..@ area   : num 16075 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:132, 1:2] 63660 63660 63665 63665 63670 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 63795 109872 \cr
  .. .. .. ..@ ID       : chr "5" \cr
  .. .. .. ..@ area     : num 16075 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 63309 109732 \cr
  .. .. .. .. .. .. ..@ area   : num 334700 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:224, 1:2] 63115 63125 63125 63130 63130 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 63309 109732 \cr
  .. .. .. ..@ ID       : chr "6" \cr
  .. .. .. ..@ area     : num 334700 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 64030 109425 \cr
  .. .. .. .. .. .. ..@ area   : num 589825 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:422, 1:2] 63785 63790 63790 63795 63795 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 64030 109425 \cr
  .. .. .. ..@ ID       : chr "7" \cr
  .. .. .. ..@ area     : num 589825 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 63952 109802 \cr
  .. .. .. .. .. .. ..@ area   : num 25 \cr 
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 63950 63955 63955 63950 63950 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 63952 109802 \cr
  .. .. .. ..@ ID       : chr "8" \cr
  .. .. .. ..@ area     : num 25 \cr
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots \cr
  .. .. .. ..@ Polygons :List of 1 \cr
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots \cr
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 64458 108753 \cr
  .. .. .. .. .. .. ..@ area   : num 356625 \cr
  .. .. .. .. .. .. ..@ hole   : logi FALSE \cr
  .. .. .. .. .. .. ..@ ringDir: int 1 \cr
  .. .. .. .. .. .. ..@ coords : num [1:211, 1:2] 63925 63935 63935 63950 63950 ... \cr
  .. .. .. ..@ plotOrder: int 1 \cr
  .. .. .. ..@ labpt    : num [1:2] 64458 108753 \cr
  .. .. .. ..@ ID       : chr "9" \cr
  .. .. .. ..@ area     : num 356625 \cr
  ..@ plotOrder  : int [1:10] 5 2 8 4 3 10 7 1 6 9 \cr
  ..@ bbox       : num [1:2, 1:2] 62935 108355 65735 111040 \cr
  .. ..- attr(*, "dimnames")=List of 2 \cr
  .. .. ..$ : chr [1:2] "x" "y" \cr
  .. .. ..$ : chr [1:2] "min" "max" \cr
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot \cr
  .. .. ..@ projargs: chr "+proj=tmerc +lat_0=49.83333333333334 +lon_0=6.166666666666667  \cr
  +k=1 +x_0=80000 +y_0=100000 +ellps=intl +units=m +no_defs" \cr
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Goe_catchment)
str(Goe_catchment) 
plot(Goe_catchment)
}
\keyword{datasets}
