\name{shpBrindisi}
\alias{shpBrindisi}
\docType{data}
\title{
Polygons (shape files) information for communities in Brindisi
}
\description{
R object of class SpatialPolygonsDataFrame that holds polygons with attributes for each community in Brindisi provincie as from Italian Population Census 2001.
}
\usage{data("shpBrindisi")}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	20 obs. of  5 variables:
  .. ..$ PRO_COM : int [1:20] 74001 74002 74003 74004 74005 74006 74007 74008 74009 74010 ...
  .. ..$ COD_REG : int [1:20] 16 16 16 16 16 16 16 16 16 16 ...
  .. ..$ COD_PRO : int [1:20] 74 74 74 74 74 74 74 74 74 74 ...
  .. ..$ NOME_COM: Factor w/ 8092 levels "ABANO TERME",..: 924 1402 1885 1904 2138 2645 2716 2918 3493 4029 ...
  .. ..$ POP2001 : int [1:20] 89081 14960 21370 6818 12078 8740 38667 36274 15371 27587 ...
  ..@ polygons   :List of 20
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .................
  ..@ plotOrder  : int [1:20] 1 12 8 3 7 10 2 11 17 15 ...
  ..@ bbox       : num [1:2, 1:2] 17.3 40.4 18.1 40.9
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
}
\details{
The object contains 5 slots. The data slot presents the following variables (length 20, number of communities in Brindisi NUTS3 region): PRO_COM integer containing the id code of the community; COD_REG integer: code for the region (equal to 16); COD_PRO integer: code for the provincie (NUTS3); NOME_COM: character containing the name of the community; POP2001: integer containg the population of the community as from 2001 Italian Population Census.
}
\source{
The data is taken from the shape files (in generalised form) of the whole of Italian municipalities at the 2001 Populalation Census published by Istat: http://www.istat.it/it/archivio/124086 
}
\examples{
data(shpBrindisi)
plot(shpBrindisi)
## maybe str(shpBrindisi) ;  ...
}
\keyword{datasets}
