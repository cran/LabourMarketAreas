\name{shpSardinia}
\alias{shpSardinia}
\docType{data}
\title{shpSardinia

}
\description{
R object of class SpatialPolygonsDataFrame that holds polygons with attributes for each community in Sardinia island as from Italian Population Census 2001.    
}
\usage{data("shpSardinia")}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	377 obs. of  5 variables:
  .. ..$ PRO_COM : int [1:377] 90001 90002 90003 90004 90005 90006 90007 90008 90009 90010 ...
  .. ..$ COD_REG : int [1:377] 20 20 20 20 20 20 20 20 20 20 ...
  .. ..$ COD_PRO : int [1:377] 90 90 90 90 90 90 90 90 90 90 ...
  .. ..$ NOME_COM: chr [1:377] "AGGIUS" "ALA' DEI SARDI" "ALGHERO" "ANELA" ...
  .. ..$ POP2001 : int [1:377] 1686 1949 38404 817 847 10730 677 2181 3177 501 ...
  ..@ polygons   :List of 377
  .. ..
  ..............
  ..@ plotOrder  : int [1:377] 64 47 191 52 274 6 254 106 3 151 ...
  ..@ bbox       : num [1:2, 1:2] 8.13 38.86 9.83 41.27
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
}
\details{
The object contains 5 slots. The data slot presents the following variables (length 377, number of communities in Sardinia): PRO_COM integer containing the id code of the Sardinian community; COD_REG integer: code for the region Sardinia (equal to 20); COD_PRO integer: code for the provincie (NUTS3); NOME_COM: character containing the name of the community; POP2001: integer containg the population of the community as from 2001 Italian Population Census.
}
\source{
The data is taken from the shape files (in generalised form) of the whole of Italian municipalities at the 2001 Populalation Census published by Istat: http://www.istat.it/it/archivio/124086 
}
\examples{
data(shpSardinia)
plot(shpSardinia)
## maybe str(shpSardinia) ; ...
}
\keyword{datasets}
