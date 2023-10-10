\name{shpBrindisi}
\alias{shpBrindisi}
\docType{data}
\title{
shpBrindisi
}
\description{
object of class sf that holds polygons with attributes for each community in Brindisi provincie as from Italian Population Census 2001.
}
\usage{data("shpBrindisi")}
\source{
The data is taken from the shape files (in generalised form) of the whole of Italian municipalities at the 2001 Populalation Census published by Istat: http://www.istat.it/it/archivio/124086 
}
\examples{
#library(data.table)
\dontrun{
data(shpBrindisi)
plot(shpBrindisi)
## maybe str(shpBrindisi) ;  ...
}}
\keyword{datasets}
