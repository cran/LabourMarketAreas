\name{AddStatistics}
\alias{AddStatistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AddStatistics
%%  ~~function to do ... ~~
}
\description{
Function to evaluate statistics at LMA level based on data at community level.
}
\usage{
AddStatistics(statData, comID.file, lma, comID.lma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statData}{
data.frame or data.table containing the ID of the communities and the numerical variables to be summed at LMA level.
}
  \item{comID.file}{
character: name of the variable containing the community ID in the statData object.
}
  \item{lma}{
list of three data.tables: clusterList, LWClus and marginals. See function findClusters.
}
  \item{comID.lma}{
character: name of the variable containing the community ID in the lma object.
}
}
\details{
This function can be used in general to compute several statistics at LMA level provided data at community level is provided. This function sums the values at community level to obtain the corresponding value at LMA level.
}
\value{
data.table containing the LMA ID and the summed numerical variables from statData. 
}

\author{
Daniela Ichim, Luisa Franconi
}
\examples{
# compute population totals at LMA level from population values at community level.
\dontrun{
out<- findClusters(LWCom=Brindisi, minSZ=1000,minSC=0.6667,tarSZ=10000,tarSC=0.75, 
verbose=TRUE)
AddStatistics(shpBrindisi[,c("PRO_COM","POP2001")], "PRO_COM",out$lma,"community" )
}
}

