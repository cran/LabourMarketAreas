\name{getLeastSelfContained}
\alias{getLeastSelfContained}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getLeastSelfContained
}
\description{
This function determines the validity for all clusters as well as the minimum value i.e. the value for which the "X-equation" is minimal (see [1] and [2]).
This minimum corresponds to the selected cluster to be dissolved. 
If more than one cluster share the same minimum value the first will be chosen.
}
\usage{
getLeastSelfContained(LWClus, marginals, minSZ, minSC, tarSZ, tarSC)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LWClus}{
data table containing the flows between the clusters.
}
  \item{marginals}{
data table containing the structural characteristics of the clusters.
}
  \item{minSZ}{
numeric, parameter indicating the minimum size of the cluster in order to be an acceptable LMA.
}
  \item{minSC}{
numeric, parameter indicating the minimum self containment of the cluster in order to be an acceptable LMA.
}
  \item{tarSZ}{
numeric, parameter indicating the target size of the clusters.  
}
  \item{tarSC}{
numeric, parameter indicating the target self containment of the clusters.  
}
}

\value{ A list of two components:
\item{minivalout}{data.table containing the identifier of cluster attaining the minimum validity and its corresponding value.}
\item{LWSelf}{data.table containing the validity computations for each cluster.}
}
\references{
[1] Coombes, M. e Bond, S. (2008). Travel-to-Work Areas: the 2007 review. London: Office for National Statistics, 2008. 

[2] Franconi, L., D'Alo' M. and Ichim, D. (2016). Istat implementation of the  algorithm to develop Labour Market Areas.

}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' and Guido van den Heuvel 
}
\note{
This function is called and used internally by the main function findClusters. 
}
\seealso{
findClusters
}
