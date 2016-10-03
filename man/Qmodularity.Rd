\name{Qmodularity}
\alias{Qmodularity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Qmodularity
}
\description{
This function computes the Q-modularity index for a given partition.
}
\usage{
Qmodularity(lma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{lma}{
list of three data.frames: clusterList, LWClus and marginals. See function findClusters.
}
}

\value{
numeric value, the Q-modularity index for the given partition.
}
\author{
Daniela Ichim, Luisa Franconi and Michele D'Alo' 
}
\seealso{
findClusters
}
\references{
[1] Y. Liu, Q. Liu, and Z. Qin, "Community Detecting and Feature
Analysis in Real Directed Weighted Social Networks",  Journal of Networks, vol. 8, no. 6, pp. 1432-1439, Jun. 2013.

}

