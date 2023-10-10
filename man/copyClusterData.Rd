\name{copyClusterData}
\alias{copyClusterData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
copyClusterData
}
\description{
This function copies (in data.table parlance) the three components of lma, i.e. clusterList, LWClus and marginals. See function findClusters.
}
\usage{
copyClusterData(lma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{lma}{
list of three data.tables: clusterList, LWClus and marginals. See function findClusters.
}
}
\value{
list of three. It contains the three data tables clusterList, LWClus, marginals. They are copies of the input data.tables.
}
\author{
Daniela Ichim, Luisa Franconi
}
\seealso{
findClusters
}
