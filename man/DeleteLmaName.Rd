\name{DeleteLmaName}
\alias{DeleteLmaName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DeleteLmaName
}
\description{
This function deletes the variables corresponding to names of communities and clusters.
}
\usage{
DeleteLmaName(lma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{lma}{
list of three data.tables: clusterList, LWClus and marginals. See function findClusters.
}
}
\value{
list of three. It contains the three data.tables clusterList, LWClus, marginals. From clusterList the deleted variables are com.name, lma.name, from LWCom the deleted variables are lma.name.live, lma.name.work  and from marginals the deleted variable is lma.name.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo'
}
\seealso{
findClusters
}
