\name{determineRegroupList}
\alias{determineRegroupList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
determineRegroupList
}
\description{
This function determines, for each cluster in LWClus, the cluster with which that cluster is most closely associated. First the cohesion between all pairs of clusters is calculated; then the cohesion is sorted for each cluster and only the last (i.e., highest) one is selected.
}
\usage{
determineRegroupList(LWClus, marginals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LWClus}{
data table containing the flows between the clusters.
}
  \item{marginals}{
data table containing the main characteristics of the cluster.
}
}
\details{
This function determines, for each pair of cluster in LWClus,  the cohesion; then it is sorted and only the last (i.e., highest) one is selected. The highest value it associated with the dominant cluster for the dissolved cluster.
}
\value{
Integer: the candidate cluster identifier.
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
\references{
[1] Franconi, L., D'Alo' M. and Ichim, D. (2016). Istat implementation of the  algorithm to develop Labour Market Areas.

}