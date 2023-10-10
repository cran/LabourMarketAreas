\name{mergeCluster}
\alias{mergeCluster}
\title{
mergeCluster
}
\description{
This function updates clusterData by merging cluster1 and cluster2. 
}
\usage{
mergeCluster(clusterData, cluster1, cluster2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusterData}{
List (three components) containing all defining information on the current clusters found by the algorithm.
}
  \item{cluster1}{
Cluster ID to be merged. An unique positive integer.
}
  \item{cluster2}{
cluster ID to be merged. An unique positive integer.
}
}
\value{
A cluster data object, i.e. a list of three components: 
\item{ClusterList }{data.table of three columns: community, cluster, residents}
\item{LWClus }{data.table of three columns; cluster_live, cluster_work and amount}
\item{marginals }{data.table of three columns: cluster, amount_live, amount_work}
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo', Guido van den Heuvel
}
\note{
This function is called and used internally by the main function findClusters. 
}
\seealso{
findClusters
}

