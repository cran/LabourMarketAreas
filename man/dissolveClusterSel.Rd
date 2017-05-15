\name{dissolveClusterSel}
\alias{dissolveClusterSel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dissolveClusterSel
}
\description{
This function dissolves a selected cluster into its constituent communities. Such communities are given temporary cluster IDs. 
Then clusterList is updated with the temporary IDs and the number of commuters (LWClus) and the cluster structure (marginals) are recomputed.
}
\usage{
dissolveClusterSel(clusterData, cluster, lwcom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusterData}{
list of 3 data.tables defining all the elements of the current set of clusters.
}
\item{cluster}{
integer: id of the selected cluster to be dissolved into its constituent communities.
}
\item{lwcom}{
data frame/data.table containing the selected commuting data. 
}
}

\value{
 \item{clusterData}{
list of 3 data tables defining all the elements of the current set of clusters. The ID includes negative values corresponding to the dissolved cluster.
}
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo and Guido van den Heuvel
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
