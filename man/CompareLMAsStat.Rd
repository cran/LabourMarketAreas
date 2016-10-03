\name{CompareLMAsStat}
\alias{CompareLMAsStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CompareLMAsStat
}
\description{
This function computes several statistics to be used for comparing partitions, e.g. for parameter selection.
}
\usage{
CompareLMAsStat(list.lma,dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{list.lma}{
list of LMAs to be compared. Each component of the list is a list of at least two components: lma and param (see the output of the function findCluster). The lma component is a list of three data.tables: clusterList, LWClus and marginals (the names should have not been assigned; otherwise use function DeleteLmaName). The param is a numeric vector containing the parameters of the corresponding LMAs, i.e. minSZ, minSC, tarSZ, tarSC.
}
\item{dat}{data frame/data.table with the original commuting flows between communities. It contains three columns: community_live, community_work and amount.}
}
\value{
A matrix containing the quality statistics computed by the StatClusterData function.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' 
}
\seealso{
findClusters, StatClusterData
}
