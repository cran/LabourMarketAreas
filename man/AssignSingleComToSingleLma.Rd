\name{AssignSingleComToSingleLma}
\alias{AssignSingleComToSingleLma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AssignSingleComToSingleLma
}
\description{
This function assigns a community to a given labour market area. It simulates a manual assignment. It might be used inside the fine tuning process.
}
\usage{
AssignSingleComToSingleLma(lma,comID,lmaID,dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lma}{
The partition to be modified. A list of three components named:  clusterList, LWClus and marginals, respectively. See the output of the findClusters function.
}
 \item{comID}{
The identifier of the community to be assigned.
}
  \item{lmaID}{
The identifier of the  labour market area where comID has to be assigned.
}
 \item{dat}{
The initial commuting matrix between communities. A data.table containing the variables community_live, community_work and amount.
}
}
\value{
The new labour market partition. A list of three components named:
  \item{clusterList }{A data.table with three variables: community, cluster, residents}
  \item{LWClus }{A data.table with three variables: cluster_live, cluster_work, amount}
  \item{marginals }{A data.table with three variables: cluster, amount_live, amount_work}
}
\author{
Daniela Ichim, Luisa Franconi and Michele D'Alo' 
}
\note{
This function can be called before the call to the function AssignLmaName.
}
\seealso{
findClusters
}
