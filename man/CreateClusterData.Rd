\name{CreateClusterData}
\alias{CreateClusterData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CreateClusterData
}
\description{
This function creates a labour market areas structure, i.e. a list of three data.tables: clusterList, LWClus and marginals given a set of commuting data.
}
\usage{
CreateClusterData(LWCom,residents=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LWCom}{
input data frame/data.table of commuting data (see for example Sardinia).
}
\item{residents}{
A data.table with two columns: Code and residents. Code is the community Id while residents represents the amount of occupied persons living in the community. If NULL, the residents data.table is computed using the commuting flows dataset. The missing values are not allowed: communities with no residents present zero values.
}
}
\details{
If needed, the fictitious community (see Franconi, D'Alo' and Ichim, 2016 for definition and details) should be a-priori included in both residents and commuting flows datasets.
}
\value{
A list of three components:
\item{clusterList:}{data.table with three variables: community, cluster and residents. Each community is assigned to a cluster. The clusters ID is a numerical sequential vector, generated independently on the community IDs values.}
\item{LWClus: }{a data.table representing the commuting flows between clusters. The three variables are cluster_live, cluster_work and amount}
\item{marginals: }{a data.table representing the cluster characteristics. The three variables are cluster, amount_live (residents) and amount_work (workers).}
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo'
}
\seealso{
findClusters
}
\references{
[1] Franconi, L., D'Alo', M. and Ichim, D. (2016). Istat Implementation of the  algorithm to develop Labour Market Areas. Available at 

\url{http://www.istat.it/en/files/2016/03/Description-of-the-LabourMarketAreas-algorithm.pdf}.
}
