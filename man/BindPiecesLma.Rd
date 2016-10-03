\name{BindPiecesLma}
\alias{BindPiecesLma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BindPiecesLma
}
\description{
This function (row) binds two local labour market areas structures. It deletes the communities that are registered twice (or more) in both structures. It deletes the communities that are not registered in the input flows data frame/data.frame. LMA Ids that are registered in both structures are deleted as well.

In case there are common LMA ids, those of input2 are summed to the maximum LMA id of input1.
}
\usage{
BindPiecesLma(input1,input2,LWCom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{input1}{
list of three data.table: clusterList, LWClus and marginals. See function findClusters.
}
\item{input2}{
list of three data.table: clusterList, LWClus and marginals. See function findClusters.
}
\item{LWCom}{
data frame/data.table containing the commuting flows information. Three variables: community_live, community_work and amount. See function findClusters.
}
}

\value{
List of five components.
\item{com.twice.1 }{ numeric vector; communities ids that are registered twice (or more) in the first input data structure}
\item{com.twice.2 }{ numeric vector; communities ids that are registered twice (or more) in the second input data structure}
\item{coms.not.in.flows }{ numeric vector; communities ids that are registered in either input data structures, but not in the commuting flows data.table}
\item{LMAtwice }{ numeric vector; LMA ids that are registered in both input data structures. These LMAs are not included in the new structure.}
\item{lma }{ a list of three data.tables: clusterList, LWClus and marginals. Information on commuters, residents and workers is computed using the input commuting flows data frame/data.table.}
}
\author{
Daniela Ichim, Luisa Franconi and Michele D'Alo' 
}
\seealso{
findClusters
}
