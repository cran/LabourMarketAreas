\name{AssignLmaName}
\alias{AssignLmaName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AssignLmaName
}
\description{
This function assigns names to each labour market area given its code. The lma name corresponds to the community name (elementary area or municipality) having the highest number of jobs among all the communities in the lma. If more than one community shares the maximum number of jobs, the first one is taken. In order to differ the community name by the lma with the same name, the community is expressed as  first letter in uppercase and the remaining letters in lowercase, whereas the lma name is all in uppercase.
}
\usage{
AssignLmaName(LWCom,lma,ComNames)
}
\arguments{
  \item{LWCom}{
input data frame/data.table of commuting data (see for example Sardinia).
}
\item{lma}{
list of three data tables. It is the output of the function findClusters. It contains the three data.tables clusterList, LWClus, marginals.
}
\item{ComNames}{
data frame/data.table containing code, integer representing the id of the community and com.name, character containing the community name.  The Code must be positive.
}
}
\value{
\item{lma}{
list of three data.tables. It contains the three data.table clusterList, LWClus, marginals with the new added columns related to the names of the communities and their corresponding labour market areas. The added columns are character type. In clusterList the new variables are com.name, lma.name, in LWCom the variables are  lma.name.live, lma.name.work  and in marginals the new added variable is lma.name.
}
}
\author{
Daniela Ichim, Luisa Franconi and Michele D'Alo' 
}
\seealso{
findClusters
}
