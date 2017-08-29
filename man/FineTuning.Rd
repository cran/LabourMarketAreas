\name{FineTuning}
\alias{FineTuning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FineTuning
}
\description{
This function assigns enclaves (i.e.communities - or part of them - that are not contiguous to the rest of the LMA they belong to) to labour market areas, based on contiguity principle and the cohesion.
}
\usage{
FineTuning(dat, out.ini, list.contiguity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
The commuting flows between communities. See function findCluster.
}
  \item{out.ini}{
A list of three components: clusterList, LWClus and marginals. See function findClusters. The names should not have been assigned. Use function DeleteLmaName.
}
  \item{list.contiguity}{
list. Each component of the list is a vector indicating the neighbouring labour market areas. The names of the list are the communities IDs. 
}
}
\details{
The algorithm (function findCluster) assigns communities to clusters based on a rule. There is no check that communities in clusters are contiguous. For this reason a fine tuning of the initial result is needed (the function FineTuning).
There is no ordering of the neighbours.
}
\value{
A list of two components: 
\item{tunned.lma }{ list of three components clusterList, LWClus and marginals containing the result of the fine tuning procedure. See function findClusters}
\item{not.tunned.commID }{ a vector of communities IDs that were not assigned based on a contiguity principle; e.g. those communities having no flows with their neighbours.}
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' 
}
