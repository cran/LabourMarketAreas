\name{EqualLmaPartition}
\alias{EqualLmaPartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EqualLmaPartition
}
\description{
This function tests whether two partitions are equal. 
}
\usage{
EqualLmaPartition(lma1, lma2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lma1}{
List (three components) containing all defining information of the partition. The clusterList component should contain variables community and LMA. See findClusters function.
}
  \item{lma2}{
List (three components) containing all defining information of the partition to be compared with the previous one.The clusterList component should contain variables community and LMA.See findClusters function.
}
}
\value{
A logical value.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' 
}
\seealso{
findClusters
}
