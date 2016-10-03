\name{regroupDissolved}
\alias{regroupDissolved}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
regroupDissolved
}
\description{
This function is used within the core function when the selected cluster to be dissolved has only one municipality. The call, inside this function, to the two functions determineRegroupList and determineCohesion allows to identify the new cluster to which the municipality is temporarily assigned. The output of the function is different whether the new cluster is a proper one or it is the reserve list (i.e. it does not exist a cluster to whom it can be assigned). 
}
\usage{
regroupDissolved(clusterData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusterData}{
List (three components) containing all defining information on the current clusters found by the algorithm.
}
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
