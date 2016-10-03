\name{regroupDissolved.ncom}
\alias{regroupDissolved.ncom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
regroupDissolved.ncom
}
\description{
This function is used within the core function findcluster when the selected cluster to be dissolved has more than one municipality. The function identifies the new cluster to which the selected municipality is temporarily assigned. The output of the function is different whether the new cluster is a proper one or it is the reserve list (i.e. it does not exist a cluster to whom it can be assigned).
}
\usage{
regroupDissolved.ncom(clusterData, index.com.2diss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusterData}{
List (three components) containing all defining information on the current clusters found by the algorithm. 
}
  \item{index.com.2diss}{
integer containing the identifier of the community inside the dissolved cluster that will be assigned to a different cluster.
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
