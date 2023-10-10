\name{determineCohesion}
\alias{determineCohesion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
determineCohesion
}
\description{
Computes the cohesion matrix.
}
\usage{
determineCohesion(LWClus, marginals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{LWClus}{
data table containing the flows between the clusters.
}
\item{marginals}{
data table containing the main characteristics of the current set of clusters.
}
}
\value{
The cohesion matrix between clusters.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo', Guido van den Heuvel
}
\note{
This function is called and used internally by the main function findClusters
through the function determineRegroupList. 
}
\seealso{
determineRegroupList
}
\references{

[1] Coombes, M.G., Green, A.E. and Openshow, S. (1986). An efficient algorithm to generate official statistics report areas: the case of the 1984 Travel-to-Work Areas in Britain. The Journal of Operational Research Society, Vol. 37, No. 10, pp. 943-953.

[2] Franconi, L., D'Alo' M. and Ichim, D. (2016). Istat implementation of the  algorithm to develop Labour Market Areas.

}
