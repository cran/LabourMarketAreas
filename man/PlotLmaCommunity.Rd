\name{PlotLmaCommunity}
\alias{PlotLmaCommunity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotLmaCommunity
}
\description{
This function visualizes/plots the LMAs containing given communities.
  
It may be used to compare two partitions or to see the assignment of a set of communities during the iterations of the algorithm (object intermClusterData when trace is not NULL in the findClusters function). 
}
\usage{
PlotLmaCommunity(list.lma, lmaIDs, communityID, shp_com, id_shp,
bmpfile, col.vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{list.lma }{
List of two labour market areas structures (lma). The two components of the list are lists with components clusterList, LWClus and marginals representing the two different partitions to be compared. The names of the communities and LMAs have already been assigned before running this function; See functions findClusters and AssignLmaName.

If the list has length greater than 2, only the first two partitions will be compared.
}
\item{lmaIDs }{
The name of the variable identifying the LMA in list.lma.
}
\item{communityID }{
Positive integer vector, the IDs of the communities under study.
}
\item{shp_com }{
object of class sf containing the shape files of the communities.
}
\item{id_shp }{
The name of the variable identifying the communities in shp_com object of class sf.
}
\item{bmpfile }{
character: the name of the bmp file where to save the images complete with its path.
}
\item{col.vec }{
character vector of three color names. The first one corresponds to the communities under investigation; the second color corresponds to the communities which are present in the first partition, but not in the second one; the third color corresponds to the communities which are present in the second partition, but not in the first one. Defaults to c("red","orange","yellow").
}
}
\value{
logical  indicating whether the communities belong to the same clusters. The identity is checked using the cluster ID.

The graphic is saved in the specified path/file.
}

\author{
Daniela Ichim, Luisa Franconi
}

\seealso{
findClusters, AssignLmaName
}
