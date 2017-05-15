\name{FindContig}
\alias{FindContig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FindContig
}
\description{
Given a territorial object (either an LMA, formed by a single community, or a polygon) the function determines its contiguous labour market areas.
}
\usage{
FindContig(type = "poly", lma, contig.matrix, isolated)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
Character indicating which type of objects should be treated. It may be "poly" (polygons) or "lma" (local labour market areas). Defaults to "poly".
}
  \item{lma}{
A list of three components clusterList, LWClus, marginals. See the function FindClusters.
}
  \item{contig.matrix}{
matrix: the (spatial) contiguity matrix to be used (see the function FindIsolated).
}
  \item{isolated}{
Information on the isolated objects. If type is "poly", isolated is the association matrix between communities and polygons. See the poly.com.linkage component in the output of FindIsolated function. If type is "lma", isolated is the vector of labour market areas identifiers. See the lma.unique component in the output of the FindIsolated function.
}
}
\value{
If type="poly", the output is a list of two components:
\item{list.contig.poly}{list containing the IDs of the contiguous labour market areas of each community (polygon). The list names are the communities identifiers.}
\item{com_no.LMA.neigh}{character. The names of the communities having only polygons as neighbours.}

If type="lma", the output is a list containing the IDs of the contiguous labour market areas of each given lma. The list names are the communities identifiers. This option should be used for the labour market areas with a unique community.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' 
}
\note{
This function should be used to identify the neighbours of the labour market areas having an unique community (type="lma") or to identify the labour market areas which are contiguous to a polygon associated to a unique community (the case of a community having an enclave outside its own territory).

If type="poly", there is no special ordering of the contiguous LMAs.

If type="lma", the contiguous LMAs are ordered in decreasing order of commuters who are resident in the LMA.
}
\seealso{
findClusters, FindIsolated}
