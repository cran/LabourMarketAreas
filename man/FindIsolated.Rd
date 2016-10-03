\name{FindIsolated}
\alias{FindIsolated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FindIsolated
}
\description{
A labour market area is defined as isolated when there are no (spatial) neighbours or there is a unique community inside it. A polygon is defined as isolated  when there are no (spatial) neighbours (e.g. a small island which is part of a community in the main land is isolated). 
This function identifies the isolated labour market areas and their isolated  polygons.  The contiguity between two objects (labour market areas or polygons) is based on spatial relationships, not on their commuting flows.
}
\usage{
FindIsolated(lma,lma_shp=NULL,lma_shp_path,
lma_shp_name,com_shp=NULL,com_shp_path,
com_shp_name, id_com, proj4string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lma}{
List (three components) containing all defining information on the labour market areas. See findClusters function. The names of the lma have already been assigned (see AssignLmaName).
}
  \item{lma_shp}{
spatial polygon object corresponding to  the labour market areas. See function CreateLmaShape. Defaults to NULL.
}

  \item{lma_shp_path}{
character. The path where the shape files of the labour market areas are saved. See function CreateLmaShape. Defaults to NULL.
}

  \item{lma_shp_name}{
character. The file name where the shape files of the labour market areas are saved. See function CreateLmaShape. Defaults to NULL.
}

 \item{com_shp}{
spatial polygon object corresponding to the communities. Defaults to NULL.
}

  \item{com_shp_path}{
character. The path where the shape files of the communities are saved. Defaults to NULL.
}

  \item{com_shp_name}{
character. The file name where the shape files of the communities are saved. Defaults to NULL.
}

 \item{id_com}{
character. The field name of the variable containing the community ID in the communities spatial polygon or in their shape files.
}

\item{proj4string}{
character. Geographical details on the projection system used for both shape files (communities and shape files).
}

}
\value{
A list of two components.
\item{isolated.lma}{ A list of three components: contig.matrix.lma, lma.unique and lma.nolink.
}

contig.matrix.lma: matrix. The contiguity matrix of the given labour market areas.

lma.unique: data.table. It has two columns: lma.unique.ID and lma.unique.name. lma.unique.ID: integer, the identifier of the unique labour market areas.
lma.unique.name: character, name of the unique labour market area. The unique areas are the areas with a unique community.

lma.nolink data.table with two columns: lma.nolink.ID and lma.nolink.name. lma.nolink.ID integer, the identifier of the labour market area with no links. lma.nolink.name: character, the name of the labour market area with no links.

\item{isolated.poly}{A list of three components: contig.matrix.poly, poly.com.linkage and poly.nolink.
 }

contig.matrix.poly: matrix. It is the contiguity matrix of the polygons.

poly.com.linkage data.table with two columns, community and Polygon. It represents the association between communities and polygons.

poly.nolink: data.table with two colums: poly.nolink.ID and poly.nolink.name. poly.nolink.ID integer: the identifiers of the  no-linked polygons.
poly.nolink.name character: the name of the labour market area with no-linked polygons.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' 
}
\seealso{
findClusters, CreateLmaShape
}
