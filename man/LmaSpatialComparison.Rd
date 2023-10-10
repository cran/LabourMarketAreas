\name{LmaSpatialComparison}
\alias{LmaSpatialComparison}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LmaSpatialComparison
}
\description{
This function takes as input the shapefiles of two distinct sets of LMAs (on the same territory) and produces statistics to compare them.
}
\usage{
LmaSpatialComparison(shape,shape_ref,lma.id="LMA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shape}{
Shapefile of the output of an LMA delineation process (sf object).
}
  \item{shape_ref}{
Shapefile of the output of an LMA delineation process (sf object).
}

  \item{lma.id}{
Column name of the column registering the LMA (the same in both shape files).
}

  
}
\value{
The output is a data.table with statistics on the comparison of the two shapefiles.

Each row represents an association between an LMA in the
shape partition (first argument) and an unique LMA in the shape_ref
partition (second argument).

The output columns are as follows:
\item{shape_lma}{ID of the LMA unit in the shape partition (first argument)}
\item{shape_ref_lma}{ID of the LMA unita in the shape_ref partition (second argument)}
\item{area_intersection}{area of the intersection of the two LMA units}
\item{shape_area }{area of the LMA unit in the shape partition (first argument)}
\item{shape_ref_area}{area of the associated LMA unit is the shape_ref partition (second argument)}       
\item{shape_EMP_live}{number of employees living in the LMA unit is the shape partition}
\item{shape_EMP_work}{number of employees working in the LMA unit in the shape partition} \item{shape_ref_EMP_live}{number of employees living in the LMA unit is the shape_ref partition}
\item{shape_ref_EMP_work}{number of employees working in the LMA unit in the shape_ref partition}     
\item{perc_intersection_shape}{percentage of the intersection area over the area of the LMA unit in the shape partition}
\item{perc_intersection_shape_ref}{percentage of the intersection area over the area of the LMA unit in the shape_ref partition}

}


\author{
Daniela Ichim, Luisa Franconi 
}
\seealso{
findClusters, CreateLmaShape
}
