\name{CreateLMAShape}
\alias{CreateLMAShape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CreateLMAShape
}
\description{
Starting from the communities shape files, the function creates the shape files of labour market areas. 
}
\usage{
CreateLMAShape(lma,comIDs,lmaIDs,shp_com, shp_com_name,
id_shp_com,proj4string, outdir,outfile,bmpfile,plot_opt)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lma}{
List (three data.tables) containing all defining information of the LMAs. See the output of the function findClusters. The names of the clusters have been assigned, see AssignLmaName function.
}
  \item{comIDs}{
Name of the variable containing the community identifier in the lma input.
}
   \item{lmaIDs}{
Name of the variable containing the labour market areas identifier in the lma input.
}
 \item{shp_com}{
Spatial polygon (R object) containing the shape file of the communities. Defaults to NULL. In case both shp_com and shp_com_name are provided, only the first one is considered.
}
\item{shp_com_name}{
The file name (including the path) of the communities shape file. Defaults to NULL. In case both shp_com and shp_com_name are provided, only the first one is considered.
}
\item{id_shp_com}{
Name of the variable containing the communities identifier in the shp_com object or in the shp_com_name file. 
}
\item{proj4string}{
character. Spatial projection identification. Defaults to "+proj=longlat +datum=WGS84". 
}
\item{outdir}{
character. The path where to save the shape files. Defaults to NULL.
}
\item{outfile}{
character. The file name where to save the shape files. Defaults to NULL. If it is provided, the outdir should be provided, too.
}
\item{bmpfile}{
character. The file name (including the path) of the bitmap file where to save the cartographic map of the communities and labour market areas together.  Defaults to NULL.
}
\item{plot_opt}{
Graphical parameters to be used when bmpfile is not NULL. The parameters are: background colour of the map, line width of the lma borders, line type of the lma borders, lma names color, line width for the lma names, line type for the lma names, cex factor for the lma names, font for the lma names. Defaults to c("green", 1, 2, "red", 1, 2, 0.8, 2).
}
}
\value{
A list of three components:
\item{shp_lma }{spatial polygon containing the shape file of the lma}
\item{comID.in.LMA.not.in.SHP }{vector of communities registered in lma but not in the input shape object/file.}
\item{comID.in.SHP.not.in.LMA }{vector of communities registered in the input shape object/file but not in the lma.}
}
\author{
Daniela Ichim, Luisa Franconi and Michele D'Alo' 
}
\note{
This function may be applied on a labour market partition whose names are assigned. 
}
\seealso{
findClusters, AssignLmaName
}
