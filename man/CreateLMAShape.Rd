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
CreateLMAShape(lma,comIDs,lmaIDs,shp_com,dsn,shp_com_name,id_shp_com,outd,outf,bf,po)}

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
\item{dsn}{
character; data source name specifying the directory (path) in which the shp_com_name file is stored. 
}
\item{shp_com_name}{
character; the file name of the communities shape file. Defaults to NULL. In case both shp_com and shp_com_name are provided, only the first one is considered.
}
\item{id_shp_com}{
character; name of the variable containing the communities identifier in the shp_com object or in the shp_com_name file. 
}

\item{outd}{
character. The path where to save the shape files. Defaults to NULL.
}
\item{outf}{
character. The file name where to save the shape files. Defaults to NULL. If it is provided, the outdir should be provided, too.
}
\item{bf}{
character. The bmpfile file name (including the path) of the bitmap file where to save the cartographic map of the communities and labour market areas together.  Defaults to NULL.
}
\item{po}{
Graphical parameters (plot_opt) to be used when bmpfile is not NULL. The parameters are: background colour of the map, line width of the lma borders, line type of the lma borders, lma names color, line width for the lma names, line type for the lma names, cex factor for the lma names, font for the lma names. Defaults to c("green", 1, 2, "red", 1, 2, 0.8, 2).
}
}
\value{
A list of three components:
\item{shp_lma }{spatial polygon containing the shape file of lma}
\item{comID.in.LMA.not.in.SHP }{vector of communities registered in lma but not in the input shape object/file.}
\item{comID.in.SHP.not.in.LMA }{vector of communities registered in the input shape object/file but not in the lma.}
}
\author{
Daniela Ichim, Luisa Franconi and Michele D'Alo' 
}
\note{
This function has to be applied on a labour market partition whose names are assigned. 
Note that some communities present in the shape files may not be available in the lma structure. These missing communities can be found in the zero.list and ComNotAssigned. They could also be structurally missing i.e. could not be present in the initial commuting data.
Note also that if the option of providing an output file is activated then the usage of an existing file name in the selected outdir will generate an error.
}
\seealso{
findClusters, AssignLmaName
}
