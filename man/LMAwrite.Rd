\name{LMAwrite}
\alias{LMAwrite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LMAwrite
}
\description{
This function saves the lists composing the output of the lma package into separate data frames as .RData. The files are saved in the path_wd directory. The main output - the first and second list containing, respectively, the characteristics of the created labour market areas and the characteristics of the areas before the assignment of the reserve list - are also saved in a .csv file. 
}
\usage{
LMAwrite(out, path_wd = NULL, suff = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out}{
list of lists containing the output of the lma package.
}
  \item{path_wd}{
character containing the path of the directory where the output ought to be saved. Default is NULL, working directory.
}
  \item{suff}{
character containing the suffix to be added to the name of the saved output files. Default is NULL.
}
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' and Guido van den Heuvel
}
