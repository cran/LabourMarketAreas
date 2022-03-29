\name{SardiniaGrid}
\alias{SardiniaGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SardiniaGrid
}
\description{data frame/data.table containing the commuting flows between grid cells in the Sardinia region; 
from Italian Population Census 2011.  
Each row corresponds to an observation i.e. a flow and each column corresponds to a variable.  
The variables are: community_live, community work and amount, in this order.  
The meaning of the variables is the following: community_live, Character, containing the id of the grid cell where the commuters live, community_work, character, containing the id grid cell where the commuter/commuters work and amount, numeric, containing the number of commuter/commuters commuting between community_live and community_work (direction is important). 
}
\usage{
data(SardiniaGrid)
}
\source{
The data is elaborated from the shape files of the Geostat 2011 population grid: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat 
}
\author{
Daniela Ichim, Luisa Franconi 
}
