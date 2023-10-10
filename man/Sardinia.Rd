\name{Sardinia}
\alias{Sardinia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sardinia
}
\description{data frame/data.table containing the commuting flows between municipalities in Sardinia Island (Italy) from 2001 Italian Population Census (the complete data set http://www.istat.it/it/archivio/157423).  Each row corresponds to an observation i.e. a flow and each column corresponds to a variable.  The variables are: community_live, community work and amount, in this order.  The meaning of the variables is the following: community_live, integer, id number of the community (elementary territorial unit) where the commuter/commuters live, community_work, integer, containing the id number of the community (elementary territorial unit) where the commuter/commuters work and amount, numeric containing the number of commuter/commuters commuting between community_live and community_work (direction is important). 
}
\usage{
data(Sardinia)
#Encoding(names.Sardinia$com.name)="latin1" # to deal with a specific accent.
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo'
}
