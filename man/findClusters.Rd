\name{findClusters}
\alias{findClusters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
findClusters
}
\description{
This function builds labour market areas (LMAs) starting from commuting data between communities i.e. elementary territorial units (municipalities, census output areas, provinces, etc.). The function implements the algorithm described in Coombes and Bond (2008) according to the implementation detailed in Franconi, D'Alo' and Ichim (2016).
}
\usage{
findClusters(LWCom, minSZ, minSC, tarSZ, tarSC, 
verbose = F, sink.output = NULL,trace=NULL,
PartialClusterData=NULL, idcom_type=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LWCom}{
data frame/data.table containing the commuting data. Each row corresponds to an observation i.e. a flow and each column corresponds to a variable. The variables are named: "community_live", integer contains the id number of the elementary zone of residence, "community_work", integer, contains the id number of the working community (elementary zone of work), "amount", integer/numeric contains the number of commuters commuting between the "community_live" and the "community_work" (direction is important). Missing values (NAs) are not allowed. The community id must be positive. Only positive flows are present in the data frame. See Sardinia.   
}
  \item{minSZ}{
integer specifying the parameter containing the acceptable minimum size of an area in terms of occupied persons. Must be positive.
}
  \item{minSC}{
numeric in the interval (0,1) specifying the parameter representing the acceptable minimum self-containment of an area. Usually values range from 0.6 to 0.7.
}
  \item{tarSZ}{
integer specifying the parameter containing the target size of an area in terms of occupied persons. It must be greater than minSZ. 
}
  \item{tarSC}{
numeric in the interval (0,1) specifying the parameter representing the target self containment of an area. It must be greater than minSZ. Usually values range 0.75 to 0.9. 
}
  \item{verbose}{
logical. If TRUE the iteration number and the minimum validity are printed on the screen together with warning messages. Default is FALSE.

}
  \item{sink.output}{
character string containing the name of the .txt file that will contain optional information for each iteration of the algorithm. Default is NULL i.e. the sink function is not activated. 
}
 \item{trace}{integer. If not NULL (default value) and if the number of (internal) iterations is multiple of it, an intermediate output is saved in the file intermclusterData.Rdata in the current working directory. The intermediate output is a list of two elements: a clusterData object and the vector of parameters.}
 
 \item{PartialClusterData }{labour market areas structure (clusterData) representing the starting point of the iterative algorithm.
 
 The labour market areas structure is a list of three components: clusterList (whose variables are: community, cluster and residents), LWClus (variables: cluster_live, cluster_work and amount) and marginals (variables: cluster, 
 amount_live and amount_work). Defaults to NULL. 
 
 If it is not NULL, the information related to the process used to derive this structure is not registered in the output of the function). }
  \item{idcom_type}{character. If not NULL (default value) the identification code of the community is a character and not an integer.}
 
}
\value{The output of the function is a list of lists with components:
\item{lma}{
List of three data.tables: it contains all the information on the partitioning of the initial communities stemming from the input data frame LWCom into a set of labour market areas. Each data frame contains a dimension of the partition: the initial list of communities (municipalities or elementary areas), the relationships between the labour market areas and their structural characteristics. 


The three data.tables are:

\bold{clusterList}: data.table containing the allocation of each community to the corresponing Labour Market Areas (LMA). It  includes three variables: \emph{community}, integer containing the id of the community, \emph{cluster}, integer containing the id of the labour market areas, and \emph{residents}, integer/numeric containing the number of commuters who are residents in the corresponding community. 

\bold{LWClus}: data.table containing the flows between the LMA. It includes three variables:  \emph{cluster_live}, integer representing the id number of the labour market area of residence, \emph{cluster_work}, integer representing the id number of the labour market area of work and \emph{amount}, numeric representing the number of employee commuting from cluster_live to cluster_work (the direction is important).

\bold{marginals}: data.table containing the main characteristics of the labour market areas. The variables representing such characteristics are: 
\emph{cluster}, integer containing the id number of the labour market area, \emph{amount_live} numeric, number of employees who are residents in the LMA (i.e. who live in the LMA), \emph{amount_work} numeric, number of employees working in LMA regardless of where they live (this variable is also known as workers or jobs). 
}
\item{lma.before0}{List with the same structure as lma (above) containing the result of the algorithm before the assignment of the communities belonging to the reserve list to the dominating labour market areas (if it exists). The reserve list is the LMA with id=0. The communities belonging to it can be investigated through the data.table clusterList. } 
\item{reserve.list}{List of lists. Communities that do not improve the value of the validity when assigned to the dominating cluster or that do not have a dominating cluster are put into the reserve list. Each list contains a character string with information on:

the type reason for being assigned to the reserve list; possible values are: "A", "B", "C", "D", "E", "F". Please refer to Franconi, D'Alo' and Ichim (2016) for details on the description of such cases.

the iteration in which the community was assigned to the reserve list;

the id of the dissolved cluster i.e. the cluster to which the community belonged before being assigned to the reserve list;

the value of the validity of such cluster (in character format); 

the id of the community assigned to the reserve list;

the community belonging to the cluster being dissolved with the second lowest 
external relationships value see Franconi, D'Alo' and Ichim (2016) (if available otherwise NULL). } 
\item{comNotAssigned}{List. Component: integer containing the id of the community in the reserve list that the algorithm was not able to assign to any existing cluster. NULL otherwise. } 
\item{zero.list}{List of four objects: they contain information on communities (elementary areas or municipalities) that could not be processed by the algorithm for various reasons; either the number of commuters resident in it is 0 or the number of workers/jobs is 0 or the community has no interaction with any other community. In such cases the algorithm eliminates these communities from the initial list and let the user the choice to allocate them at a later stage (see function AssignSingleComToSingleLma).

Communities: integer containing the ids of the communities that could not be processed by the algorithm. 

LWCom: data.table containing the flows involving the above communities. The data.table is in a sense a subset of the initial data.table containing the commuting data. Its variables are community_live, integer containing the id of the community where the commuters live, community_work, integer containing the id of the community where the commuters work, amount, numeric, containing the number of commuters commuting from community_live to community_work. 

Residents, data frame containing the variables Code, integer representing the id of the community and residents, integer representing the number of employees who are resident in the community. 

Workers, data frame containing the variables Code, integer representing the id of the community and workers, integer/numeric representing the number of commuters working in the community (jobs).
} 
\item{communitiesMovements}{data.table with two columns: \emph{community}  and \emph{moves}. 

\emph{community}: integer. It  represents the community ID;

\emph{moves}: integer. It represents the number of times a community has changed cluster. The movement toward the reserve list is not computed.}

\item{param}{vector containing the parameters used to apply the algorithm, i.e. minSZ, minSC, tarSZ, tarSC.}

\item{idcom_rel}{data.table containing the list of original id community (character) and their corresponding numerical labels created and used inside the algorithm.}
}

\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' and Guido van den Heuvel
}
\note{
Note that everytime that the idcom_type is not NULL in all the output of the function the community identifier will be character and not integer. In anycase in the sink file and in the PartialClusterData component the community identifier will still be integer and not character.
}
\seealso{
LMAwrite
}
\examples{
\dontrun{
out<- findClusters(LWCom=Brindisi, minSZ=1000,minSC=0.6667,tarSZ=10000,tarSC=0.75, 
verbose=TRUE)
}
}

\references{
[1] Franconi, L., D'Alo' M. and Ichim, D. (2016). Istat implementation of the  algorithm to develop Labour Market Areas.

}
