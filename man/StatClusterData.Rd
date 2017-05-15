\name{StatClusterData}
\alias{StatClusterData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
StatClusterData
}
\description{
This function computes several statistics on a given set of labour market areas (a given partition). 
}
\usage{
StatClusterData(lma,param,threshold,dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lma}{
A list of data.table containing information on the labour market areas. Three components: clusterList, LWClus and marginals. clusterList is a data.table containing the variables community, cluster and EMP_live; everything else will be ignored. LWClus is a data.table containing the variables cluster_live, cluster_work and amount; everything else will be ignored. marginals is a data.table containing the variables cluster, amount_live and amount_work; everything else will be ignored. 

In each data.table object, the order of the variables is mandatory.

The lma names should have not been assigned; otherwise use function DeleteLmaName.
}
\item{param}{numeric vector: the set of parameters corresponding to the lma object, i.e. minSZ,minSC,tarSZ,tarSC, respectively. See function findClusters.}
\item{threshold}{
numeric. It is used to identify particular small labour market areas or flows. 
}
\item{dat}{
data frame/data.table containing the commuting flows between communities (see for example Sardinia).
}
}
\value{
A list with the following components:
\item{marginals}{data.table containing the following variables for each LMA.

\bold{LMA: }{ positive integer; labour market areas ID} 

\bold{EMP_live: }{numeric; number of commuters who live in the area} 

\bold{EMP_work: }{numeric; number of commuters working in the area}

\bold{validity: }{numeric; validity value computed with the current parameters}

\bold{EMP_live_work: }{numeric; number of commuters living and working in the area}

\bold{lma_commuter_percent: }{numeric; the quantity: (EMP_live-EMP_live_work)+(EMP_work-EMP_live_work)/(2*EMP_live_work) }

\bold{Home_Work_Ratio: }{numeric; the quantity 
  (( EMP_live-EMP_live_work)-( EMP_work-EMP_live_work))/EMP_live_work
 }

\bold{SC_demand_side: }{numeric; demand side self-containment}

\bold{SC_supply_side: }{numeric; supply side self-containment}

\bold{N_com: }{integer; number of communities forming the LMA}

\bold{InternalCohesionLink: }{numeric; consistency of internal relationships. It is given by the ratio between number of links between communities inside LMA, excluding itself, and the maximum number of possible links, i.e. (N_com * (N_com-1)). See [1].
}

\bold{InternalCohesionFlows: }{numeric; intensity of internal relationships. It is the percentage of internal flows (excluding flows having as origin and destination the same node) of the LMA between different communities w.r.t the total internal flows. See [3].
}

\bold{NbCentralComm: }{integer; number of communities having a centrality index greater than 1 (for communities with more than 100 workers, the centrality index is the ratio between net incoming flows and net outgoing flows).
}

\bold{N_links_in: }{integer; number of LMAs whose residents work in the current LMA (including itself) } 

\bold{N_links_out: }{integer; number of LMAs where the residents of the current LMA work (including itself)}


}

\item{StatFlows}{list containing several statistics on flows and links between the labour market areas of the given partition.

\bold{N_links: }{numeric; number of links between LMAs}

\bold{PercNbLinksLessThreshold: }{numeric; percentage of links corresponding to flows below threshold}

\bold{summFlows: }{numeric vector; summary statistics on flows}

\bold{summFlowsNoItself: }{numeric vector; summary statistics on flows, excluding the self-flows}

\bold{summLinks_in: }{numeric vector; summary statistics on the number of incoming flows}

\bold{summLinks_out: }{summary statistics on the number of outgoing flows}

\bold{clusterMaxNlinks_in: }{positive integer; the LMA ID of the cluster reaching the maximum number of incoming flows}

\bold{clusterMaxNlinks_out: }{positive integer; the LMA ID of the cluster reaching the maximum number of outgoing flows}

\bold{clusterMinNlinks_in: }{positive integer; the LMA ID of the cluster reaching the minimum number of incoming flows}

\bold{clusterMinNlinks_out: }{positive integer; the LMA ID of the cluster reaching the minimum number of outgoing flows}


}
\item{StatQuality}{list containing several statistics on the given partition: 

\bold{NbClusters: }{integer; number of clusters}

\bold{NbClusterUniqueCom: }{integer; number of clusters with an unique community}

\bold{NbClustersValidLess1: }{integer; number of clusters with validity smaller than 1}

\bold{NbClustersNoCentralCom: }{integer; number of clusters with no communities having a centrality measure greater than 1 }

\bold{Mean.SC_demand_side: }{numeric; mean of the demand side self-containment of the clusters in the partition  }

\bold{Std.SC_demand_side: }{numeric; standard deviation of the demand side self-containment }

\bold{Mean.SC_supply_side: }{numeric; mean of the supply side self-containment of the clusters in the partition }

\bold{Std.SC_supply_side: }{numeric; standard deviation of the supply side self-containment   }

\bold{Q1.InternalCohesionFlows: }{numeric; first quartile of the InternalCohesionFlows }

\bold{Q2.InternalCohesionFlows: }{numeric; median of the InternalCohesionFlows}

\bold{Q3.InternalCohesionFlows: }{numeric; third quartile of the InternalCohesionFlows }

\bold{Q1.InternalCohesionLink: }{numeric; first quartile of the InternalCohesionLink }

\bold{Q2.InternalCohesionLink: }{numeric; median of the InternalCohesionLink}

\bold{Q3.InternalCohesionLink: }{numeric; third quartile of the InternalCohesionLink }

\bold{Q1.EMP_live: }{numeric; first quartile of the residents }

\bold{Q2.EMP_live: }{numeric; median of the residents}

\bold{Q3.EMP_live: }{numeric; third quartile of the residents }

\bold{Mean.EMP_live: }{numeric; mean value of the residents }

\bold{Std.EMP_live: }{numeric; standard deviation of the residents }

\bold{Min.EMP_live: }{numeric; minimum value of the residents }

\bold{Max.EMP_live: }{numeric; maximum value of the residents }

\bold{Q1.EMP_work: }{numeric; first quartile of the workers/jobs }

\bold{Q2.EMP_work: }{numeric; median of the workers/jobs}

\bold{Q3.EMP_work: }{numeric; third quartile of the workers/jobs }

\bold{Mean.EMP_work: }{numeric; mean value of the workers }

\bold{Std.EMP_work: }{numeric; standard deviation of the workers }

\bold{Min.EMP_work: }{numeric; minimum value of the workers }

\bold{Max.EMP_work: }{numeric; maximum value of the workers }

\bold{Q1.EMP_live_work: }{numeric; first quartile of the commuters living and working in the same area}

\bold{Q2.EMP_live_work: }{numeric; median of the commuters living and working in the same area}

\bold{Q3.EMP_live_work: }{numeric; third quartile of the the commuters living and working in the same area }

\bold{Min.EMP_live_work: }{numeric; minimum value of the commuters living and working in the same area }

\bold{Max.EMP_live_work: }{numeric; maximum value of the commuters living and working in the same area }

\bold{Mean.lma_commuter_percent: }{numeric; mean value of the quantity: (EMP_live-EMP_live_work)+(EMP_work-EMP_live_work)/(2*EMP_live_work) }

\bold{Std.lma_commuter_percent: }{numeric; standard deviation of the quantity (EMP_live-EMP_live_work)+(EMP_work-EMP_live_work)/(2*EMP_live_work) }

\bold{Mean.Home_Work_Ratio: }{numeric; mean value of the quantity 
  (( EMP_live-EMP_live_work)-( EMP_work-EMP_live_work))/EMP_live_work
 }
 \bold{Std.Home_Work_Ratio: }{numeric; standard deviation of the quantity 
  (( EMP_live-EMP_live_work)-( EMP_work-EMP_live_work))/EMP_live_work
 }
 
\bold{Q_modularity: }{numeric; Q_modularity index }
}
\item{param: }{numeric vector; it contains the parameters of the given solution, i.e. the output of the function findClusters. The parameters are minSZ,minSC,tarSZ,tarSC.}
}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo'
}
\seealso{
findClusters
}

\references{
[1] Erba, A., D'Angio', A. e Marzulli, S. (1990). Partizioni funzionali del territorio: il modello Isers, Franco Angeli, Milano.

[2] Franconi, L., D'Alo' M. and Ichim, D. (2016). Istat implementation of the algorithm to develop Labour Market Areas.

[3] Lipizzi, F. (2014). Strumenti e indicatori per la misura della consistenza e omogeneita' delle aree funzionali. XXXV Conferenza annuale AISRe, "Uscire dalla crisi. Citta', Comunita' e Specializzazione Intelligenti", Padova,  11-13 September 2014.

}

