\name{LabourMarketAreas-package}
\alias{LabourMarketAreas-package}
\alias{LabourMarketAreas}
\docType{package}
\title{
LabourMarketAreas
}
\description{
Makes Travel-To-Work-Areas from commuting flow data by means of the version of the TTWA algorithm by Coombes and Bond (2008) according to the implementation carried out at Istat (Franconi, D'Alo' and Ichim, 2016) within a Europan framework of Labour Market Area development (Franconi, Ichim and D'Alo'  2017). The whole process of Labour Market Areas development has been taken care of (regionalisation algorithm, creation of shape file, assignment of names, fine tuning of the output of the algorithm, quality assessment and sensitivity analysis.).
}
\details{
\tabular{ll}{
Package: \tab LabourMarketAreas\cr
Type: \tab Package\cr
Version: \tab 3.2.3\cr
Date: \tab 2020-01-13\cr
License: \tab GPL (>=2)\cr
}

Labour market areas (LMAs) are sub-regional geographical areas where the bulk of the labour force lives and works, and where establishments can find the main part of the labour force necessary to occupy the offered jobs. They are functional regions that stem from the aggregation of elementary
geographical units (municipalities, census output areas, etc.) on the basis of
their level of spatial interaction measured by commuting to work 
flows through quantitative methods. The guiding idea is to maximise the flow inside  the area (internal cohesion) and minimise it outside (external separation) according to a predefined rule.

The package is based on original script by Guido van den Heuvel at Statistics Netherlands and further developed at the Italian National Statistical Institute (Istat) to implement the algorithm described in Coombes and Bond (2008) - a variation of the seminal paper by Coombes et al. (1986). See Franconi, D'Alo'and Ichim (2016) for the full description of this implementation and Franconi, Ichim and D'Alo' (2017) for an overview of project. 

The algorithm is a rule based algorithm that stops when all areas satisfy the rule.

Every area is characterised by the number of commuters living in it, by the number of commuters that go there to work (called number of jobs/workers) and by those commuters that live and work in the same area. The ratios between these quantities define the concept of self-containment. We are interested in the minimum self-containment i.e. the minimum between the Supply side self-containment and the Demand side self-containment (see Coombes and Bond, 2008). According to Coombes and Bond (2008) an area is a Labour Market Area (LMA) if it satisfies the validity condition (see Coombes and Bond, 2008). Such validity depends on the number of commuters living in the area, the minimum self-containment and four parameters chosen by the user (see function findcluster).

The package produces LMAs for the country/region for which commuting flows are available at basic territorial level (municipality, province, census output areas, etc.). We call this basic territorial level community. If the names of the communities are provided, the package allows to assign names to each LMA (see function AssignLmaName). The package allows also to plot a cartographic map of the produced LMAs given the shape files of the communities (see function PlotLmaCommunity). 
Finally the package identifies enclaves (communities that are not contiguous with the rest of the LMA they belong to) and perform a re-assignment of such community based on contiguity and cohesion (see the function FineTuning). 

The main output of the function findcluster is the set of LMAs produced by the algorithm, the so called a partition. There are three dimensions that characterise the partition: the dimension of the starting configuration i.e. number of communities in the country/region of interest, the dimension of the final configuration, i.e. the number of LMAs identified by the algorithm using the chosen parameters, and the flows between LMAs. 
These three dimensions need to be represented in the solution of the algorithm; this is done through a list of three data.table the lma (or clusterData) data structure. Such lma data structure is the cornerstone of the whole package as it is the input or the output of many functions inside the package. 

The work was partially funded by Eurostat Grant "EU-TTWA method: improvements, documentation and sharing knowledge activities" awarded to Istat the Italian National Statistical Institute (\url{http://www.istat.it/en/archive/182743}). This is part of a system of grants in the EU with the aim of supporting the ongoing development of a methodology for the creation of LMA and test the application of methods nationally in the participating National Statistical Institutes. The goal is to arrive at a harmonised EU-wide definition for labour market areas. More info at: 

\url{https://ec.europa.eu/eurostat/cros/content/labour-market-areas_en}

}
\author{
Daniela Ichim, Luisa Franconi, Michele D'Alo' and Guido van den Heuvel

Maintainer: Luisa Franconi <franconi at istat.it>
}
\references{

[1] Coombes, M. e Bond, S. (2008). Travel-to-Work Areas: the 2007 review. London: Office for National Statistics, 2008. 

\url{https://ons.maps.arcgis.com/home/item.html?id=5efb4f1c1a114fef8f74f6f0347c10e8}

[2] Coombes, M., Casado-Diaz, J.M., Martinez-Bernabeu, L. e Carausu, F. (2012). Study on comparable labour market areas: final research report. 17 October 2012. Eurostat- Framework contract no:6001. 2008.001 - 2009.065, Specific contract no:50405.2010.004 - 2011.325.

[3] Coombes, M.G., Green, A.E. e Openshow, S. (1986). An efficient algorithm to generate official statistics report areas: the case of the 1984 Travel-to-Work Areas in Britain. The Journal of Operational Research Society, Vol. 37, No. 10, pp. 943-953.

[4] Franconi, L., D'Alo', M. and Ichim, D. (2016). Istat Implementation of the  algorithm to develop Labour Market Areas. Available at 

\url{http://www.istat.it/en/files/2016/03/Description-of-the-LabourMarketAreas-algorithm.pdf}.

[5] Franconi, L., Ichim, D. and D'Alo', M. (2017). Labour Market Areas for territorial policies:
tools for a European approach. Statistical Journal of the IAOS,
Vol. 33, No. 3, pp. 585-591. 

\url{https://content.iospress.com/articles/statistical-journal-of-the-iaos/sji160343}

[6] Istat (2015). La nuova Geografia dei Sistemi Locali. (in italian). Available at 

\url{http://www.istat.it/it/files/2015/10/La-nuova-geografia-dei-sistemi-locali.pdf}

}
\keyword{ LabourMarketAreas }

