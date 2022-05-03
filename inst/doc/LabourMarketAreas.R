## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = 'center',
  fig.width = 7,
  comment = "#>",
  root.dir="/tmp"
)
suppressPackageStartupMessages(library(sp))
suppressMessages(library(sp))
suppressMessages(library(data.table))


## ----echo=FALSE---------------------------------------------------------------
suppressPackageStartupMessages(library(sp))
suppressMessages(library(sp))
options(warn=-1)
options(digits=2)
options(rmarkdown.html_vignette.check_title=FALSE)
options(big.mark="")
library(data.table)
library(tmap)
tmap_mode("view")

## ----eval=TRUE----------------------------------------------------------------
library(LabourMarketAreas)
data(Brindisi)
#?Brindisi
head(Brindisi)

## ----eval=TRUE,fig.align='center'---------------------------------------------
#?shpBrindisi
data("shpBrindisi")
tm_shape(shpBrindisi)+tm_borders("black",alpha=0.5)+tm_fill("gray",alpha=0.2)

## ----eval=TRUE----------------------------------------------------------------
data("names.Brindisi")
head(names.Brindisi)

## ----eval=F-------------------------------------------------------------------
#  ?findClusters

## ----eval=T-------------------------------------------------------------------
out = findClusters(LWCom=Brindisi, minSZ=1000,minSC=0.6667,tarSZ=10000,tarSC=0.75, 
verbose=FALSE)

## ----eval=T-------------------------------------------------------------------
str(out)

## ----eval=T-------------------------------------------------------------------
#str(out$lma)
head(out$lma$clusterList)

## ----eval=T-------------------------------------------------------------------
head(out$lma$LWClus)

## ----eval=T-------------------------------------------------------------------
head(out$lma$marginals)

## ----eval=T-------------------------------------------------------------------
str(out$reserve.list)

## ----eval=T-------------------------------------------------------------------
#str(out$lma.before0)
out$lma.before0$clusterList[cluster==0]

## ----eval=T-------------------------------------------------------------------
lma_name=AssignLmaName(Brindisi,out$lma,names.Brindisi)
head(lma_name$clusterList)
#head(lma_name$LWClus)
#head(lma_name$marginals)

## ----eval=T-------------------------------------------------------------------
out_shp=CreateLMAShape(lma=lma_name,
                  comIDs="community",
                  lmaIDs="LMA",
                  shp_com=shpBrindisi,
                  id_shp_com="PRO_COM")
# str(shp)

## ----eval=T-------------------------------------------------------------------
# check whether there are communities registered in lma but not in the communities shape file
out_shp$comID.in.LMA.not.in.SHP
#check whether there are communities registered in the communities shape file but not in the lma
out_shp$comID.in.SHP.not.in.LMA

## ----eval=T,fig.align='center'------------------------------------------------
# plot(shp)
# or

tm_shape(shpBrindisi)+tm_borders("black",alpha=0.5)+tm_fill("gray",alpha=0.2)+tm_shape(out_shp$shp_lma)+tm_borders("red",alpha=0.5)+tm_fill("blue",alpha=0.2)

## ----eval=T-------------------------------------------------------------------
lma_no_name=DeleteLmaName(lma_name)
#?StatClusterData
mystat=StatClusterData(lma_no_name,out$param,1,Brindisi)
# mystat$marginals
# mystat$StatFlows
# mystat$StatQuality

#?StatReserveList
stat_reserve=StatReserveList(out$reserve.list,Brindisi)


## ----eval=F-------------------------------------------------------------------
#  #?FindIsolated
#  iso=FindIsolated(lma=lma_name,
#                   lma_shp=out_shp$shp_lma,
#                   com_shp=shpBrindisi,
#                   id_com="PRO_COM"
#                  )
#  

## ----eval=T,echo=F------------------------------------------------------------
setwd("..")
setwd("R")
load("sysdata.rda")
setwd("..")
setwd("vignettes")

## ----eval=T,echo=F,warning=FALSE,message=FALSE,cache.comments=FALSE,fig.align='center'----

lma_shp_path = NULL
lma_shp_name = NULL
com_shp_path = NULL
com_shp_name = NULL
  
  lma=lma_name
  lma_shp=out_shp$shp_lma
  com_shp=shpBrindisi
  id_com="PRO_COM"
  
      shp = lma_shp
      proj4string = lma_shp@proj4string
      comuni91 = com_shp
      proj4string = com_shp@proj4string
    
  kiki = grep(id_com, names(comuni91@data))

  
  comuni91 <- sp::merge(comuni91, data.frame(lma$clusterList), 
                        by.x = kiki, by.y = "community")
  nbpolygsll <- spdep::poly2nb(shp, row.names = shp@data$LMA, 
                               queen = TRUE)
  Wsll = spdep::nb2mat(nbpolygsll, style = "B", zero.policy = TRUE)
  #rownames(Wsll[rowSums(Wsll) == 0, ])
  colnames(Wsll) = shp@data$LMA
  nolinksll = shp@data$LMA[shp@data$LMA%in%rownames(Wsll)[rowSums(Wsll) ==      0]]
  nolinksllname = shp@data$lma.name[shp@data$LMA %in% rownames(Wsll)[rowSums(Wsll) ==  0]]
  shp = shp[!shp$LMA %in% nolinksll, ]
  setDT(lma$clusterList)
  lma$clusterList = lma$clusterList[!(lma$clusterList$LMA %in% 
                                        nolinksll), ]
  lma$LWCom = lma$LWCom[!(lma$LWCom$LMA_live %in% nolinksll), 
  ]
  lma$LWCom = lma$LWCom[!(lma$LWCom$LMA_work %in% nolinksll), 
  ]
  lma$marginals = lma$marginals[!(lma$marginals$LMA %in% nolinksll), 
  ]
  lma$clusterList[, `:=`(Ncom, .N), by = LMA]
  badsll = lma$clusterList[Ncom == 1, ]
  setorder(badsll, -EMP_live)
  badsll = badsll$LMA
  badsllname = "0"
  if (length(badsll) > 0) {
    for (kiki in 1:length(badsll)) {
      badsllname = c(badsllname, as.character(shp@data$lma.name[shp$LMA == 
                                                                  badsll[kiki]]))
    }
  }
  badsllname = badsllname[-1]
  d = disaggregate(shp)
  d@data = data.table(d@data)
  d@data[, `:=`(x, .N), by = lma.name]
  d@data[x >= 2, `:=`(ID_PiecesLMA, paste(as.character(LMA), 
                                          1:max(x), sep = "_")), by = lma.name]
  d@data[is.na(ID_PiecesLMA), `:=`(ID_PiecesLMA, as.character(LMA))]
  sll.shp.nb <- spdep::poly2nb(d, row.names = d@data$ID_PiecesLMA, 
                               queen = TRUE)
  W = spdep::nb2mat(sll.shp.nb, style = "B", zero.policy = TRUE)
  #rownames(W[rowSums(W) == 0, ])
  colnames(W) = d@data$ID_PiecesLMA
  nolinkpolig = d@data$ID_PiecesLMA[d@data$ID_PiecesLMA %in% 
                                      rownames(W)[rowSums(W) == 0]]
  nolinkpoligname = d@data$lma.name[d@data$ID_PiecesLMA %in% 
                                      rownames(W)[rowSums(W) == 0]]
  badlmalist = sort(unique(d@data$LMA[d@data$x > 1]))
  zeris = c(0, 0)
  df.mun.poly = data.frame(t(zeris))
  colnames(df.mun.poly) = c("community", "Polygon")
  #rm(zeris)
  indice.ident = grep(id_com, names(comuni91@data))
  badlma=badlmalist[1]
  
  
   zz = d[d$LMA == badlma, ]
    zz@data$colore=c("yellow","red")
    # plot(zz, main = paste("polygons", unique(zz@data$lma.name), 
    #                       sep = " "), col = c("yellow", "red", "cyan", "orange"), 
    #      col.main = "red")
    # centroids <- coordinates(zz)
    # text(centroids, label = zz@data$ID_PiecesLMA, cex = 0.8)
    
    mappa1=tm_shape(zz)+tm_borders("gray")+tm_fill(col="colore")+tm_text("ID_PiecesLMA")
    mappa2=tm_shape(comuni91[comuni91$LMA == badlma, ])+tm_borders("black")+tm_fill("gray",alpha=0.2)+tm_text("PRO_COM")
    tmap_arrange(mappa1,mappa2,ncol=2)

## ----eval=T-------------------------------------------------------------------
conti.lma=FindContig(type = "lma", 
                     lma=out$lma, 
                     contig.matrix=iso$isolated.lma$contig.matrix.lma, 
                     isolated=iso$isolated.lma$lma.unique$lma.unique.ID)

conti.lma$list.contig.lma

## ----eval=T-------------------------------------------------------------------
conti.poly=FindContig(type = "poly", 
                      lma=out$lma, 
                      contig.matrix=iso$isolated.poly$contig.matrix.poly, 
                      isolated=iso$isolated.poly$poly.com.linkage)


conti.poly$list.contig.poly
conti.poly$com_no.LMA.neigh


## ----eval=T-------------------------------------------------------------------
conti.poly$list.contig.poly=
  conti.poly$list.contig.poly[!is.na(conti.poly$list.contig.poly)]

## ----eval=T-------------------------------------------------------------------
out$lma=DeleteLmaName(out$lma)
lma.tuned=FineTuning(dat=Brindisi, 
                     out.ini=out$lma, 
                     list.contiguity=conti.lma$list.contig.lma)

str(lma.tuned$tunned.lma)
str(lma.tuned$not.tunned.commID)

poly.tuned=FineTuning(dat=Brindisi, 
                      out.ini=out$lma, 
                      list.contiguity=conti.poly$list.contig.poly)

output=AssignLmaName(Brindisi,lma.tuned$tunned.lma,names.Brindisi)
out$lma=output


## ----eval=T-------------------------------------------------------------------
#generate a partition with a first set of parameters
out1= findClusters(LWCom=Brindisi, minSZ=50,minSC=0.3,tarSZ=100,tarSC=0.4)
out1_name=AssignLmaName(Brindisi,out1$lma,names.Brindisi)                    
x=CreateLMAShape(out1_name,comIDs="community",lmaIDs="LMA",shp_com=shpBrindisi,id_shp_com="PRO_COM")
shape1=x$shp_lma

#generate a partition with a second set of parameters
out2= findClusters(LWCom=Brindisi, minSZ=1000,minSC=0.6,tarSZ=10000,tarSC=0.7)
out2_name=AssignLmaName(Brindisi,out2$lma,names.Brindisi)                    
x=CreateLMAShape(out2_name,comIDs="community",lmaIDs="LMA",shp_com=shpBrindisi,id_shp_com="PRO_COM")
shape2=x$shp_lma

## ----eval=F, warning=FALSE,message=FALSE--------------------------------------
#  EqualLmaPartition(out1$lma, out2$lma)

## ----eval=T, warning=FALSE,message=FALSE--------------------------------------
#Use the structure without names
stats_first=StatClusterData(out1$lma,c(50,0.3,100,0.4),1,Brindisi)
stats_second=StatClusterData(out2$lma,c(1000,0.6,10000,0.7),1,Brindisi)
#str(stats_first)
#str(stats_second)
stats_first$StatQuality$NbClusterUniqueCom
stats_second$StatQuality$NbClusterUniqueCom


## ----eval=T, warning=FALSE,message=FALSE--------------------------------------
comparison=CompareLMAsStat(list(out1,out2),Brindisi)

## ----eval=F,warning=FALSE,message=FALSE---------------------------------------
#  PlotLmaCommunity(list(out1_name,out2_name),"LMA","74014", shpBrindisi, "PRO_COM","my_full_path\\name_bmp_file.bmp")

## ----eval=T,echo=F ,warning=FALSE,message=FALSE,fig.align='center'------------
col.vec = c("red", "orange", "yellow")
  list.lma=list(out1_name,out2_name)
  shp_com=shpBrindisi
  lmaIDs="LMA"
  communityID="74014"
  id_shp="PRO_COM"

  # par(mfrow = c(1, 2))
  counter = 1
  lma = list.lma[[counter]]
  x = CreateLMAShape(lma, comIDs = "community", lmaIDs = lmaIDs, 
                     shp_com = shp_com, dsn = NULL, shp_com_name = NULL, 
                     id_shp_com = id_shp, outd = NULL, outf = NULL, bf = NULL, 
                     po = c("green", 1, 2, "red", 1, 2, 0.8, 2))
  indice = grep(lmaIDs, names(lma$clusterList))
  current.lma = lma$clusterList[community %in% communityID, 
                                indice, with = F]
  com.in.current.lma = lma$clusterList[unlist(lma$clusterList[, 
                                                              indice, with = F]) %in% unlist(current.lma), community]
  current.lma2 = list.lma[[2]]$clusterList[community %in% 
                                             communityID, indice, with = F]
  com.in.current.lma2 = list.lma[[2]]$clusterList[unlist(list.lma[[2]]$clusterList[, 
                                                                                   indice, with = F]) %in% unlist(current.lma2), community]
  new.com = setdiff(com.in.current.lma, com.in.current.lma2)
  indicecom = grep(id_shp, names(shp_com@data))
  indicelma = grep(lmaIDs, names(x$shp_lma))
  # plot(shp_com[shp_com@data[, indicecom] %in% com.in.current.lma,  ], border = "gray", main = counter)
  # plot(x$shp_lma[x$shp_lma@data[, indicelma] %in% current.lma, ], border = "red", add = T)
  # plot(shp_com[shp_com@data[, indicecom] %in% communityID,   ], border = "gray", col = col.vec[1], add = T)
  # 
mappa1=tm_shape(shp_com[shp_com@data[, indicecom] %in% com.in.current.lma,])+tm_borders("gray")
mappa1=mappa1+tm_shape(x$shp_lma[x$shp_lma@data[, indicelma] %in% current.lma, ])+tm_borders("red")
mappa1=mappa1+tm_shape(shp_com[shp_com@data[, indicecom] %in% communityID,   ])+tm_borders("gray")+tm_fill(col=col.vec[1])+tm_text("PRO_COM")
 
  
  
  counter = 2
  lma = list.lma[[counter]]
  x = CreateLMAShape(lma, comIDs = "community", lmaIDs = lmaIDs, 
                     shp_com = shp_com, dsn = NULL, shp_com_name = NULL, 
                     id_shp_com = id_shp, outd = NULL, outf = NULL, bf = NULL, 
                     po = c("green", 1, 2, "red", 1, 2, 0.8, 2))
  indice = grep(lmaIDs, names(lma$clusterList))
  current.lma = lma$clusterList[community %in% communityID, 
                                indice, with = F]
  com.in.current.lma = lma$clusterList[unlist(lma$clusterList[, 
                                                              indice, with = F]) %in% unlist(current.lma), community]
  current.lma1 = list.lma[[1]]$clusterList[community %in% 
                                             communityID, indice, with = F]
  com.in.current.lma1 = list.lma[[1]]$clusterList[unlist(list.lma[[1]]$clusterList[, 
                                                                                   indice, with = F]) %in% unlist(current.lma1), community]
  new.com = setdiff(com.in.current.lma, com.in.current.lma1)
  indicecom = grep(id_shp, names(shp_com@data))
  indicelma = grep(lmaIDs, names(x$shp_lma))
  # plot(shp_com[shp_com@data[, indicecom] %in% com.in.current.lma, ], border = "gray", main = counter)
  # plot(x$shp_lma[x$shp_lma@data[, indicelma] %in% current.lma, ], border = "red", add = T)
  # plot(shp_com[shp_com@data[, indicecom] %in% communityID, ], border = "gray", col = col.vec[1], add = T)
  # 
  mappa2=tm_shape(shp_com[shp_com@data[, indicecom] %in% com.in.current.lma, ])+tm_borders("gray")+tm_fill("yellow")+tm_text("PRO_COM")
  mappa2=mappa2+tm_shape(x$shp_lma[x$shp_lma@data[, indicelma] %in% current.lma, ])+tm_borders("red")
  mappa2=mappa2+tm_shape(shp_com[shp_com@data[, indicecom] %in% communityID, ])+tm_borders("gray")+tm_fill(col= "red")+tm_text("PRO_COM")

 mappa1
 
 mappa2
 
#tmap_arrange(mappa1,mappa2,ncol=2)

## ----eval=TRUE,warning=FALSE,message=FALSE------------------------------------
spatial_comp=LmaSpatialComparison(shape1,shape2)[]
str(spatial_comp)

## ----echo=TRUE,eval=TRUE,fig.align='center'-----------------------------------
lma_pop=AddStatistics(shpBrindisi@data[,c("PRO_COM","POP2001")], "PRO_COM",out1$lma,"community" )
head(lma_pop)
shp_stats=sp::merge(shape2,lma_pop,by.x="LMA",by.y="cluster")
tm_shape(shp_stats)+tm_borders("red")+tm_fill("POP2001")+tm_view(view.legend.position = c('right','bottom'))+tm_layout(legend.text.size=0.6)

## ----echo=FALSE---------------------------------------------------------------
options(warn=0)

