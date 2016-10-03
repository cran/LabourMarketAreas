utils::globalVariables(c("Code"
                         ,"lma.name"
                         ,"com.name"
                         ,"amount"
                         ,"residents"
                         ,"community_work"
                         ,"community_live"
                         ,"maxi"
                         ,"LMA"
                         ,"community"
                         ,"EMP_live"
                         ,"EMP_live_work"
                         ,"EMP_work"
                         ,"LMA_live"
                         ,"LMA_work"
                         ,"commuters"
                         ,"N"
                         ,"V1"
                         ,"cluster_live"
                         ,"cluster_work"
                         ,"lma.name.live"
                         ,"lma.name.work"
                         ,"cluster"
                         ,"Ncom"
                         ,"N_com"
                         ,"x"
                         ,"ID_PiecesLMA"
                         ,"validity"
                         ,"SC_demand_side"
                         ,"SC_supply_side"
                         ,"InternalCohesionFlows"
                         ,"InternalCohesionLink"
                         ,"ic"
                         ,"icc"
                         ,"cent"
                         ,"centrality"
                         ,"N_links_in"
                         ,"N_links_out"
                         ,"NbCentralComm"
                         ,"q_modularity"
                         ,"Residents"
                         ,"Workers"
                         ,"amount_live"
                         ,"amount_work"
                         ,"strengthOnesided"
                         ,"setClusterSwap"
                         ,"temp"
                         ,"cluster1"
                         ,"cluster2"
                         ,"moves"
                         ,"NoToFrom"
                         ,"live_work"
                         ,"orderRegroup"
                         ,"msc"
                         ,"Y"
                         ,"sizeMeasure"
                         ,"Z"
                         )
                       ,add=T)
##############################################################################
# 
# livework-cluster-onlyONS.r
# modified version of the CBS code to implement Coombes and Bond (2007)
###############################################################################
#
# livework-cluster.r
#
# (c) 2013 Centraal Bureau voor de Statistiek
# Author: Guido van den Heuvel (EHVL)
#
# This R script implements the clustering algorithm described on page 15 in
# Coombes & Bond (2007). The script has been developed on a 32 bits Windows 7 machine
# for R versions 2.10.1 and 2.15.2.
#
# Revision history:
# 29 july 2013      EHVL (creation)
# 30 august 2013  	EHVL - bugfix and comment update
# 30 august 2013  	EHVL - added code to calculate the total number of
#                   	     commuters in the input file dynamically
# 03 september 2013	EHVL - Added second clustering algorithm, described in [3], [4] and [5]
#                        - Added runScenario()
# 10 september 2013 EHVL - Added findBeta()
# 11 september 2013 EHVL - Cleanup
# 09 january 2014   EHVL - Treat cases correctly in which the number of residents or 
#                          the number of workers in a community is zero
#
# Literature:
# [1] Coombes, M., Bond, S., 2007, Travel-to-work-areas: the 2007 review, 
#     Office for National Statistics, ISBN 978 1 85774 676 1 
# [2] Coombers, M., 2013, FW: TTWA, e-mail correspondence with H. Keuning-van Oirschot,
#     12 and 16 agust 2013
# [3] Gouweleeuw, J., 2001, Cluster.pas, Pascal source implementing cluster algorithm
# [4] Gouweleeuw, J., 2013, Werking van het programma Cluster.pas
# [5] Gouweleeuw, J., 2002, ONDERZOEK NAAR DE ACTUALITEIT VAN DE HUIDIGE COROP-INDELING,
#     22 maart 2002
#
########################################################################################################
#   Further 2014 revision history
#   Authors: Michele D'Alo', Luisa Franconi and Daniela Ichim (Istat), July,16,2014
#########################################################################################################
#   In 2013 Eurostat has created a Task Force in order to develop harmonised European Local Market Areas.
#   The original script by CBS (livework-cluster.r) has been made available to Eurostat task force members in April 2014.
#   A script in Java developed by Dev-stat (Coombes et al.) that implements Coombes and Bond (2007) algorithm 
#   has also been provided to the Task Force members in April 2014.
#   In june 2014 the original code by CBS has been modified by Istat in order to reproduce the output of the Java code.
#  
#   The present file contains ONLY those CBS functions in the original livework-cluster.r that allow 
#   the user to find the LMA according to the 
#   Coombes and Bond (2007) algorithm; it does not contain all the other functions created for different purposes.
#   These modified versions have been developed by Istat in  
#   june 2014 after clarifications on specific points by Coombes in order to reproduce the output of the Java code (ttwa). 
#
#   The main differences introduced in the modifed version of the functions w.r.t. the original CBS code are related to:
#    1) the input and output phases: a new script has been created (LMA.R) where the user sets the parameters of the algorithm 
#       and call the function to run the algorithm and save the output in an .Rdata file for future analysis.
#       The four parameters relate to the coordinates of the 2 points in the (x,y) axies that identify the X-equation:
#       the coordinates of the minimum (x1,y1) and the coordinates of the target point (x2,y2). 
#    2) there is no need in this version of the code to create a separate file to provide the residents. They
#       are currently computed from the original flows between municipalities (communities).
#    3) the "X equation has been re-written to make it equal to the "validity function" built in the Java code.
#    4) following the java code and the clarification by Coombes, the regrouping of the communities in the cluster that needs to be dissolved 
#       is now done sequentially and not simultaneously as in the original version. The communities are regrouped according to the ascending order
#       of their identification number.
#    5) some particular cases eg zero residents, zero workers, all residents that work in other zones, ect. that were not 
#       treated in the original script are now specifically recognised and accounted for. 
#    6) the function globalmargins (allowing the user to make distinct analysis for different provinces)
#       has been deleted as it was not reputed important for the current scope. In the input file
#       the variable provNr should be kept and should be equal for the whole file as this variable is used in the scritpt.
#
#       The current version of the code allows to create a txt file via the sink() function. This has been implemented in order to 
#   monitor the behaviour of the R code w.r.t. the java code and understand where a specific community was
#   attached to a specific cluster (i.e. the story of each cluster). 
# 
#   Extreme cases:
#   If a municipality is present only in the list community_work
#   then its residents are set equal to 0. Communities with residents=0 will be written in the warning file, on the screen and in the sink file.
#   If a municipality exists which is not present in the input file of the flows (no inflow or outflow) 
#   then this community will be completely ignored by the algorithm without any warning.  
#   
#   The order in which the municipalities of a dissolved cluster are regrouped influences the result. 
#   We believe that a more meaningful order than the identification code currently implemented should be considered. 
#   In the function findcluster a further order is present but commented. In such code a new order of the municipalities is implemented:  
#   order by residents, decreasing, the first community assigned will be the one with the greatest number of residents.
#   The user may decide to delete the comment and try this option.
#   
#   15 July 2014  
#
##################################################################################################
#
# Further revision August 2014 
# Authors: Michele D'Alo', Luisa Franconi
#
#  13 August 2014 - a new order to analyse communities implemented according to Coombes (2014) 
#  18 August 2014 - the reserve list (i.e. the communities that lower the value of the validity of
#                   the cluster to which they are assigned) is created: output changed accordingly
#
#  Coombes and Wymer (2014)  - exchange of e-mails with Oliver HEIDEN (ESTAT) June 2014 
#  
# 
##################################################################################################

# findClusters: this function clusters municipalities based on commuter patterns between them.
# Municipalities with a large number of commuters working in one and living in the other
# are clustered together, resulting in clusters with a high degree of "self-containment",
# meaning that most residents of a cluster work in the same cluster.
#
# The algorithm used is described in Coombes & Bond (2007), page 15 and 
# Coombes and Wymer (2014) Personal communications July 2014- e-mail 


##this is the core function that calls all other functions


# 
findClusters <- function(LWCom,minSZ,minSC,tarSZ,tarSC, verbose=F,sink.output=NULL,trace=NULL,PartialClusterData=NULL) {
  
  #library(data.table)
  # Initialization
  #filenameLWCom <- paste(inDir, filename, sep="\\")
  #LWCom <- readLWCommunities(filenameLWCom)
  #DT
  #LWCom <- fread(filenameLWCom)
  #dt
  
  ## the communities are ordered by the code of community_live
  
  param=c(minSZ,minSC,tarSZ,tarSC)
  
  LWCom<-data.table( LWCom)
  LWCom=LWCom[order(community_live)]
  
  ### end of ordering by code_live
  
  ###############16/08/2016
  #controls on names and values on LWCom
  n=names(LWCom[1])
  if(any(sort(n)!=c("amount","community_live","community_work"))){
    stop("ERROR: please check the names of the commuting flows data.")
  }
  
  if(!all(is.integer(LWCom[,community_live]))){
    stop("ERROR: community_live is not always integer.")
    #end if
  }
  
  if(!all(is.integer(LWCom[,community_work]))){
    stop("ERROR: community_work is not always integer.")
    #end if
  }
  
  if(!all(LWCom[,community_live>0])){
    stop("ERROR: community_live is not always positive.")
    #end if
  }
  if(!all(LWCom[,community_work>0])){
    stop("ERROR: community_work is not always positive.")
    #end if
  }
  
  if(!all(is.numeric(LWCom[,amount]))){
    stop("ERROR: amount is not always integer.")
    #end if
  }
  
  if(minSZ<=0 | minSC<=0 | tarSZ<=0| tarSC<=0){
    stop("ERROR self-containment and target parameters should be positive numbers")
    #end if min
  }
  
  ###############16/08/2016
  
  
  #list of the communities
  
  
  LIST.COM=data.table(c(LWCom[,community_live],LWCom[,community_work]))
  LIST.COM=unique(LIST.COM)[order(V1)]
  #find the number of residents from the input file if a municipality is present only in the list community_work
  ## its residents are set equal to 0
  
  
  residents=setcolorder(LWCom[,list(residents=sum(amount)),by=list(Code=community_live)],c("residents","Code"))
  residents=merge(residents,LIST.COM,by.x="Code",by.y="V1",all=T)
  residents[is.na(residents),residents:=0]
  #18/5/2016
  workers=setcolorder(LWCom[,list(workers=sum(amount)),by=list(Code=community_work)],c("workers","Code"))
  workers=merge(workers,LIST.COM,by.x="Code",by.y="V1",all=T)
  workers[is.na(workers),workers:=0]
  #18/5/2016
  #dt
  
  
  
  ####check number of residents and warning if resident=0
  ## communities with residents=0 will be written in the warning file, on the screen and in the sink file.
  
  codes.0<-sort(unique(residents$Code[residents$residents==0]))
  codes.0<-c(codes.0,sort(unique(workers$Code[workers$workers==0])))
  ####27/05/2016
  temp=LWCom
  temp=merge(temp,residents,by.x="community_live",by.y="Code",all=T)
  temp=merge(temp,workers,by.x="community_work",by.y="Code",all=T)
  temp=temp[amount==residents & amount==workers & community_live==community_work]
  codes.0=c(codes.0,temp[,community_work])
  ##fine 27/05/2016
  
  codes.0=sort(codes.0)
  #18/5/16
  zero.list=list(Communities=NULL,LWCom=NULL,Residents=NULL,
                 Workers=NULL)
  if(length(codes.0)>=1){
    print("WARNING:")
    print("Please check the zero.list component of the output. It is not empty.")
    print("The following communities will NOT assigned by the algorithm:")
    print(codes.0)
    # 16/5/16	
    
    zero.list$Communities=codes.0
    zero.list$LWCom=LWCom[community_work%in%codes.0 | community_live%in%codes.0 ,]
    zero.list$Residents=residents[Code%in%codes.0]
    zero.list$Workers=workers[Code%in%codes.0]
    #write.table(codes.0,file=file.codici0,append=F,row.names=F,col.names=T,sep=";",quote=F)
    #write.table(t(names(LWCom)),file=file.codici0,append=T,row.names=F,col.names=F,sep=";",quote=F)
    #write.table(LWCom[LWCom$community_work%in%codes.0 ,],file=file.codici0,append=T,row.names=F,col.names=F,sep=";",quote=F)
    
    residents<-residents[!(Code%in%codes.0),]
    LWCom<-LWCom[!(community_live%in%codes.0),]
    LWCom<-LWCom[!(community_work%in%codes.0),]
    LIST.COM<-LIST.COM[!(LIST.COM[,1]%in%codes.0),]
    #end if length(codes.0)>1
  }
  
  ##13/08 add a fictitious community to act as "cluster=0" where to assign the communities 
  #that do not increase the validity of the "best corresponding cluster"
  ###the fictitious community should be a number NEVER registered as community. 
  ##Here we implement the maximum community number multiplied by 10.
  #This fictitious community will not be reported as community with zero residents and 
  #will be deleted at the end of the computations.
  # Such fictitious community will become the cluster zero i.e. the reserve list
  #fict.community=max(as.numeric(LIST.COM[,1]))*10
  #LWCom=rbind(LWCom,rep(fict.community,ncol(LWCom)))
  #LWCom$amount[LWCom$community_live==fict.community]=0
  #residents=rbind(residents,c(fict.community,0))
  #dt  
  fict.community=LIST.COM[,max(V1)*10]
  LWCom=rbind(LWCom,data.table(matrix(rep(fict.community,ncol(LWCom)),1)),use.names=F)
  #14/12 utilizzato use.names=F perche' altrimenti si aggiungono altre tre colonne  
  LWCom[community_live==fict.community,amount:=0]
  residents=rbind(residents,data.table(matrix(c(fict.community,0),1)),use.names=F)
  #dt
  
  
  ####16/08/2016
  ##the initial creation of clusterData was replaced by 
  ##the function CreateClusterDataBelow
  clusterData=CreateClusterData(LWCom,residents=residents)
  communitiesList <- clusterData$clusterList[,community]
  #####16/08/2016
  
  
  
  fict.cluster=clusterData$clusterList[community==fict.community, cluster]
  clusterData$clusterList[cluster==fict.cluster, cluster:=0]
  clusterData$LWClus[cluster_live==fict.cluster, cluster_live:=0]
  clusterData$LWClus[cluster_work==fict.cluster, cluster_work:=0]
  clusterData$marginals[cluster==fict.cluster, cluster:=0]
  
  ####  create the data.frame that allows to order communities by workers
  ###the data frame will not be modified by iterations
  CommunityWorkersOrig=merge(clusterData$clusterList,clusterData$marginals,by="cluster")[,list(community,amount_work)]
  ####fine 
  # 13/08 compute internal flows
  
  Community_live_work= LWCom[community_live==community_work,list(community=community_live,live_work=amount)]
  # 
  #call to the sink function to monitor the behavior of each municipality and each cluster
  # 
  
  if(!is.null(sink.output)){
    sink(file=sink.output,split=T)
  }
  # Main loop
  counter <- 1
  
  ## 13/5/2016
  reserve.list=list()
  counter.list=1
  ComNotAssigned.list=list()
  clusterDataInterm=list()
  communitiesMovements=data.table(community=communitiesList)
  communitiesMovements[,moves:=0]
  setkey(communitiesMovements,community)
  traceeval=!is.null(trace)
  #listi=list()
  
  
  ####16/08/2016
  ##introduced the PartialClusterData
  if(!is.null(PartialClusterData)){
    #rm(clusterData)
    clusterData=copyClusterData(PartialClusterData)
    #end if PartialClusterData
  }
  ####16/08/2016

  
  repeat {
  # for(cicione in 1:190){ 
    ##13/08 consider only the first component of the getLeastSelfContained() output
#     rm(leastSelfContainedFULL)
#     rm(leastSelfContained)
    leastSelfContainedFULL <- getLeastSelfContained(clusterData$LWClus, clusterData$marginals,minSZ,minSC,tarSZ,tarSC)
    leastSelfContained=leastSelfContainedFULL[[1]]
#     listi[[cicione]]=leastSelfContainedFULL[[2]]
#     save(listi,file="listi55.Rdata")
    
    if (verbose) {
      print(leastSelfContained$validity, digits = 20)
    }
    if (leastSelfContained$validity>=1) { 
      print("The algorithm has converged.")
      break
    }
    
    #number of municipalities to be regrouped
    
    com.cluster2dissolve<-clusterData$clusterList[cluster%in%leastSelfContained$cluster,]
    n.com.cluster2dissolve<-nrow(com.cluster2dissolve)
    
    
    if(n.com.cluster2dissolve>1){
      ##### 13/08 Order as described in Coombes and Wymer (2014) 
      WorkersFromOutside=LWCom[xor(LWCom$community_live%in%com.cluster2dissolve$community, LWCom$community_work%in%com.cluster2dissolve$community),]
      ####NoToFRom = workers from outside the current cluster
      WorkersFromOutside2=WorkersFromOutside[,list(NoToFrom=sum(amount)),by=list(community=community_work)]
      
      com.cluster2dissolve=merge(com.cluster2dissolve,WorkersFromOutside2,by="community",all.x=T)
      com.cluster2dissolve[is.na(NoToFrom),NoToFrom:=0]
      com.cluster2dissolve= merge(com.cluster2dissolve,Community_live_work, by="community",all.x=T)
      com.cluster2dissolve[is.na(live_work),live_work:=0]
      ####QUI AGGIUNTO IL SALVATAGGIO
      com.cluster2dissolve=com.cluster2dissolve[,orderRegroup:=NoToFrom + residents -live_work ][order(-orderRegroup)][,list(community,cluster,residents)]
      
      #end if n.com.cluster2dissolve>1 (the ordering is performed only when there are more than one 
      ##community in the dissolved cluster)
    }
    
    ### 13/08 depending on the type of the assignment of the first community,
    ### the cluster may be fully dissolved and its communities assigned to different clusters
    ### or only the first community may be excluded from the dissolved cluster and 
    #assigned to another cluster
    
    clusterData <- dissolveCluster(clusterData, leastSelfContained[,cluster], LWCom)
    
    if(n.com.cluster2dissolve==1){
      #13/08 the assignment to the new cluster will be finalized only if 
      #the validity of the new cluster is not less than the validity of the old cluster
      
      ##16/08/16 aggiunto [[1]] per modifica regroupDissolved e tirato anche il candidate cluster
      otim=regroupDissolved(copy(clusterData))
      if(is.list(otim)){
        clusterData.new <- otim[[1]]
        candcluster=otim[[2]]
      }else{clusterData.new=otim}
      
      
      #rm(otim)
      ###16/08/16
      
      
      ##the output of the regroupDissolved is a list if the assignment is not the zero
      ##cluster and a numeric value otherwise
      if(is.list(clusterData.new)){
        
        #compute the validity of the new cluster (i.e. the second component of the getLeastSelfContained function)
        #16/08/2016/ OLD validity.new=getLeastSelfContained(copy(clusterData.new)$LWClus, copy(clusterData.new)$marginals, minSZ,minSC,tarSZ,tarSC)
        sotto=c(candcluster$cluster2,com.cluster2dissolve$cluster)
        validity.new=getLeastSelfContained(copy(clusterData.new)$LWClus[cluster_live%in%sotto | cluster_work%in%sotto,], copy(clusterData.new)$marginals[cluster%in%sotto,], minSZ,minSC,tarSZ,tarSC)
        ###end 16/08/2016
        
#         
#         #13/08 order the validity dataframes
#         validity.new[[2]]=validity.new[[2]][order(cluster)]
#         leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][order(cluster)] 
#         
        #16/08/2016 cancellata la riga sotto  -- non si puo fare calcolando la validity solo per alcuni clusters
        #if(setdiff(leastSelfContainedFULL[[2]][,cluster],validity.new[[2]][,cluster])!=leastSelfContainedFULL[[1]][,cluster]){stop("ERROR the dissolved cluster is not the only one disappeared!!!")}
        
        ##16/08/2016
        #aggiunta
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][cluster%in%sotto]
        ###end 16/08/2016
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][!cluster%in%leastSelfContainedFULL[[1]][,cluster]]
        ###13/08
        ###if validity of the new clusters is greater or equal
        ###than the validity of the old clusters, then the assignment is performed
        
        if(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity])){
          
          rm(clusterData)
          clusterData=copy(clusterData.new)
          communitiesMovements[community==com.cluster2dissolve[1,community],moves:=moves+1]
          
#           ###08/04/2016 check cluster zeros
#           print("no reserve list case;  a single community")
#           print("number of cluster zero occurences: ")
#           print(dim(clusterData$marginals[cluster==0]))
#           ###08/04/2016 check cluster zeros
          
          #end if all validity
        }
        ## 
        ## 13/08 if the validities are not all improved, the community is assigned to cluster=0
        if(!(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity]))){  
          vec2out0=c("A",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[1,community])
          # 16/5/2016
          reserve.list[[counter.list]]=vec2out0
          counter.list=counter.list+1
          
          clusterData$clusterList[cluster==-1,cluster:=0]
          clusterData$LWClus[cluster_live==-1,cluster_live:=0]
          clusterData$LWClus[cluster_work==-1,cluster_work:=0]
          clusterData$marginals[cluster==-1,cluster:=0]
          
#           ###04/08/2016 check cluster zeros
#           print("A case; position on reserve list")
#           print(counter.list-1)
#           print("number of cluster zero occurences: ")
#           print(dim(clusterData$marginals[cluster==0]))
#           ###04/08/2016 check cluster zeros
          
          #clusterData$LWClus[,temp:=NULL]
          
          #08/08/2016 - add recompact
          clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
          clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
          ##end 08/08/2016
          
          #end if all validity
        }
        
        #end if is.list
      }
      
      if(is.numeric(clusterData.new)){
        ## 26/08 
        vec2out0=c("B",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[1,community])
        
        # 13/5/2016
        reserve.list[[counter.list]]=vec2out0
        counter.list=counter.list+1
        
        clusterData$clusterList[cluster==-1,cluster:=0]
        clusterData$LWClus[cluster_live==-1,cluster_live:=0]
        clusterData$LWClus[cluster_work==-1,cluster_work:=0]
        clusterData$marginals[cluster==-1,cluster:=0]
        #clusterData$LWClus[,temp:=NULL]
        
#         ###04/08/2016 check cluster zeros
#         print("B case; position on reserve list")
#         print(counter.list-1)
#         print("number of cluster zero occurences: ")
#         print(dim(clusterData$marginals[cluster==0]))
#         ###04/08/2016 check cluster zeros
        
        
        #08/08/2016 - add recompact
        clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
        clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
        ##end 08/08/2016
        
        
        #end if is.numeric
      }
      #
      #end if n.com.cluster2dissolve==1
    }
    
    
    #if there is more than one municipality in the cluster then the municipalities need to be ordered before regrouping
    if(n.com.cluster2dissolve>1){
      ##13/08 The implementation is the following:
      #Using the communities in the dissolved cluster D, using the order defined above, 
      ## the first community is assigned to a cluster C. 
      #If C=0 (cluster zero), then all the other communities in the dissolved cluster are assigned to their
      ##corresponding cluster (they may be different clusters --- there is no constraint).
      ## If C!=0, the dissolved cluster D is regrouped. The regrouped cluster will contain all the communities, 
      ##except the first one in D (the one which was already assigned).
      
      ###assign the first community
      ncom=1
      index.com.2diss=clusterData$clusterList[community==com.cluster2dissolve[ncom,community],cluster]
      ##16/08/2016 aggiunto [[1]] per modifica regroupDissolved
      #clusterData.new <- regroupDissolved.ncom(copy(clusterData),index.com.2diss)[[1]]
      
      otim=regroupDissolved.ncom(copy(clusterData),index.com.2diss)
      if(is.list(otim)){
        clusterData.new <- otim[[1]]
        candcluster=otim[[2]]
      }else{clusterData.new=otim}
      #rm(otim)
      
      ###end 16/08/2016
      
      ## 13/8 the output of the regroupDissolved.ncom is a list if the assignment is not the zero
      ## cluster and a numeric value otherwise
      
      if(is.list(clusterData.new)){
        #13/08 The assignment is performed only if the validity is improved
        
        ###16/08/2016
        #OLD validity.new=getLeastSelfContained(copy(clusterData.new)$LWClus, copy(clusterData.new)$marginals, minSZ,minSC,tarSZ,tarSC)
        sotto=c(candcluster$cluster2,com.cluster2dissolve$cluster)
        validity.new=getLeastSelfContained(copy(clusterData.new)$LWClus[cluster_live%in%sotto | cluster_work%in%sotto,], copy(clusterData.new)$marginals[cluster%in%sotto,], minSZ,minSC,tarSZ,tarSC)
        
        ####end 16/08/2016
#         
#         validity.new[[2]]=validity.new[[2]][order(cluster)]
#         leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][order(cluster)]  
#         
        ###16/08/2016 cancellata la riga sotto perche non piu' possibile
        #if(setdiff(leastSelfContainedFULL[[2]][,cluster],validity.new[[2]][,cluster])!=leastSelfContainedFULL[[1]][,cluster]){stop("ERROR the dissolved cluster is not the only one disappeared!!!")}
        
        ##16/08/2016
        #aggiunta
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][cluster%in%sotto]
        ###end 16/08/2016
        
        # 15/12/15
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][!cluster%in%leastSelfContainedFULL[[1]][,cluster]]
        
        ### 13/08
        ###if each validity of the new clusters is greater or equal
        ###than the validity of the old clusters, then the assignment of the first is performed
        
        if(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity])){
          
          rm(clusterData)
          clusterData=copy(clusterData.new)
          communitiesMovements[community==com.cluster2dissolve[ncom,community],moves:=moves+1]
          
#           ###04/08/2016 check cluster zeros
#           print("no reserve list case;  many communities")
#           print("number of cluster zero occurences: ")
#           print(dim(clusterData$marginals[cluster==0]))
#           ###04/08/2016 check cluster zeros
          
          
          #08/08/2016 - add recompact
          clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
          clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
          ##end 08/08/2016
          
          #end if all validity
        }
        
        ## 13/08 if the validities are not all improved, the first community is assigned to cluster=0
        if(!(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity]))){
          
          vec2out0=c("C",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community],com.cluster2dissolve[ncom+1,community])
          
          # 13/5/2016
          reserve.list[[counter.list]]=vec2out0
          counter.list=counter.list+1
          
          clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
          clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
          clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
          clusterData$marginals[cluster==index.com.2diss,cluster:=0]
          
          
#           ###04/08/2016 check cluster zeros
#           print("C case; position on reserve list")
#           print(counter.list-1)
#           print("number of cluster zero occurences: ")
#           print(dim(clusterData$marginals[cluster==0]))
#           ###04/08/2016 check cluster zeros
          
          
          #08/08/2016 - add recompact
          clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
          clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
          ##end 08/08/2016
          
          #end if all validity
        }
        
        #13/08 A side note on the implementation. When we regroup i.e. the negative clusters (the remaining communities of the dissolved cluster)
        ## become positive, again. Here we use the same label of the initially dissolved cluster.
        ##This reasoning/implementation is valid ONLY if one cluster is dissolved at each iteration.
        ##In case more clusters are dissolved at a single iteration 
        #(when the minimum validity is reached by different clusters), the rows below MUST be changed.
        
        clusterData$clusterList[cluster<0, cluster:=leastSelfContainedFULL[[1]][1,cluster]]
        clusterData$LWClus[cluster_live<0,cluster_live:=leastSelfContainedFULL[[1]][1,cluster]]
        clusterData$LWClus[cluster_work<0, cluster_work:=leastSelfContainedFULL[[1]][1,cluster]]
        clusterData$marginals[cluster<0,cluster:=leastSelfContainedFULL[[1]][1,cluster]]
        
        #end if type of new cluster
      }
      
      if(is.numeric(clusterData.new)){
        
        vec2out0=c("D",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community],com.cluster2dissolve[ncom+1,community])
        
        # 13/5/2016
        reserve.list[[counter.list]]=vec2out0
        counter.list=counter.list+1 
        
        clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
        clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
        clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
        clusterData$marginals[cluster==index.com.2diss,cluster:=0]
        
        #29/04/2015
        
        clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
        clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
        
#         ###04/08/2016 check cluster zeros
#         print("D case; position on reserve list")
#         print(counter.list-1)
#         print("number of cluster zero occurences: ")
#         print(dim(clusterData$marginals[cluster==0]))
#         ###04/08/2016 check cluster zeros
        
        
         #end 29/04/2015
        
        ##the other communities in the dissolved cluster are assigned, one by one.
        for(ncom in 2:n.com.cluster2dissolve){
#           rm(leastSelfContainedFULL)
#           rm(leastSelfContained)
          
          leastSelfContainedFULL <- getLeastSelfContained(clusterData$LWClus, clusterData$marginals, minSZ,minSC,tarSZ,tarSC)
          leastSelfContained=leastSelfContainedFULL[[1]]
          
          #29/04/2015
          
          rm(index.com.2diss)
          index.com.2diss=leastSelfContained[,cluster]
          #      
          ##16/08/16 aggiunto [[1]] per modifica regroupDissolved
          #OLD clusterData.p <- regroupDissolved.ncom(copy(clusterData),index.com.2diss)[[1]]
          
          otim=regroupDissolved.ncom(copy(clusterData),index.com.2diss)
          if(is.list(otim)){
            clusterData.p <- otim[[1]]
            candcluster=otim[[2]]
          }else{clusterData.p=otim}
          ###16/08/16
          
          
          if(is.list(clusterData.p)){
            ##13/08 also for the other communities, the assignment is performed only if the validity is improved.
            ##otherwise the communities are assigned to cluster=0, as usual
            
            ##16/08/2016
          
            # old validity.p=getLeastSelfContained(clusterData.p$LWClus, clusterData.p$marginals, minSZ,minSC,tarSZ,tarSC)
            if(is.list(otim)){sotto=c(candcluster$cluster2)}else{sotto=0}
            
            validity.p=getLeastSelfContained(clusterData.p$LWClus[cluster_live%in%sotto | cluster_work%in%sotto,], clusterData.p$marginals[cluster%in%sotto,], minSZ,minSC,tarSZ,tarSC)
            #end 16/08/2016
            
#             validity.p[[2]]=validity.p[[2]][order(cluster)]
#             leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][order(cluster)]  
#             ####18/05/2016 nella riga sotto cambiato validity.new con validity.p
            ###16/08/2016 cancellata la riga sotto perche non piu' possibile
            #if(setdiff(leastSelfContainedFULL[[2]][,cluster],validity.p[[2]][,cluster])!=leastSelfContainedFULL[[1]][,cluster]){stop("ERROR the dissolved cluster is not the only one disappeared!!!")}
            
            
            ##16/08/2016
            #aggiunta
            leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][cluster%in%sotto]
            ###end 16/08/2016
            
            #15/12/15
            leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][!cluster%in%leastSelfContainedFULL[[1]][,cluster]]
            #15/12/15 
            
            if(all(validity.p[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity])){
              rm(clusterData)        
              clusterData=copy(clusterData.p)
              communitiesMovements[community==com.cluster2dissolve[ncom,community],moves:=moves+1]
              
              #end if all validity
            }
            
            ##13/08
            ##if the validities are not all improved, the community is assigned to cluster=0
            if(!(all(validity.p[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity]))){
              
              vec2out0=c("E",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community])
              # 13/5/2016
              reserve.list[[counter.list]]=vec2out0
              counter.list=counter.list+1
              
              clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
              clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
              clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
              clusterData$marginals[cluster==index.com.2diss,cluster:=0]
              
              clusterData$LWClus[,temp:=NULL]
              
              
              
              #29/04/2015
              clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
              #18/05/2016 aggiunto list(amount=sum(amount)) al posto di amount=sum(amount)
              clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
              
#               ###04/08/2016 check cluster zeros
#               print("E case; position on reserve list")
#               print(counter.list-1)
#               print("number of cluster zero occurences: ")
#               print(dim(clusterData$marginals[cluster==0]))
#               ###04/08/2016 check cluster zeros
#               
              
              
              #end if all validity
            }
            
            #end if is.list clusterdata.p
          }
          if(is.numeric(clusterData.p)){
            vec2out0=c("F",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community])
            # 13/5/2016
            reserve.list[[counter.list]]=vec2out0
            counter.list=counter.list+1
            
            clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
            clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
            clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
            clusterData$marginals[cluster==index.com.2diss,cluster:=0]
            
            clusterData$LWClus[,temp:=NULL]
            #29/04/2015
            clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
            #18/05/2016 aggiunto list(amount=sum(amount)) al posto di amount=sum(amount) nella riga sotto
            clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
            
#             ###04/08/2016 check cluster zeros
#             print("F case; position on reserve list")
#             print(counter.list-1)
#             print("number of cluster zero occurences: ")
#             print(dim(clusterData$marginals[cluster==0]))
#             ###04/08/2016 check cluster zeros
#             
       
            #end if is.numeric clusterdata.p
          }
          
          #end for ncom
        }
        
        #end if clusterCheck==0
      }
      
      #end if n.com.cluster2dissolve>1 
    }
    
    
    
    
    
    # Simple logging
    if (verbose) {
      print("end of loop number ")
      print(counter)
    }
    
    if(traceeval){ 
      if(counter%%trace==0){
        clusterDataInterm[[length(clusterDataInterm)+1]]=list(lma=clusterData,param=param)
        #end if counter multiplo di trace
      }
      #end if trace
    }
    counter <- counter + 1
    
    #end repeat 
  }
  
  
  if(traceeval){
  clusterDataIterations=clusterDataInterm
  save(clusterDataIterations,file=paste(minSZ,minSC,tarSZ,tarSC,"interm.clusterData.Rdata",sep=" "))
  #end if traceeval
  }
  
  ###08/08/2016 - add recompact
  clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
  clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  ###08/08/2016
  
  if(!is.null(sink.output)){
    sink()
  }
  #  
  
  
  ##13/08 eliminate the fictitious community but not the cluster0 
  clusterData$clusterList=clusterData$clusterList[community!=fict.community]
  ####keep the output before the assignment of the communities in the zero cluster
  clusterDataBeforeZeroCluster=copy(clusterData)
  ##only for the communities in cluster zero, 
  com.cluster2dissolve<-clusterData$clusterList[cluster==0]
  n.com.cluster2dissolve<-nrow(com.cluster2dissolve)
  if(n.com.cluster2dissolve>0){
    ##a) dissolve cluster zero
    
    clusterData <- dissolveCluster(clusterData, 0, LWCom)
    
    ##b) order according to Coombes and Wymer (2014)
    if(n.com.cluster2dissolve>1){
      
      #12/15/12 perche' le due righe qui sotto sono uguali??   
      WorkersFromOutside=LWCom[xor(community_live%in%com.cluster2dissolve[,community], community_work%in%com.cluster2dissolve[,community])]
      
      WorkersFromOutside2=WorkersFromOutside[,list(NoToFrom=sum(amount)),by=list(community=community_work)]
      com.cluster2dissolve=merge(com.cluster2dissolve,WorkersFromOutside2,by="community",all.x=T)
      com.cluster2dissolve[is.na(NoToFrom),NoToFrom:=0]
      
      com.cluster2dissolve= merge(com.cluster2dissolve,Community_live_work, by="community",all.x=T)
      
      com.cluster2dissolve[is.na(live_work),live_work:=0]
      
      com.cluster2dissolve[,orderRegroup:=  NoToFrom + residents -live_work ]
      
      com.cluster2dissolve=com.cluster2dissolve[order(-orderRegroup)]
      
      # 15/12/15
      #    com.cluster2dissolve=com.cluster2dissolve[,c("community","cluster","residents")]
      com.cluster2dissolve=com.cluster2dissolve[,list(community,cluster,residents)]
      
      #end if n.com.cluster2dissolve>1 (the ordering is performed only when there are more than one 
      ##community in the dissolved cluster)
    }
    
    
    for(j in 1:n.com.cluster2dissolve){
      
      ##c) try the assignment to an existing cluster, 
      ##without taking into account the validity of the new cluster
      
      index.com.2diss=clusterData$clusterList[community==com.cluster2dissolve[j,community],cluster]
      ##16/08/16 aggiunto [[1]] per modifica regroupDissolved
      clusterData.new <- regroupDissolved.ncom(clusterData,index.com.2diss)[[1]]
      
      if(is.list(clusterData.new)){clusterData=clusterData.new}
      if(is.numeric(clusterData.new)){
        
        ComNotAssigned.list[[length(ComNotAssigned.list)+1]]=com.cluster2dissolve[j,community]
        
        #end if is.numeric
      }
      
      #end for j ncom.2dissolve
    }
    
    
    #end if com2dissolve>0
  }
  
  
  #  calculate in the marginals data frame some useful quantities: self flow and both self containments for each cluster 
  
  ######################16/08/2016
  ###removed the statistics as we introduced a special function to compute them
#   val.data=getLeastSelfContained(clusterData$LWClus, clusterData$marginals,minSZ,minSC,tarSZ,tarSC)[[2]][,list(cluster,validity)]
#   
#   
#   clusterData$marginals=merge(clusterData$marginals,val.data,by="cluster",all=T)
#   
#   amount_live_work<-clusterData$LWClus[cluster_live==cluster_work,list(cluster_live,amount)]
#   
#   clusterData$marginals<-merge(clusterData$marginals, amount_live_work,by.x="cluster",by.y="cluster_live",all=T)
#   names(clusterData$marginals)[names(clusterData$marginals)=="amount"]="amount_live_work"
#   
#   
#   clusterData$marginals[,SCA:=amount_live_work/amount_work]
#   clusterData$marginals[,SCO:=amount_live_work/amount_live]
#   
#   
#   colnames(clusterData$marginals)<-c("LMA", "EMP_live","EMP_work","validity", "EMP_live_work","SC_demand_side","SC_supply_side")
#   colnames(clusterData$clusterList)<-c("community","LMA","EMP_live")
#   colnames(clusterData$LWClus)<-c("LMA_live","LMA_work","commuters")
#   
#   
#   ##Finalization of output (remove useless cols and rows)
#   clusterData$marginals=clusterData$marginals[LMA>0]
#   #change col order
#   setcolorder(clusterData$marginals, c( "LMA", "EMP_live","EMP_work","EMP_live_work", "validity","SC_demand_side","SC_supply_side"))
#   #add nb of communities
#   nbcoms=clusterData$clusterList[,list(N_com=.N),by=LMA]
#   
#   clusterData$marginals=merge(clusterData$marginals,nbcoms,by="LMA")
#   
#   
#   ##13/08 the same as above for clusterDataBeforeZeroCluster
#   
#   val.data=getLeastSelfContained(clusterDataBeforeZeroCluster$LWClus, clusterDataBeforeZeroCluster$marginals,minSZ,minSC,tarSZ,tarSC)[[2]][,list(cluster,validity)]
#   
#   clusterDataBeforeZeroCluster$marginals=merge(clusterDataBeforeZeroCluster$marginals,val.data,by="cluster",all=T)
#   amount_live_work<-clusterDataBeforeZeroCluster$LWClus[cluster_live==cluster_work,list(cluster_live,amount)]
#   
#   clusterDataBeforeZeroCluster$marginals<-merge(clusterDataBeforeZeroCluster$marginals, amount_live_work,by.x="cluster",by.y="cluster_live",all=T)
#   names(clusterDataBeforeZeroCluster$marginals)[names(clusterDataBeforeZeroCluster$marginals)=="amount"]="amount_live_work"
#   
#   clusterDataBeforeZeroCluster$marginals[,SCA:=amount_live_work/amount_work]
#   clusterDataBeforeZeroCluster$marginals[,SCO:=amount_live_work/amount_live]
#   
#   
#   
#   colnames(clusterDataBeforeZeroCluster$marginals)<-c("LMA", "EMP_live","EMP_work","validity", "EMP_live_work","SC_demand_side","SC_supply_side")
#   colnames(clusterDataBeforeZeroCluster$clusterList)<-c("community","LMA","EMP_live")
#   colnames(clusterDataBeforeZeroCluster$LWClus)<-c("LMA_live","LMA_work","commuters")
#   
#   
#   
  
  # Finalization
  if (verbose) {
    print(clusterData$clusterList)
  }
  
  
  ####13/08 -- keep the final clusters and the clusters before 
  ##the assignment of the zero cluster communities
  
  
  out=list(lma=clusterData,
           lma.before0=clusterDataBeforeZeroCluster,
           reserve.list=reserve.list,
           comNotAssigned=ComNotAssigned.list,
           zero.list=zero.list,
           communitiesMovements=communitiesMovements,
           param=c(minSZ,minSC,tarSZ,tarSC))
  
  return(out)
}
####in the follow the functions used into the core function are reported 

### ---------------









# mergeCluster: updates clusterData by merging the clusters in cluster1 and cluster2. A simple wrapper
# around the individual merge functions.
mergeCluster <- function(clusterData, cluster1, cluster2) {
  #13/5/2016
  
  
  #13/5/16     clusterList
#   index <- match(clusterData$clusterList$cluster, cluster1)
#   
#   clusterList<- clusterData$clusterList
#   clusterList[cluster%in%cluster1,cluster:=cluster2[index[!is.na(index)]]]
#   rm(index)   
  
  ###16/08/2016
  ##changed the four lines above in data.table
  clusterList=clusterData$clusterList
#   print(head(clusterList))
#   print(head(clusterList))
  clusterList[cluster==cluster1,cluster:=cluster2]
  ###end 16/08/2016
  
  
  #13/5/16   LWCLUS
  
#   index_live <- match(clusterData$LWClus$cluster_live, cluster1)
#   index_work <- match(clusterData$LWClus$cluster_work, cluster1)
#   LWClus=clusterData$LWClus
#   LWClus[LWClus$cluster_live %in% cluster1, cluster_live:=cluster2[index_live[!is.na(index_live)]]]
#   LWClus[LWClus$cluster_work %in% cluster1, cluster_work:=cluster2[index_work[!is.na(index_work)]]]
#   LWClus=LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
#   rm(index)
  
  ###16/08/16
  ##change the seven lines above in data.table
  LWClus=clusterData$LWClus
  LWClus[cluster_live==cluster1, cluster_live:=cluster2]
  LWClus[cluster_work==cluster1, cluster_work:=cluster2]
  LWClus=LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  ###end 16/08/2016
  
  
  #13/5/16   MArginals
#   index <- match(clusterData$marginals$cluster, cluster1)  
#   marginals=clusterData$marginals
#   marginals[marginals$cluster %in% cluster1, cluster:=cluster2[index[!is.na(index)]]] 
#   marginals=marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
#   
  ###16/08/2016
  ##change the four lines above in data.table
  marginals=clusterData$marginals
  marginals[cluster==cluster1, cluster:=cluster2] 
  marginals=marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]

  ##end 16/08/2016
  
  #16/08/16 - aggiunto if sotto per eliminare warning inutili
  
  
  return(list(clusterList=clusterList,  LWClus=LWClus , marginals=marginals))
#end function mergecluster
  }






# getLeastSelfContained: determines the cluster for which the "X-equation" (see [1]) is minimal.
# The X-equation is as follows (see [2]):
#
#     X = Y * Z, with
#
#     Y = (1 / s_{target}) min(s_{s,A}, s_{d,A}, s_{target}), with
#     s_{s,A} = C_AA / R_A the supply self-containment, and
#     s_{d,A} = C_AA / W_A the demand self-containment
#
#     Z = 1 - (1-s_{min} / s_{target}) max((R_{target} - R_A) / (R_{target} - R_{min}), 0)
#
# The parameters s_{min}, s_{target}, R_{min} and R_{target} can be freely chosen, as long as
#     0 <= s_{min} < s_{target} <= 1, and
#     0 <= R_{min} < R_{target}
#
# These determine the minimum allowed values and the target values for the self-containment and
# cluster size, respectively. If R_A >= R_{target} and s_A >= s_{target}, then the cluster is ok;
# if these are not satisfied, the combination of R_A and s_A determines whether the cluster is ok.
# A cluster is ok if X >= s_{min} / s_{target}
#####################################################################################################################
# Note: this function has been substituted by the modified version below in order to IMPLEMENT THE validity FUNCTION
#AS IMPLEMENTED IN THE JAVA CODE
######################################################################################################################
getLeastSelfContained <- function(LWClus, marginals,minSZ,minSC,
                                  tarSZ,tarSC) {
#   TUTTO IL BLOCCO COMMENTATO E L AVERS ORIG, QUI AGGIUNTO ORDINAMNETO, MA PER SICUREZZA, HO MANTENUTO ANCHE LA VECCHIA VERSIONE
#   #13/01/2016
#   #remove rows containing missing cluster_live or cluster_work
#   LWClus=LWClus[!is.na(cluster_live) & !is.na(cluster_work),]
#   marginals=marginals[!is.na(cluster)]
#   #13/01/2016
#   
#   #16/08 included the keys in LWClus and marginals
#   setkey(LWClus,cluster_live,cluster_work)
#   setkey(marginals,cluster)
#   
#   ####13/08 do not compute for cluster=0
#   indi=1:nrow(LWClus)
#   indi=indi[LWClus$cluster_live==0 | LWClus$cluster_work==0]
#   if(length(indi)>0){LWClus=LWClus[-indi,]}
#   
#   indi=1:nrow(marginals)
#   indi=indi[marginals$cluster==0]
#   if(length(indi)>0){marginals=marginals[-indi,]}
#   ##end 13/08
#   
#   LWSelf <- LWClus[cluster_live == cluster_work, list(cluster_live, amount)]
#   
#   
#   #13/01/2016
#   x=try(LWSelf.temp <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T))
#   if("try-error"%in%class(x)){
#     LWSelf <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T,allow.cartesian = T)
#   }
#   
#   if(!"try-error"%in%class(x)){
#     #16/08 LWSelf <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T)
#     LWSelf=x
#     }
#   #fine 13/01/2016
#   
#   
#   LWSelf[is.na(amount),amount:=0]
#   #13/5/2016
# #   LWSelf[,id:=1:nrow(LWSelf)]
# #   LWSelf[,msc:=min(amount/amount_live,amount/amount_work), by=id]
# #   LWSelf[,id:=NULL]
#   
#   ##16/08/16
#   #the three lines above were replaced by the following
#   LWSelf[,msc:=min(amount/amount_live,amount/amount_work), by=1:nrow(LWSelf)]
#   ##end16/08/16
#   
#   # 18/9
#   
#   LWSelf[,':='(Y=msc/tarSC, sizeMeasure=(tarSZ - amount_live) / (tarSZ - minSZ))]
#   LWSelf[Y > 1, Y:=1]
#   LWSelf[sizeMeasure < 0, sizeMeasure:=0]
#   LWSelf[,Z:=1 - ((1 - (minSC / tarSC))*sizeMeasure)]
#   LWSelf[,validity:=Y * Z * (tarSC / minSC)]
#   
#   #end 18/9
#   
#   
#   LWSelf[msc==1, validity:=1]
#   
#   #13/08 added the validity=1 for cluster=0
#   
#   #dt
#   LWSelf=rbind(LWSelf,data.table(matrix(rep(0,ncol(LWSelf)),1)),use.names=F)
#   
#   LWSelf[cluster==0,validity:=1]
#   
#   #minValidity= LWSelf[,min(validity,na.rm=T)] 
#   ##16/08/16
#   #the line above together with the firstcomponent of output list was replaced by the following
#   minivalout=LWSelf[validity==min(validity,na.rm=T),list(cluster,validity)][1,]
#   ##end 16/06/16
#   
#   ### 13/08 create a list to output the entire validity. The first component is the minimum
#   ##the second component of the list is the entire dataframe LWSelf
#   ### PLEASE BE CAREFULL THAT THE DISSOLVING RULE IS RELATED TO THE 
#   ##WAY THE REGROUPING IS IMPLEMENTED, IN FUNCTION findClusters (in case of more than
#   ##one community in the dissolved cluster).
#   # out=list(LWSelf[LWSelf$validity == minValidity, c("cluster", "validity")][1,],
#   #          LWSelf )
#   
# #   #16/08/2016 remove keys
# #   setkey(LWClus,NULL)
# #   setkey(marginals,NULL)
#   
#   # 16/08/16THE REPLACED COMPONENT OF OUTPUT out=list(LWSelf[validity == minValidity, list(cluster, validity)][1,],LWSelf )
#   out=list(minivalout,LWSelf )
#  
#   return(out)
#   
  
  
  #13/01/2016
  #remove rows containing missing cluster_live or cluster_work
  LWClus=LWClus[!is.na(cluster_live) & !is.na(cluster_work),]
  marginals=marginals[!is.na(cluster)]
  #13/01/2016
  
  #16/08 included the keys in LWClus and marginals
  setkey(LWClus,cluster_live,cluster_work)
  setkey(marginals,cluster)
  
  ####13/08 do not compute for cluster=0
  indi=1:nrow(LWClus)
  indi=indi[LWClus$cluster_live==0 | LWClus$cluster_work==0]
  if(length(indi)>0){LWClus=LWClus[-indi,]}
  
  indi=1:nrow(marginals)
  indi=indi[marginals$cluster==0]
  if(length(indi)>0){marginals=marginals[-indi,]}
  ##end 13/08
  
  LWSelf <- LWClus[cluster_live == cluster_work, list(cluster_live, amount)]
  
  
  #13/01/2016
  x=try(LWSelf.temp <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T),silent=T)
  if("try-error"%in%class(x)){
    LWSelf <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T,allow.cartesian = T)
  }
  
  if(!"try-error"%in%class(x)){
    #16/08 LWSelf <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T)
    LWSelf=x
  }
  #fine 13/01/2016
  
  
  LWSelf[is.na(amount),amount:=0]
  #13/5/2016
  #   LWSelf[,id:=1:nrow(LWSelf)]
  #   LWSelf[,msc:=min(amount/amount_live,amount/amount_work), by=id]
  #   LWSelf[,id:=NULL]
  
  ##16/08/16
  #the three lines above were replaced by the following
  LWSelf[,msc:=min(amount/amount_live,amount/amount_work), by=1:nrow(LWSelf)]
  ##end16/08/16
  
  # 18/9
  
  LWSelf[,':='(Y=msc/tarSC, sizeMeasure=(tarSZ - amount_live) / (tarSZ - minSZ))]
  LWSelf[Y > 1, Y:=1]
  LWSelf[sizeMeasure < 0, sizeMeasure:=0]
  LWSelf[,Z:=1 - ((1 - (minSC / tarSC))*sizeMeasure)]
  LWSelf[,validity:=Y * Z * (tarSC / minSC)]
  
  #end 18/9
  
  
  LWSelf[msc==1, validity:=1]
  
  #13/08 added the validity=1 for cluster=0
  
  #dt
  LWSelf=rbind(LWSelf,data.table(matrix(rep(0,ncol(LWSelf)),1)),use.names=F)
  
  LWSelf[cluster==0,validity:=1]
  
  #minValidity= LWSelf[,min(validity,na.rm=T)] 
  ##16/08/16
  #the line above together with the firstcomponent of output list was replaced by the following
  minivalout=LWSelf[validity==min(validity,na.rm=T),list(cluster,validity)][1,]
  ##end 16/06/16
  
  ### 13/08 create a list to output the entire validity. The first component is the minimum
  ##the second component of the list is the entire dataframe LWSelf
  ### PLEASE BE CAREFULL THAT THE DISSOLVING RULE IS RELATED TO THE 
  ##WAY THE REGROUPING IS IMPLEMENTED, IN FUNCTION findClusters (in case of more than
  ##one community in the dissolved cluster).
  # out=list(LWSelf[LWSelf$validity == minValidity, c("cluster", "validity")][1,],
  #          LWSelf )
  
  #   #16/08/2016 remove keys
  #   setkey(LWClus,NULL)
  #   setkey(marginals,NULL)
  
  # 16/08/16THE REPLACED COMPONENT OF OUTPUT out=list(LWSelf[validity == minValidity, list(cluster, validity)][1,],LWSelf )
  #sort LWSelf by cluster to avoid further orderings in findCluster
  setorder(LWSelf,cluster)
  out=list(minvalout=minivalout,LWSelf=LWSelf )
  
  return(out)
  
  
}
# dissolveCluster: dissolves a cluster into its constituent communities. The constituents are given temporary
# cluster IDs. To signify their temporary nature, these temporary IDs have negative values (ordinary IDs are positive)
#
# First the clusterList is updated with the temporary IDs; then the various init() functions are used to update
# the number of commuters between the temorary clusters and the existant ones, and re-calculate the cluster marginals.
# the modified version dissolveCluster allows to dissolve more clusters simultaneously.

dissolveCluster <- function(clusterData, cluster, LWCom) {
  
  #   temp=cluster
  #   
  #   clusterData$clusterList[,setDissolve:=(cluster%in%temp)]
  #   
  #   sizeDissolve <- clusterData$clusterList[,sum(setDissolve)]
  #   
  #   clusterData$clusterList[setDissolve==T, cluster:=-(1:sizeDissolve)][,setDissolve:=NULL]
  #   
  ###16/08/2016
  #changed the four lines above for speed and warnings
  temp=cluster
  sizeDissolve=clusterData$clusterList[cluster%in%temp,.N]
  clusterData$clusterList[cluster%in%temp,cluster:=-(1:sizeDissolve)]
  setkey(clusterData$clusterList,cluster)
  #rm(temp)
  ###16/08/2016
  
  # LWCLus
  #   names(LWCom)[grep("_live",names(LWCom))][1]="community_live"
  #   names(LWCom)[grep("_work",names(LWCom))][1]="community_work"
  #   
  ###16/08/2016
  #changed the two lines above to avoid copies of LWCom
  setnames(LWCom,grep("_live",names(LWCom[1]))[1],"community_live")
  setnames(LWCom,grep("_work",names(LWCom[1]))[1],"community_work")
  ###16/08/2016
  
  LWClus <- merge(LWCom, data.table(clusterData$clusterList), by.x = "community_live", by.y = "community",all=T)
  LWClus <- merge(LWClus, data.table(clusterData$clusterList), by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  out<-LWClus[, list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  out[is.na(amount),amount:=0]
  
  #13/01/2016
  #   out[,col:=(is.na(cluster_live) & is.na(cluster_work) & amount==0)]
  #   out=out[!(col)]
  #   out[,col:=NULL]
  #   clusterData$LWClus <- out
  #   rm(out)
  
  ##16/08/2016
  ##changed the five lines above
  clusterData$LWClus=out[!(is.na(cluster_live) & is.na(cluster_work) & amount==0),]
  ###end 16/08/2016
  
  setkey(clusterData$LWClus,cluster_live,cluster_work)
  
  ##16/08/2016 keyby nelle due righe sotto
  liveMarginal <- clusterData$LWClus[, list(amount=sum(amount)), keyby = list(cluster=cluster_live)]
  workMarginal <- clusterData$LWClus[, list(amount=sum(amount)), keyby = list(cluster=cluster_work)]
  #DT
  # 20140109: EHVL: added "all=TRUE" to perform full outer join. Without this all clusters that did not
  # have both residents and workers were excluded from the algorithm
  ### in the original function the merge was loosing all the communities which had residents but not workers or
  ### viceversa community which had workers but not residents again it was solved by using the option exclude.
  
  clusterData$marginals  <- merge(liveMarginal, workMarginal, by="cluster",suffixes = c("_live", "_work"), all=TRUE)
  
  rm(liveMarginal)
  rm(workMarginal)
  
  return(clusterData)
  #end function dissolve
}


# determineCohesion: this function determines the cohesion between different clusters. 
# The cohesion strength d(A, B) between clusters A and B, with A != B, is defined as follows:
#
#     d(A, B) = C_AB / R_A * C_AB / W_B + C_BA / R_B * C_BA / W_A
#
# Notation:
# * C_AB denotes the number of workers who live in cluster A and work in cluster B;
# * R_A denotes the total number of workers who live in cluster A: R_A = sum_B C_AB
# * W_B denotes the total number of workers who work in cluster B: W_B = sum_A C_AB
#
# For this implementation, we use the fact that d(A, B) can be written as follows:
# 
#     d(A, B) = D_AB + D_BA, with D_AB = C_AB / R_A * C_AB / W_B.
#
# We call D_AB the one-sided cohesion strength. From this it is clear that d(A,B) is symmetrical
# in A en B. We therefore calculate d(A, B) only for clusters A < B.
#
# Note that these formulas only work if neither R_A or W_B equals 0. If R_A or W_B is zero, we use
#
# D_AB = 0,   
#
# which is a straightforward continuous extension of the previous formula.
#
# In de code D_AB is calculated first, for all A and B. D_BA is obtained from this by transposing 
# D_AB. As we are only interested in d(A,B) when A < B, we only need the upper triangular portions 
# of D_AB and D_BA, viewed as matrices. The upper triangle of D_BA is obtained by transposing the 
# lower triangle of D_AB. If C_AB = 0 we automatically get D_AB = 0 (even when R_A or W_B = 0), 
# since in this case C_AB is not present in the (sparse) representation of the matrix C_AB 
# used throughout this program.
#
# Since D_AB is stored in a table with rows containing values of A, B, and D_AB, transposing D_AB
# simply means swapping the columns containing the values of A and B.


determineCohesion <- function(LWClus, marginals) {
  
  setkey(LWClus,cluster_live,cluster_work)
  setkey(marginals,cluster)
  # Prevent d(A,A) from being calculated
  
  cohesion <- LWClus[cluster_live != cluster_work, ]
  
  # calculate D_AB
  
  cohesion <- merge(cohesion, marginals[, list(cluster, amount_live)], 
                    by.x = "cluster_live", by.y = "cluster")
  cohesion <- merge(cohesion, marginals[, list(cluster, amount_work)], 
                    by.x = "cluster_work", by.y = "cluster")
  
  # a check on  RA=0 and WB=0  has been added below, as done by the JAVA code
  
  
#   cohesion[,strengthOnesided:=rep(-1,nrow(cohesion))]
#   cohesion[,condRAWB:=((amount_live==0)|(amount_work==0))]
#   cohesion[condRAWB==T,strengthOnesided:=0]
#   cohesion[condRAWB==F,strengthOnesided:=amount / amount_live * amount / amount_work] 
#   cohesion[,condRAWB:=NULL]
  
  ###16/08/2016
  ##the four lines above where changed as below in order to speed the computation
  cohesion[,strengthOnesided:=0]
  cohesion[!((amount_live==0)|(amount_work==0)),strengthOnesided:=amount / amount_live * amount / amount_work] 
  ###END 16/08/2016
  
  
  
  # Transpose D_AB to obtain D_BA
  
  cohesion[,setClusterSwap:=cluster_live > cohesion$cluster_work]
  cohesion[setClusterSwap==TRUE, temp:=cluster_live][setClusterSwap==TRUE,cluster_live:=cluster_work] [setClusterSwap==TRUE,cluster_work:=temp][,':='(temp=NULL,setClusterSwap=NULL)]
  
  # Add D_AB and D_BA together
  
  cohesion[,list(strength=sum(strengthOnesided)),by=list(cluster1 = cluster_live, cluster2 = cluster_work)]
}

#####
# determineRegroupList: determines, for each cluster in LWClus, the cluster with which that cluster is most
# closely associated. First the cohesion between all pairs of clusters is calculated; then the cohesion is sorted
# for each cluster and only the last (i.e., highest) one is selected.
##############################################  
### in this function a series of "print" has been added to trace the history of the clusters; 
### the result of the operations is printed on the screen 
### and in the "sink" file. These prints can be commented to make the program more efficient; however 
### at the moment thay are consider important.
##############################################
determineRegroupList <- function(LWClus, marginals) {
  
  cohesion <- determineCohesion(LWClus, marginals)
  cohesion <- cohesion[order(cohesion$cluster1, cohesion$strength),]
  
  cohesion[!duplicated(cohesion$cluster1, fromLast = TRUE), list(cluster1, cluster2)]
}


# regroupDissolved: regroups the communities in a dissolved cluster (i.e., those with IDs < 0). For each such community
# the cluster with which is has the highest cohesion is determined; these are then merged together.
# this function call the function  determineRegroupList.

regroupDissolved <- function(clusterData) {
  
  neg.cluster=clusterData$marginals[cluster<0,cluster]
  
#   LWClusDissolve=clusterData$LWClus[,temp :=xor(cluster_live < 0, cluster_work < 0)]
#   LWClusDissolve=LWClusDissolve[temp==T]
#   LWClusDissolve[,temp:=NULL]
  
  ###16/08/2016
  #the three lines above were replaced with a single opration
  LWClusDissolve=clusterData$LWClus[xor(cluster_live < 0, cluster_work < 0),]
  ###end 16/08/2016
  
  
  #LWClusDissolve=LWClusDissolve[cluster_live!=0][cluster_work!=0]
  ###16/08/2016
  #the line above changed into
  LWClusDissolve=LWClusDissolve[cluster_live!=0 & cluster_work!=0]
  ###end 16/08/2016
  
  
  if(nrow( LWClusDissolve)==0){out=1}
  
  if(nrow( LWClusDissolve)>0){
        #regroupList <- determineRegroupList(LWClusDissolve, clusterData$marginals)
    ###16/08/2016
    # the line above was replaced four lines below in order to evaluate 
    #only the clusters having links with the dissolved one!
        uno=LWClusDissolve[cluster_live>0,cluster_live]
        due=LWClusDissolve[cluster_work>0,cluster_work]
        involvedclusters=sort(unique(c(uno,due,neg.cluster,0)))
        regroupList <- determineRegroupList(LWClusDissolve, clusterData$marginals[cluster%in%involvedclusters,])
    ###end 16/08/2016
    
    out=list(mergeCluster(clusterData, regroupList$cluster1, regroupList$cluster2),regroupList)
  }
  return(out)
}

#  this modified version of the original function regroupDissolved is used when the cluster has only one municipality in it

###############################################################################
#####  regroupDissolved.ncom is used when the cluster has more than one municipality
#####  18/06/2014  the regrouping implemented in this function is sequential and not simultaneous as in the original script. 
#####  A new parameter, "index.com.2diss", has been created in order to define the order to be followed in the regrouping.
#####  The order in which the municipalities are considered for regrouping influences the final result.  
#####  13/8 A new order has been implemented according to Coombes and Wymer (2014)
#####
############################################################################
regroupDissolved.ncom <- function(clusterData,index.com.2diss) {
  
  
  ###18/06/2014  the 2 lines below identify the links of the community "index.com.2diss" with other communities
  ###            except those  that are being regrouped 
  LWClusDissolve <- clusterData$LWClus[xor(clusterData$LWClus$cluster_live == index.com.2diss, clusterData$LWClus$cluster_work == index.com.2diss), ]
  LWClusDissolve <- LWClusDissolve[LWClusDissolve$cluster_live*LWClusDissolve$cluster_work<0,]
  
  ###16/08/2016
  neg.cluster=clusterData$marginals[cluster==index.com.2diss,cluster]
  ###end 16/08/2016
  
  ##13/08
  ##from the list of possible clusters, the cluster=0 has to be eliminated.
  
  ##If cluster=0 is the only possible cluster, the output will be a numeric one. Otherwise,
  ##the output will be a list
  indi=1:nrow(LWClusDissolve)
  indi=indi[LWClusDissolve$cluster_live!=0]
  if(length(indi)>0){LWClusDissolve=LWClusDissolve[indi,]}
  
  indi=1:nrow(LWClusDissolve)
  indi=indi[LWClusDissolve$cluster_work!=0]
  if(length(indi)>0){LWClusDissolve=LWClusDissolve[indi,]}
  
  if(nrow( LWClusDissolve)==0){out=1}
  ##END 13/08
  
  #29/04/2015 changed from >= to > in the row below
  if(nrow( LWClusDissolve)>0){
    ## call the version FULL of the function determineRegroupList, but as the communities are regrouped one after the other
    ### it seems that there should be no difference in identifying the first component (the one actually used) or all components (commented line).
    
    ###16/08/2016 - changes as in regroupList
    uno=LWClusDissolve[cluster_live>0,cluster_live]
    due=LWClusDissolve[cluster_work>0,cluster_work]
    involvedclusters=sort(unique(c(uno,due,neg.cluster,0)))
    regroupList <- determineRegroupList(LWClusDissolve, clusterData$marginals[cluster%in%involvedclusters,])
    ##end 16/08/2016
    #regroupList <- determineRegroupList(LWClusDissolve, clusterData$marginals)[1,]
    
    ##for coherence there is the call to the version FULL of the function
    out=list(mergeCluster(clusterData, regroupList$cluster1, regroupList$cluster2),regroupList)
  }
  
  return(out)
}


###############################################
############################################

LMAwrite=function(out,path_wd=NULL,suff=NULL){
  # the program gets the output and 
  # 1 if the path is present then it will write the output in the given directory
  #   otherwise the output will be written the the current directory
  # 2 if a suffix is given it will write the names of the output with the given suffice
  #   otherwise the standard names will be given
  # 3 writes the output in thre different csv files (the three components of the list)
  # 4 saves an .RData object with the output of the procedure (list of 3)
  # 5 saves the three lists in three list: reserve.list, ComNotAssigned.list and
  #   zero.list
  # at the end it comes back to the original directory
  #
  #  
  s=getwd()
  if(is.null(path_wd)){path_wd=s}
  s=getwd()
  setwd(path_wd)
  # clusterList
  if(!is.null(suff)){fname=paste("LMA clusterList ",suff,".csv",sep="")}
  if(is.null(suff)){fname=paste("LMA clusterList ","",".csv",sep="")}
  write.table(out[[1]]$clusterList,file=fname,sep=";",row.names=F,col.names=T,append=F,quote=F)
  rm(fname)
  ##marginals
  if(!is.null(suff)){fname=paste("LMA marginals ",suff,".csv",sep="")}
  if(is.null(suff)){fname=paste("LMA marginals ","",".csv",sep="")}
  write.table(out[[1]]$marginals,file=fname,sep=";",row.names=F,col.names=T,append=F,quote=F)
  rm(fname)
  #LWClus
  if(!is.null(suff)){fname=paste("LMA LWClus ",suff,".csv",sep="")}
  if(is.null(suff)){fname=paste("LMA LWClus ","",".csv",sep="")}
  write.table(out[[1]]$LWClus,file=fname,sep=";",row.names=F,col.names=T,append=F,quote=F)
  rm(fname)
  
  
  
  ################
  
  ##############
  
  
  # save the output
  if(!is.null(suff)){fname=paste("LMA list ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("LMA list ","",".RData",sep="")}
  lma=out[[1]]
  save(lma,file=fname)
  rm(lma)
  rm(fname)
  # the output before the assignment of the reserve.list
  if(!is.null(suff)){fname=paste("LMA list before assignment ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("LMA list before assignment ","",".RData",sep="")}
  lma.before0=out[[2]]
  save(lma.before0,file=fname)
  rm(lma.before0)
  rm(fname)
  # reserve.list
  if(!is.null(suff)){fname=paste("reserve list ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("reserve list ","",".RData",sep="")}
  reserve.list=out[[3]]
  save(reserve.list,file=fname)
  rm(reserve.list)
  rm(fname)
  # ComNotAssigned
  if(!is.null(suff)){fname=paste("ComNotAssigned ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("ComNotAssigned ","",".RData",sep="")}
  comNotAssigned=out[[4]]
  save(comNotAssigned,file=fname)
  rm(comNotAssigned)
  rm(fname)
  # zero.codes
  if(!is.null(suff)){fname=paste("zero.codes ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("zero.codes ","",".RData",sep="")}
  zero.list=out[[5]]
  save(zero.list,file=fname)
  rm(zero.list)
  rm(fname)
  
  
  setwd(s)
  
  #end function
}



#########################################################
#######################

AssignLmaName=function(LWCom,lma,ComNames){
  
  LWCom=data.table(LWCom)
  lma$clusterList=data.table(lma$clusterList)
  lma$LWClus=data.table(lma$LWClus)
  lma$marginals=data.table(lma$marginals)
  ComNames=data.table(ComNames)
  setnames(lma$clusterList,c("community","LMA","EMP_live"))
  setnames(lma$LWClus,c("LMA_live","LMA_work","commuters"))
  setnames(lma$marginals,c("LMA","EMP_live","EMP_work"))
  
  ##11/09/2016
#   #delete statistics from marginals
#   lma$marginals[,validity:=NULL]
#   lma$marginals[,SC_demand_side:=NULL]
#   lma$marginals[,SC_supply_side:=NULL]
#   lma$marginals[,N_com:=NULL]
#   lma$marginals[,EMP_live_work:=NULL]
  
  ComNames[,Code:=as.numeric(Code)]
  
  LmaNames=copy(ComNames)
  LmaNames[,lma.name:=toupper(com.name)][,com.name:=NULL]
  
  worker=LWCom[,list(worker=sum(amount)),by=list(community=community_work)]
  worker=merge(worker,ComNames,by.x="community",by.y="Code",all.y=T)
  worker$worker[is.na(worker$worker)]=0
  
  lma$clusterList<-merge(lma$clusterList,worker,by="community")
  lma$clusterList=merge(lma$clusterList,LmaNames, by.x="community",by.y="Code")
  
  
  ####15/08/2016 cambiato LMA.mio in temp
  NumerositaNome=lma$clusterList[,maxi:=max(worker),by=LMA]
  lma$clusterList[maxi==worker,temp:=lma.name]
  lma$clusterList[,temp:=names(table(temp))[1],by=LMA]
  lma$clusterList[,lma.name:=temp]
  lma$clusterList[,temp:=NULL]
  lma$clusterList[,maxi:=NULL]
  lma$clusterList[,worker:=NULL]
  setcolorder((lma$clusterList),c("community", "com.name", "LMA", "lma.name", "EMP_live"))
  
  
  lma$marginals=merge(lma$marginals,unique(lma$clusterList[,list(LMA,lma.name)]),
                      by.x="LMA",by.y="LMA")
  
  setcolorder(lma$marginals,c( "LMA","lma.name" ,"EMP_live","EMP_work" ))
  
  lma$LWClus=merge(lma$LWClus,unique(lma$clusterList[,list(LMA,lma.name.live=lma.name)]),
                   by.x="LMA_live",by.y="LMA")
  
  lma$LWClus=merge(lma$LWClus,unique(lma$clusterList[,list(LMA,lma.name.work=lma.name)]),
                   by.x="LMA_work",by.y="LMA")
  
  setcolorder(lma$LWClus,c("LMA_live", "lma.name.live", 
                           "LMA_work", "lma.name.work", "commuters"  ))
  
  
  return(lma)
  #end function AssignLmaName
}
######################################################

CreateLMAShape=function(lma
                        ,comIDs="community"
                        ,lmaIDs="LMA"
                        ,shp_com=NULL
                        ,shp_com_name=NULL
                        ,id_shp_com
                        ,proj4string=CRS("+proj=longlat +datum=WGS84")
                        ,outdir=NULL
                        ,outfile=NULL
                        ,bmpfile=NULL
                        ,plot_opt=c("green",1,2,"red",1,2,0.8,2)){
  
  
  # Starting from the shape files of the underlying 
  # municipalities, this function creates the shape files of the 
  # local labour market areas.
  
#   require(data.table)
#   require(bit64)
#   require(rgdal)
#   require(maptools)
#   require(maps)
#   require(rgeos)
#   library(sp)
  
  ##################################
  ##################################
  
  ####check the existence of names in lma
  if(!all(c("com.name","lma.name")%in%names(lma$clusterList))){
    stop("ERROR: please check the names of the lma$clusterList object.")
  }
  if(!all(c("lma.name.live","lma.name.work")%in%names(lma$LWClus))){ stop("ERROR: please check the names of the lma$LWClus object.")
  }
  if(!all(c("lma.name")%in%names(lma$marginals))){ stop("ERROR: please check the names of the lma$marginals object.")
  }
  
  ##end check names in lma
  ##################################################
  
  ##read the data
  if(!is.null(shp_com_name)){
    shp <- maptools::readShapePoly( shp_com_name,proj4string=proj4string)
  }
  if(is.null(shp_com_name)){
    shp=shp_com
  }
  if(is.null(shp_com_name) & is.null(shp_com)){
    stop("ERROR: please provide an input shape object or file to be read.")
  }
  
  if(!is.null(shp_com_name) & !is.null(shp_com)){
    print(" Inputs shp_com and shp_com_name are both provided. Only the first is considered.")
  }
  ###end read data
  
  
  if(comIDs%in%colnames(lma$clusterList) & lmaIDs%in%colnames(lma$clusterList) & lmaIDs%in%colnames(lma$marginals)){
    
    
    #transform the shape file using the coordinates WGS84 
    shp<-sp::spTransform(shp,CRS("+proj=longlat +datum=WGS84 +no_defs 
                             +ellps=WGS84 +towgs84=0,0,0 "))
    
    
    ###end transform
    
    shp<-sp::merge(shp,data.frame(lma$clusterList),by.x = id_shp_com, by.y = comIDs)
    new_shp<-maptools::unionSpatialPolygons(shp, IDs = shp[[as.character(colnames(lma$clusterList)[colnames(lma$clusterList)==lmaIDs])]])
    IDs <- sapply(slot(new_shp, "polygons"), function(x) slot(x, "ID"))
    indi=grep(lmaIDs,names(lma$clusterList))
    df <- data.frame(AREA=1:length(unique(data.frame(lma$clusterList)[,indi])),row.names=IDs)
    
    df[,as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList))==lmaIDs])]<-as.character(unique(data.frame(lma$clusterList)[,as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList))==lmaIDs])]))
    df<-df[order(df[,as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList))==lmaIDs])]),]
    
    row.names(df)<-IDs
    df[,as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList))==lmaIDs])]<-as.integer(df[,as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList))==lmaIDs])])
    
    join_com <- sp::SpatialPolygonsDataFrame(new_shp, df)
    
    shp_lma<-sp::merge(join_com,data.frame(lma$marginals),by=lmaIDs)
    print(class(shp_lma))
    if(!is.null(outfile)){
      writeOGR(shp_lma,outdir, outfile, driver="ESRI Shapefile")
    }
    
    if(!is.null(bmpfile)){
      bmp(bmpfile,width=1200,height=1200)
      plot(shp_lma,col=plot_opt[1],
           lwd=as.numeric(plot_opt[2]),lty=as.numeric(plot_opt[3]))
      centroids <- coordinates(join_com)
      text(centroids, label=shp_lma@data$lma.name,
           col=plot_opt[4],lwd=as.numeric(plot_opt[5]),lty=as.numeric(plot_opt[6]),
           cex = as.numeric(plot_opt[7]),font = as.numeric(plot_opt[8]))
      dev.off()
    }
    
    #end if check existence of community and LMA id 
    #in the column names of the object lma
  }
  
  if(!(comIDs%in%colnames(lma$clusterList) & lmaIDs%in%colnames(lma$clusterList))){
    stop("ERROR: please che the column names of the lma object, especially the clusterList component")
  }
  
  if(!(lmaIDs%in%colnames(lma$marginals))){
    stop("ERROR: please che the column names of the lma object, especially the marginals component")
  }
  
  comID.in.LMA.not.in.SHP=setdiff(lma$clusterList$comIDs,shp@data$id_shp_com)
  comID.in.SHP.not.in.LMA=setdiff(shp@data$id_shp_com,lma$clusterList$comIDs)
  
  return(list(shp_lma=shp_lma
              ,comID.in.LMA.not.in.SHP=comID.in.LMA.not.in.SHP
              ,comID.in.SHP.not.in.LMA=comID.in.SHP.not.in.LMA))
  #end function
}

#################################
AssignSingleComToSingleLma=function(lma,comID,lmaID,dat){
  ##This function assigns an unique community to 
  ##an lma. It should serve for "manual" assignment of communities
  ###to lma.
  
  if(!lmaID%in%lma$marginals$cluster){
    stop("ERROR: the lmaID is not in the list of possible LMA.")
  }
  
  if(!comID%in%lma$clusterList$community){
    stop("ERROR: the comID is not in the list of possible communities.")
  }
  
  
  
  #clusterList
  #lma$clusterList=data.table(lma$clusterList)
  #nomi=names(lma$clusterList[1,])
  #nomi=gsub("cluster","LMA",nomi)
  #setnames(lma$clusterList,nomi)
  clusterList=copy(lma$clusterList[,list(community,cluster,residents)])
  
  clusterList[community==comID,cluster:=lmaID]
  
  #LWClus
  LWClus <- merge(dat, clusterList[,list(community,cluster)], by.x = "community_live", by.y = "community",all=T)
  LWClus <- merge(LWClus, clusterList[,list(community,cluster)], by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  temp<-LWClus[, list(commuters=sum(amount)),by=list(cluster_live,cluster_work)]
  temp[is.na(commuters),commuters:=0]
  
  temp[,col:=(is.na(cluster_live) & is.na(cluster_work) & commuters==0)]
  temp=temp[!(col)]
  temp[,col:=NULL]
  rm(LWClus)
  LWClus <- temp
  rm(temp)
  
  #marginals
  liveMarginal <- LWClus[, list(amount=sum(commuters)), by = list(cluster=cluster_live)]
  workMarginal <- LWClus[, list(amount=sum(commuters)), by = list(cluster=cluster_work)]
  
  marginals <- merge(liveMarginal, workMarginal, by="cluster",suffixes = c("_live", "_work"), all=TRUE)
  
  rm(liveMarginal)
  rm(workMarginal)
  
  setnames(clusterList,c("community","cluster","residents"))
  setnames(LWClus,c("cluster_live","cluster_work","amount"))
  setnames(marginals,c("cluster","amount_live","amount_work"))
  
  lma.out=list(clusterList=clusterList,LWClus=LWClus,marginals=marginals)
  return(lma.out)
  #end function
}
####################################
EqualLmaPartition=function(lma1,lma2){
  
  out=TRUE
  lma1$clusterList=data.table(lma1$clusterList)
  lma2$clusterList=data.table(lma2$clusterList)
  #compare communities
  if((length(setdiff(lma1$community,lma2$community))>0)|(length(setdiff(lma2$community,lma2$community))>0)){
    print("The lists of communities are different.")
    out=FALSE
    #end if length
  }
  else{
    setkey(lma1$clusterList,community,cluster)
    setkey(lma2$clusterList,community,cluster)
    if(!all(lma1$clusterList$cluster==lma2$clusterList$cluster)){
      out=FALSE
      print("The same communities, but different clusters.")
      #end if
    }
    #end else
  }
  return(out)
  #end function EqualLmaPartition
}

##################
FindIsolated=function(lma,lma_shp=NULL,lma_shp_path=NULL,lma_shp_name=NULL,
                      com_shp=NULL,com_shp_path=NULL,com_shp_name=NULL,
                      id_com,
                      proj4string=CRS("+proj=longlat +datum=WGS84")){
  
  ###permane il problema di enclaves costituite da piu' comuni
  # forse chiedere a Michele di trovare la funzione R che mimic
  # la funzione identify in arcgis.
  # (cosi' si sistema in automatico il tutto)
  
  #
  # fine tuning "automatizzato" attenzione: questo programma non identifica
  #              sistemi locali all'interno di sistemi locali
  #
  
#   require(rgdal)
#   require(maptools)
#   require(maps)
#   require(rgeos)
#   require(sp)
#   require(spdep)
#   require(data.table)
  
  
  # lettura dati
  if(is.null(lma_shp)){
    shp <- maptools::readShapePoly(paste(lma_shp_path, lma_shp_name, sep="/"),proj4string=proj4string)
  }
  if(!is.null(lma_shp)){shp=lma_shp}
  if(length(grep("lma_nam",names(shp@data)))>0){
    names(shp@data)[names(shp@data)=="lma_nam"]="lma.name"}
  if(length(grep("EMP_liv",names(shp@data)))>0){  
    names(shp@data)[names(shp@data)=="EMP_liv"]="EMP_live"}
  if(length(grep("EMP_wrk",names(shp@data)))>0){    
    names(shp@data)[names(shp@data)=="EMP_wrk"]="EMP_work"}
  if(length(grep("EMP_lv_",names(shp@data)))>0){    
    names(shp@data)[names(shp@data)=="EMP_lv_"]="EMP_live_work"}
  
  #comuni91=readShapePoly("Com1991_WGS84_g.shp",proj4string=proj4string)
  
  if(is.null(com_shp)){
    comuni91=maptools::readShapePoly(paste(com_shp_path, com_shp_name, sep="/"),proj4string=proj4string)
  }
  if(!is.null(com_shp)){comuni91=com_shp}
  
  

  kiki=grep(id_com,names(comuni91@data))
  comuni91<-sp::merge(comuni91,data.frame(lma$clusterList),by.x = kiki, by.y = "community")
  
  ###################fine lettura dati
  
  
  ##########calcolo sll che non hanno legami####################################
  nbpolygsll <- spdep::poly2nb(shp, row.names =shp@data$LMA,queen=TRUE)
  # summary(nbpolygsll )
  # matrice contiguita'
  Wsll=spdep::nb2mat(nbpolygsll,style="B", zero.policy=TRUE)
  rownames(Wsll[rowSums(Wsll)==0,])
  colnames(Wsll)=shp@data$LMA
  
  #sll che non hanno legami con nessuno:
  nolinksll=shp@data$LMA[shp@data$LMA%in%rownames(Wsll)[rowSums(Wsll)==0]]
  nolinksllname=shp@data$lma.name[shp@data$LMA%in%rownames(Wsll)[rowSums(Wsll)==0]]
  
  #####################################################################
  
  #################################
  ###elimino sll che non hanno legami da shp
  shp=shp[!shp$LMA%in%nolinksll,]
  ###################################
  
  ###################################################
  #########SLL unici treatment
  
  ####calcolo sll unici = con un solo comune
  lma$clusterList=data.table(lma$clusterList)
  #togliamo quelli di sopra nolinksll
  lma$clusterList=lma$clusterList[!(lma$clusterList$LMA%in%nolinksll),]
  lma$LWCom=lma$LWCom[!(lma$LWCom$LMA_live%in%nolinksll),]
  lma$LWCom=lma$LWCom[!(lma$LWCom$LMA_work%in%nolinksll),]
  lma$marginals=lma$marginals[!(lma$marginals$LMA%in%nolinksll),]
  
  lma$clusterList[,Ncom:=.N,by=LMA]
  badsll=lma$clusterList[Ncom==1,]
  setorder(badsll,-EMP_live)
  
  badsll=badsll$LMA
  badsllname="0"
  for(kiki in 1:length(badsll)){
    badsllname=c(badsllname,
                 as.character(shp@data$lma.name[shp$LMA==badsll[kiki]]))
  }
  badsllname=badsllname[-1]
  ############################################
  ############################################
  
  
  ###############################################################
  ###############################################################
  ###################ENCLAVES
  
  # disaggregazione degli shape nei loro componenti (non contiguita')
  d=disaggregate(shp)
  
  
  d@data=data.table(d@data)
  
  d@data[,x:=.N,by=lma.name] #quanti poligoni sono in ogni sll
  d@data[x>=2,ID_PiecesLMA:=paste(as.character(LMA),1:max(x),sep="_"),by=lma.name]
  d@data[is.na(ID_PiecesLMA),ID_PiecesLMA:=as.character(LMA)]
  
  
  ## Identificazione poligoni contigui,  e definizione della matrice di contiguita')
  
  sll.shp.nb <- spdep::poly2nb(d, row.names =d@data$ID_PiecesLMA,queen=TRUE)
  
  # summary(sll.shp.nb )
  # matrice contiguita
  W=spdep::nb2mat(sll.shp.nb,style="B", zero.policy=TRUE)
  rownames(W[rowSums(W)==0,])
  colnames(W)=d@data$ID_PiecesLMA
  
  ###ELIMINO I POLIGONI CHE NON HANNO LEGAMI CON NESSUNO
  nolinkpolig=d@data$ID_PiecesLMA[d@data$ID_PiecesLMA%in%rownames(W)[rowSums(W)==0]]
  nolinkpoligname=d@data$lma.name[d@data$ID_PiecesLMA%in%rownames(W)[rowSums(W)==0]]
  ###fine elimino polig che non hanno legami
  
  
  ##IDENTIFICAZIONE DELLE ENCLAVES
  badlmalist=sort(unique(d@data$LMA[d@data$x>1]))
  zeris=c(0,0)
  df.mun.poly=data.frame(t(zeris))
  colnames(df.mun.poly)=c("community","Polygon")
  rm(zeris)
  
  ###VISUALIZZAZIONE E SCELTA DEI POLIGONI E COMUNI NON CONTIGUI
  
  indice.ident=grep(id_com,names(comuni91@data))
  for(badlma in badlmalist){
    ###mettiamo insieme polygons e comuni
    #windows(1200,1200)
    dev.new()
    par(mfrow=c(1,2))
    zz=d[d$LMA==badlma,]
    plot(zz,main=paste("polygons",unique(zz@data$lma.name),sep=" "),col=c("yellow","red","cyan","orange"),col.main="red")
    centroids <- coordinates(zz)
    text(centroids, label=zz@data$ID_PiecesLMA,
         cex = 0.8)
    
    plot(comuni91[comuni91$LMA==badlma,],main=paste("communities",unique(zz@data$lma.name),sep=" "))
    centroids <- coordinates(comuni91[comuni91$LMA==badlma,])
    #text(centroids, label=comuni91[comuni91$LMA==badlma,]@data$PRO_COM,
    text(centroids, label=comuni91[comuni91$LMA==badlma,]@data[,indice.ident],     
         cex = 0.8)
    
    par(mfrow=c(1,1))
    
    for(kiki in 1:(max(zz@data$x)-1)){
      mimi=readline(paste("LMA ID ",badlma ," please type com ID:     ",sep=""))
      
      ### CHECK SE SERVE POLYGON ID
      cici=readline(paste("LMA ID ",badlma ," please type polygon label:     ",sep=""))
      df.mun.poly=rbind(df.mun.poly,c(mimi,cici))
    }
    dev.off()
    #end for badlmalist
  }
  df.mun.poly=df.mun.poly[-1,]
  dim(df.mun.poly)
  df.mun.poly=df.mun.poly[(df.mun.poly$community!="" & df.mun.poly$Polygon!=""),]
  dim(df.mun.poly)
  
  answer="n"
  repeat{
    fix(df.mun.poly)
    answer=readline("do you confirm the association of communities with polygons? y/n  ")
    if (answer=="y") { 
      break
    }
  }
  df.mun.poly=df.mun.poly[(df.mun.poly$community!="" & df.mun.poly$Polygon!=""),]
  
  df.mun.poly=merge(df.mun.poly,lma$clusterList[,list(community,LMA,EMP_live)],by="community")
  df.mun.poly=data.table(df.mun.poly)
  setorder(df.mun.poly,-EMP_live)
  
  ###check se tutti poligoni inseriti sono nella lista iniziale
  if(!all(df.mun.poly$Polygon%in%rownames(W))){
    repeat{
      print("Please control the following polygons IDs in the association table.")
      print(setdiff(df.mun.poly$Polygon,rownames(W)))
      fix(df.mun.poly)
      if(all(df.mun.poly$Polygon%in%rownames(W))){break}
      #end repeat
    }
    #end if
  }
  ##end check poligoni
  
  ###check se tutti i comuni inseriti sono nella lista iniziale
  if(!all(df.mun.poly$community%in%lma$clusterList$community)){
    repeat{
      print("Please control the following communities ID in the association table.")
      print(setdiff(df.mun.poly$community,lma$clusterList$community))
      fix(df.mun.poly)
      if(all(df.mun.poly$community%in%lma$clusterList$community)){break}
      #end repeat
    }
    #end if
  }
  
  ###############################################
  ####################build the output
  
  lma.unique.ID=badsll
  lma.unique.name=badsllname
  lma.nolink.ID=nolinksll
  lma.nolink.name=nolinksllname
  
  isolated.lma=list(contig.matrix.lma=Wsll
                    ,lma.unique=data.table(lma.unique.ID,lma.unique.name)
                    ,lma.nolink=data.table(lma.nolink.ID,lma.nolink.name))
  
  poly.nolink.ID=nolinkpolig
  poly.nolink.name=nolinkpoligname
  
  isolated.poly=list(contig.matrix.poly=W,
                     poly.com.linkage=data.table(df.mun.poly),
                     poly.nolink=data.table(poly.nolink.ID,poly.nolink.name))
  
  return(list(isolated.lma=isolated.lma,isolated.poly=isolated.poly))
  ################OUTPUT
  
  #end function FindIsolated
}

#############################

FindContig=function(type="poly",lma,contig.matrix,isolated){
  
  if(!type%in%c("poly","lma")){
    stop("ERROR: type must be poly or lma")
  }
  
  if(type=="poly"){
    # identificazione della lista dei poligoni contigui  
    lista.contigui.poly=list()
    i=1
    for(sgd in isolated$Polygon){
      # print(i)
      # print(sgd)
      #vicini
      if(!is.na(match("temp",ls()))){rm(temp)}
      #temp=names(which(W[sgd,]==1))
      temp=names(which(contig.matrix[as.character(sgd),]==1))
      lista.contigui.poly[[i]]=temp
      names(lista.contigui.poly)[i]=sgd
      i=i+1
      #end for
    }
    
    names(lista.contigui.poly)=isolated$community
    if(length(lista.contigui.poly)>0){
      for(i in 1:length(lista.contigui.poly)){
        if("cici"%in%ls()){rm(cici)}
        cici=grep("_",lista.contigui.poly[[i]])
        tutti=1:length(lista.contigui.poly[[i]])
        diff=setdiff(tutti,cici)
        lista.contigui.poly[[i]]=lista.contigui.poly[[i]][diff]
        #end for length lista poly
      }
      #end if length list
    }
    numvicini=unlist(lapply(lista.contigui.poly,FUN=length))
    
    com_onlyenclave=names(numvicini)[numvicini==0]
    
    lista.contigui.poly=lista.contigui.poly[numvicini!=0]
    
    out=list(list.contig.poly=lista.contigui.poly
             ,com_no.LMA.neigh=com_onlyenclave)
    #end if type="poly"
  }
  
  if(type=="lma"){
    ###lista contig sll1
    lista.contigui.sll1=list()
    i=1
    for(sgd in isolated){
      #print(sgd)
      #vicini
      if(!is.na(match("temp",ls()))){rm(temp)}
      temp=names(which(contig.matrix[as.character(sgd),]==1))
      
      lista.contigui.sll1[[i]]=temp
      names(lista.contigui.sll1)[i]=sgd
      i=i+1
      #end for
    }
    
    ###########################################
    cici=merge(data.frame(isolated),data.frame(lma$clusterList),by.x="isolated",
               by.y="LMA")
    cici=data.table(cici)
    setorder(cici,-EMP_live)
    names(lista.contigui.sll1)=cici$community
    
    out=list(list.contig.lma=lista.contigui.sll1)
    
    #end if type==lma
  }
  
  return(out)
  #end function FindContig
}

###################


FineTuning=function(dat,out.ini,list.contiguity){
  ###fine tuning
  not.tunned.commID=-1
  
  #setwd("G:\\franconi\\Desktop\\2011carto\\prova")
  
  
  # dat<-fread("G:\\franconi\\Documenti\\1 SLL\\EURO novembre 2014\\data\\pendola1991_nomerc.csv",sep=";")
  # load("R1.1_pendola1991_nomerc.csv_1000_0.6_10000_0.75_.Rdata")
  # load("INTOCCABILE.Rdata")
  # out=out.1 SENZA NOMI - OUT DI FIND CLUSTER
  # list.contiguity=lista.contigui.poly
  
  out=copyClusterData(out.ini)
  
  out.orig=out
  dat.orig=dat
  
  for(i in 1:length(list.contiguity)){
    out=out.orig
    dat=dat.orig
    
    com.to.assign=as.numeric(names(list.contiguity)[i])
    candidates=as.numeric(list.contiguity[[i]])
    
    out$clusterList=data.table(out$clusterList)
    #dissolve il cluster di appartenenza - tolgo il comune
    out$clusterList$LMA[out$clusterList$community==com.to.assign]=-1
    
    #seleziono il -1 ed i candidates
    out$clusterList=out$clusterList[LMA%in%c(-1,candidates)]
    setnames(out$clusterList,c("community","cluster", "EMP_live"))
    
    
    #costruisco un out.f e un dat.f che rappresentano
    #solo il com.to.assign e candidates
    
    #il dat parziale
    dat.f=dat
    dat.f=merge(dat.f, out$clusterList ,by.x="community_live", by.y="community" )  
    dat.f[,EMP_live:=NULL]
    dim(dat.f)
    setnames(dat.f,c("community_live", "community_work", "amount",  "LMA_live"))
    dat.f=merge(dat.f, out$clusterList ,by.x="community_work", by.y="community" ) 
    dat.f[,EMP_live:=NULL]
    setnames(dat.f,c("community_work","community_live",  "amount",  "LMA_live", "LMA_work"))
    
    ############## costruita la matrice parziale contenente solo la fusione e i SL contigui 
    # determino out.f$LWCLus
    
    out$LWClus=dat.f[,list(commuters=sum(amount)),by=list(LMA_live,LMA_work)]
    setnames(out$LWClus,c("cluster_live", "cluster_work", "amount"))
    
    #  infine devo aggiornare marginals
    # out.f$marginals
    liveMarginal=out$LWClus[,sum(amount),by=list(LMA=cluster_live)]
    workMarginal=out$LWClus[,sum(amount),by=list(LMA=cluster_work)]
    #
    #liveMarginal=aggregate(out.f$LWClus$commuters,by=list(LMA=out.f$LWClus$LMA_live) , FUN=sum) 
    #workMarginal=aggregate(out.f$LWClus$commuters,by=list(LMA=out.f$LWClus$LMA_work) , FUN=sum) 
    liveMarginal$LMA=factor(liveMarginal$LMA, exclude="")
    workMarginal$LMA=factor(workMarginal$LMA, exclude="")
    out$marginals=merge(liveMarginal,workMarginal, by="LMA", suffixes=c("_live","_work"), all=TRUE)
    out$marginals$LMA=as.numeric(as.vector(out$marginals$LMA))
    out$marginals$LMA[is.na(out$marginals$LMA)]=0
    setnames(out$marginals,c("cluster","amount_live", "amount_work"))
    
    
    
    cosa=regroupDissolved(out)
    
    
    ###aggiorno out.orig
    #clusterList
    if(is.list(cosa)){
      l.new=cosa[[1]]$clusterList$cluster[cosa[[1]]$clusterList$community==com.to.assign]
    }else{
      ##back to the initial option
      l.new=out.ini$clusterList$LMA[out.ini$clusterList$community==com.to.assign]
      not.tunned.commID=c(not.tunned.commID,com.to.assign)
    }
    
    
    
    out.orig$clusterList[community==com.to.assign,LMA:=l.new]
    #out.orig$clusterList=out.orig$clusterList[,EMP_live:=sum(EMP_live),by=list(LMA=cluster,community=community)]
    
    #LWClus
    dat=merge(dat.orig, out.orig$clusterList ,by.x="community_live", by.y="community" )  
    dat=dat[, EMP_live:=NULL]
    setnames(dat,c("community_live", "community_work", "amount",  "LMA_live"))
    
    dat=merge(dat, out.orig$clusterList ,by.x="community_work", by.y="community" ) 
    dat=dat[, EMP_live:=NULL]
    setnames(dat,c("community_work","community_live",  "amount",  "LMA_live", "LMA_work"))
    
    out.orig$LWClus=dat[,list(commuters=sum(amount)),by=list(LMA_live,LMA_work)]
    
    
    # out.f$marginals
    liveMarginal=out.orig$LWClus[,sum(commuters),by=list(LMA=LMA_live)]
    workMarginal=out.orig$LWClus[,sum(commuters),by=list(LMA=LMA_work)]
    
    liveMarginal$LMA=factor(liveMarginal$LMA, exclude="")
    workMarginal$LMA=factor(workMarginal$LMA, exclude="")
    marginals=merge(liveMarginal,workMarginal, by="LMA", suffixes=c("_live","_work"), all=TRUE)
    marginals$LMA=as.numeric(as.vector(marginals$LMA))
    marginals$LMA[is.na(marginals$LMA)]=0
    setnames(marginals,c("LMA","EMP_live", "EMP_work"))
    out.orig$marginals=marginals
    
    #end for i length list.contiguity
  }
  
  setnames(out.orig$clusterList,c("community","cluster","residents"))
  setnames(out.orig$LWClus,c("cluster_live","cluster_work","amount"))
  setnames(out.orig$marginals,c("cluster","amount_live","amount_work"))
  not.tunned.commID=not.tunned.commID[-1]
  return(list(tunned.lma=out.orig,not.tunned.commID=not.tunned.commID))
  
  #end function FineTuning
}


#######################
StatReserveList=function(reserve.list,dat){
  dat=data.table(dat)
  ExtractComp=function(vec,n){vec[n]}
  #number of communities
  NbCom=length(unique(unlist(lapply(reserve.list, ExtractComp,5))))
  
  #number of clusters
  NbClus=length(unique(unlist(lapply(reserve.list, ExtractComp,3))))
  #range of validities
  SummValid=summary(as.numeric(unlist(lapply(reserve.list, ExtractComp,4))))
  #plot(sort(as.numeric(unlist(lapply(reserve.list, ExtractComp,4)))))
  
  #types of reserve.list
  TypesTable=table(unlist(lapply(reserve.list, ExtractComp,1)))
  
  ##clusters ordered by the number of communities 
  #put in the reserve.list
  cluster=unlist(lapply(reserve.list, ExtractComp,3))
  community=unlist(lapply(reserve.list, ExtractComp,5))
  tre=data.table(cluster,community)
  dt=tre[,list(Ncom=.N),by=cluster]
  setorder(dt,Ncom)
  NbUniqueClus=nrow(dt[Ncom==1])
  SummNComByClust=summary(dt[Ncom>1,Ncom])
  
  
  ##number of residents in the reserve.list
  
  Resid.Com=dat[community_live%in%as.numeric(community),list(Residents=sum(amount)),by=community_live]
  setorder(Resid.Com,Residents)
  SummResidByCom=summary(Resid.Com[,Residents])
  
  #number of workes in the reserve.list
  Workers.Com=dat[community_work%in%as.numeric(community),list(Workers=sum(amount)),by=community_work]
  setorder(Workers.Com,Workers)
  SummWorkersByCom=summary(Workers.Com[,Workers])
  #vec2out0=c("A",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[1,community])
  #vec2out0=c("B",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[1,community])
  #vec2out0=c("C",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community],com.cluster2dissolve[ncom+1,community])
  #vec2out0=c("D",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community],com.cluster2dissolve[ncom+1,community])
  #vec2out0=c("E",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community])
  #vec2out0=c("F",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community])
  
  out=list(NumComm=NbCom
           ,NumClus=NbClus
           ,NumUniqueClus=NbUniqueClus
           ,summaryCommByClus=SummNComByClust
           ,summaryValidities=SummValid
           ,TypesTable=TypesTable
           ,Residents=Resid.Com
           ,summaryResidByComm=SummResidByCom
           ,Workers=Workers.Com
           ,summaryWorkersByCom=SummWorkersByCom
  )
  return(out)
  
  #end function stat reserve list
}

#########################
StatClusterData=function(lma,param,threshold,dat){
  
  minSZ=param[1]
  minSC=param[2]
  tarSZ=param[3]
  tarSC=param[4]
  
  
  clusterList=copy(data.table(lma$clusterList[,1:3,with=F]))
  LWClus=copy(data.table(lma$LWClus[,1:3,with=F]))
  marginals=copy(data.table(lma$marginals[,1:3,with=F]))
  
  
  ##STAT PER CLUSTER
  ###validity
  vali=getLeastSelfContained(LWClus, marginals,minSZ,minSC,tarSZ,tarSC)
  if(all(lma$clusterList$cluster>0)){
    vali[[2]]=vali[[2]][cluster>0,]
    #end if
  }
  marginals=merge(marginals,vali[[2]][,list(cluster,validity)],by="cluster",all=T)
  
  ##EMP_LIVE_WORK
  amount_live_work<-LWClus[cluster_live==cluster_work,list(cluster_live,amount)]
  marginals=merge(marginals,amount_live_work,by.x="cluster",by.y="cluster_live",all=T)
  setnames(marginals,"amount","EMP_live_work")
  setnames(marginals,"amount_live","EMP_live")
  setnames(marginals,"amount_work","EMP_work")
  ##SCA SCO
  marginals[,SC_demand_side:=EMP_live_work/EMP_work]
  marginals[,SC_supply_side:=EMP_live_work/EMP_live]
  ###N_COM
  ncom=clusterList[,list(N_com=.N),by=cluster]
  marginals=merge(marginals,ncom,by="cluster",all=T)
  
  ###INTERNAL COEHESION
  uno=merge(clusterList,dat,by.x="community",by.y="community_live")
  if(length(grep("EMP_live",names(uno)))>0){uno[,EMP_live:=NULL]}
  #uno[,community_work:=NULL]
  #uno[,amount:=NULL]
  setnames(uno,"cluster","cluster_live")
  setnames(uno,"community","community_live")
  
  due=merge(uno,clusterList,by.x="community_work",by.y="community")
  if(length(grep("EMP_live",names(uno)))>0){due[,EMP_live:=NULL]}
  
  setnames(due,"cluster","cluster_work")
  
  #selez uguali
  tre=due[cluster_live==cluster_work & community_live!=community_work,.N,by=cluster_live]
  marginals=merge(marginals,tre,by.x="cluster",by.y="cluster_live",all=T)
  marginals[,InternalCohesionLink:=N/(N_com*(N_com-1))]

  
  ###internal IIRL dei flussi
  quattro=due[cluster_live==cluster_work & community_live!=community_work,list(ic=sum(amount)),by=cluster_live]
  cinque=due[cluster_live==cluster_work ,list(icc=sum(amount)),by=cluster_live]
  cinque=merge(cinque,quattro,by="cluster_live")
  cinque[,InternalCohesionFlows:=ic/icc*100]
  marginals=merge(marginals,cinque,by.x="cluster",by.y="cluster_live",all=T)
  
  marginals[,N:=NULL]
  marginals[,ic:=NULL]
  marginals[,icc:=NULL]
  
  ############centrality
  uno=dat[,list(EMP_live=sum(amount)),by=community_live]
  due=dat[,list(EMP_work=sum(amount)),by=community_work]
  tre=dat[community_live==community_work,list(EMP_live_work=sum(amount)),by=community_live]
  uno=merge(uno,due,by.x="community_live",by.y="community_work",all=T)
  uno=merge(uno,tre,by="community_live",all=T)
  uno[,cent:=(EMP_work-EMP_live_work)/(EMP_live-EMP_live_work)]
  uno[,centrality:=0]
  uno[cent>1 & EMP_work>100,centrality:=1]
  uno[,cent:=NULL]
  uno=merge(uno,clusterList,by.x="community_live",by.y="community",all=T)
  uno[is.na(centrality),centrality:=0]
  uno=uno[,list(NbCentralComm=sum(centrality)),by="cluster"]
  marginals=merge(marginals,uno,by="cluster",all=T)
  
  
  ####stats flows
  StatFlows=list()
  #flows : nflows, 
  StatFlows$N_links=nrow(LWClus)
  #perc flows meno di threshold including themselves
  StatFlows$PercNbLinksLessThreshold=nrow(LWClus[amount<threshold])/nrow(LWClus)*100
  
  #nflows in and out, including itself if the case
  StatFlows$summFlows=summary(LWClus$amount)
  StatFlows$summFlowsNoItself=LWClus[cluster_live!=cluster_work,summary(amount)]
  
  #links in and out
  nlinks_in=LWClus[,list(N_links_in=.N),by=cluster_work]
  marginals=merge(marginals,nlinks_in,by.x="cluster",by.y="cluster_work",all=T)
  nlinks_out=LWClus[,list(N_links_out=.N),by=cluster_live]
  marginals=merge(marginals,nlinks_out,by.x="cluster",by.y="cluster_live",all=T)
  
  #flows : nflows, summary flows between clusters, 
  StatFlows$summLinks_in=nlinks_in[,summary(N_links_in)]
  StatFlows$summLinks_out=nlinks_out[,summary(N_links_out)]
  
  
  #cluster with the largest number of links including itself
  StatFlows$clusterMaxNlinks_in=nlinks_in[N_links_in==max(N_links_in),cluster_work]
  #cluster with the largest number of links including itself
  StatFlows$clusterMaxNlinks_out=nlinks_out[N_links_out==max(N_links_out),cluster_live]
  
  #cluster with the minimum number of links including itself
  StatFlows$clusterMinNlinks_in=nlinks_in[N_links_in==min(N_links_in),cluster_work]
  #cluster with the minimum number of links including itself
  StatFlows$clusterMinNlinks_out=nlinks_out[N_links_out==min(N_links_out),cluster_live]
  
  
  ###stats marginals
  StatQuality=list()
  
  ##number clusters
  StatQuality$NbClusters=nrow(marginals)
  #number clusters with unique com
  StatQuality$NbClusterUniqueCom=nrow(marginals[N_com==1])
  #number of clusters validity less 1
  StatQuality$NbClustersValidLess1=nrow(marginals[validity<1])
  #number of clusters validity less 1
  StatQuality$NbClustersNoCentralCom=nrow(marginals[NbCentralComm==0])
  #mean e sd SC_demand_side
  StatQuality$Mean.SC_demand_side=mean(marginals$SC_demand_side,na.rm=T)
  StatQuality$Std.SC_demand_side=sd(marginals$SC_demand_side,na.rm=T)
  
  #mean e sd SC_demand_side
  StatQuality$Mean.SC_supply_side=mean(marginals$SC_supply_side,na.rm=T)
  StatQuality$Std.SC_supply_side=sd(marginals$SC_supply_side,na.rm=T)
  
  ##mean, q1 q3 internalcoehesion flows
  StatQuality$Q1.InternalCohesionFlows=quantile(marginals$InternalCohesionFlows,probs=0.25,na.rm=T)
  StatQuality$Q2.InternalCohesionFlows=median(marginals$InternalCohesionFlows,na.rm=T)
  StatQuality$Q3.InternalCohesionFlows=quantile(marginals$InternalCohesionFlows,probs=0.75,na.rm=T)
  
  ##mean, q1 q3 internalcoehesion link
  StatQuality$Q1.InternalCohesionLink=quantile(marginals$InternalCohesionLink,probs=0.25,na.rm=T)
  StatQuality$Q2.InternalCohesionLink=median(marginals$InternalCohesionLink,na.rm=T)
  StatQuality$Q3.InternalCohesionLink=quantile(marginals$InternalCohesionLink,probs=0.75,na.rm=T)
  
  ##mean, q1 q3 resid
  StatQuality$Q1.EMP_live=quantile(marginals$EMP_live,probs=0.25,na.rm=T)
  StatQuality$Q2.EMP_live=median(marginals$EMP_live,na.rm=T)
  StatQuality$Q3.EMP_live=quantile(marginals$EMP_live,probs=0.75,na.rm=T)
  
  ##mean, q1 q3 work
  StatQuality$Q1.EMP_work=quantile(marginals$EMP_work,probs=0.25,na.rm=T)
  StatQuality$Q2.EMP_work=median(marginals$EMP_work,na.rm=T)
  StatQuality$Q3.EMP_work=quantile(marginals$EMP_work,probs=0.75,na.rm=T)
  
  ##mean, q1 q3 resid work
  StatQuality$Q1.EMP_live_work=quantile(marginals$EMP_live_work,probs=0.25,na.rm=T)
  StatQuality$Q2.EMP_live_work=median(marginals$EMP_live_work,na.rm=T)
  StatQuality$Q3.EMP_live_work=quantile(marginals$EMP_live_work,probs=0.75,na.rm=T)
  
  ##mean, q1 q3 resid work
  StatQuality$Q_modularity=Qmodularity(lma)
  
  
  setnames(marginals,"cluster","LMA")
  
  out=list(marginals=marginals,StatFlows=StatFlows,
           StatQuality=StatQuality,
           param=c(minSZ,minSC,tarSZ,tarSC))
  
  
  return(out)
  #end function statscluster data
}

###########################
DeleteLmaName=function(lma){
  
  clusterData=copyClusterData(lma)
  
  clusterData$clusterList[,com.name:=NULL]
  clusterData$clusterList[,lma.name:=NULL]
  
  clusterData$marginals[,lma.name:=NULL]
  
  clusterData$LWClus[,lma.name.live:=NULL]
  clusterData$LWClus[,lma.name.work:=NULL]
  
  return(clusterData)
  #end function DeleteLmaName
}

############
copyClusterData=function(lma){
  out=list()
  out$clusterList=copy(data.table(lma$clusterList))
  out$LWClus=copy(data.table(lma$LWClus))
  out$marginals=copy(data.table(lma$marginals))
  return(out)
  #end function copyClusterData
}

#############################

#############################
BindPiecesLma=function(input1,input2,LWCom){
  
  ##rinds two partitions
  ##in case a community is given twice, it is not included
  ##A MANUAL ASSIGNMENT SHOULD FOLLOW
  ##the clusters with the same ID are not included
  ##a manual bind should follow
  
  lma1=copyClusterData(input1)
  lma2=copyClusterData(input2)
  
  if(length(intersect(input1$clusterList$community,
                      input2$clusterList$community))>0){
    warning("there are common communities. They will be assigned to the first partition.")
  }
  
  if(length(grep("cluster",names(lma1$clusterList[1])))==1){
    setnames(lma1$clusterList,match("cluster",names(lma1$clusterList[1])),"LMA")
    setnames(lma1$marginals,match("cluster",names(lma1$marginals[1])),"LMA")
    setnames(lma1$LWClus,match("cluster_live",names(lma1$LWClus[1])),"LMA_live")
    setnames(lma1$LWClus,match("cluster_work",names(lma1$LWClus[1])),"LMA_work")
  }
  
  if(length(grep("cluster",names(lma2$clusterList[1])))==1){
    setnames(lma2$clusterList,grep("cluster",names(lma2$clusterList[1])),"LMA")
    setnames(lma2$marginals,match("cluster",names(lma2$marginals[1])),"LMA")
    setnames(lma2$LWClus,match("cluster_live",names(lma2$LWClus[1])),"LMA_live")
    setnames(lma2$LWClus,match("cluster_work",names(lma2$LWClus[1])),"LMA_work")
  }
  
  ###delete the residents
  if(length(grep("residents",names(lma1$clusterList)))>0){lma1$clusterList[,residents:=NULL]}
  if(length(grep("amount",names(lma1$clusterList)))>0){lma1$clusterList[,residents:=NULL]}
  if(length(grep("EMP_live",names(lma1$clusterList)))>0){lma1$clusterList[,residents:=NULL]}
  if(length(grep("residents",names(lma2$clusterList)))>0){lma2$clusterList[,residents:=NULL]}
  if(length(grep("amount",names(lma2$clusterList)))>0){lma2$clusterList[,residents:=NULL]}
  if(length(grep("EMP_live",names(lma2$clusterList)))>0){lma2$clusterList[,residents:=NULL]}
  
  
  tab1=lma1$clusterList[,.N,by=community]
  com1twice=tab1[N>1,community]
  lma1$clusterList=lma1$clusterList[!(community%in%com1twice)]
  
  
  tab2=lma2$clusterList[,.N,by=community]
  com2twice=tab2[N>1,community]
  lma2$clusterList=lma2$clusterList[!(community%in%com2twice)]
  
  
  sameIDlma=intersect(lma1$clusterList$LMA,lma2$clusterList$LMA)
  if(length(sameIDlma)>0){
    maxlma1=lma1$clusterList[,max(LMA)]
    lma2$clusterList[,LMA:=LMA+maxlma1]
  }
  
  coms.not.in.flows=setdiff(c(lma1$clusterList$community,lma2$clusterList$community),
                            c(LWCom$community_live,LWCom$community_work))
  
  lma1$clusterList=lma1$clusterList[!(community%in%coms.not.in.flows)]
  lma2$clusterList=lma2$clusterList[!(community%in%coms.not.in.flows)]
  
  
  common.communities=intersect(lma1$clusterList[,community],lma2$clusterList[,community])
  lma2$clusterList=lma2$clusterList[!community%in%common.communities]
  
  fin=list()
  fin$clusterList=rbind(lma1$clusterList,lma2$clusterList)
  maxlma=fin$clusterList[,max(LMA)]
  
  ##add the other communities from initial flows
  listcom=data.table(community=unique(c(LWCom[,community_live],LWCom[,community_work])))
  fin$clusterList=merge(fin$clusterList,listcom,by.x="community",by.y="community",all.y=T)
  
  #create LMA where missing
  nnew=nrow(fin$clusterList[is.na(LMA)])
  fin$clusterList[is.na(LMA),LMA:=maxlma:(maxlma+nnew-1)]
  rm(maxlma);rm(nnew)
  
  #compute EMP_live using dat
  cici=LWCom[,list(EMP_live=sum(amount)),by=list(community_live)]
  fin$clusterList=merge(fin$clusterList,cici,
                        by.x="community",by.y="community_live")
  rm(cici)
  
  ###
  fin$LWClus <- merge(LWCom, fin$clusterList, by.x = "community_live", by.y = "community",all=T)
  fin$LWClus <- merge(fin$LWClus, data.table(fin$clusterList), by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  fin$LWClus<-fin$LWClus[, list(commuters=sum(amount)),by=list(LMA_live,LMA_work)]
  fin$LWClus[is.na(commuters),commuters:=0]
  
  fin$LWClus[!((is.na(LMA_live) & is.na(LMA_work) & commuters==0))]
  
  # marginals
  liveMarginal <- fin$LWClus[, list(EMP=sum(commuters)), by = list(LMA=LMA_live)]
  workMarginal <- fin$LWClus[, list(EMP=sum(commuters)), by = list(LMA=LMA_work)]
  fin$marginals <- merge(liveMarginal, workMarginal, by="LMA",suffixes = c("_live", "_work"), all=TRUE)
  rm(liveMarginal)
  rm(workMarginal)
  
  setnames(fin$clusterList,c("community","cluster","residents"))
  setnames(fin$LWClus,c("cluster_live","cluster_work","amount"))
  setnames(fin$marginals,c("cluster","amount_live","amount_work"))
  
  out=list(com.twice.1=com1twice,
           com.twice.2=com2twice,
           coms.not.in.flows=coms.not.in.flows,
           LMAtwice=sameIDlma,
           lma=fin
  )
  return(out)
  
  #end function BindPiecesLma
}

############################
PlotLmaCommunity=function(list.lma,lmaIDs,communityID,
                          shp_com,
                          id_shp,
                          bmpfile,col.vec=c("red","orange","yellow")){
  ###this function visualize/plots
  ###the lma containing a given community, comID.
  
  ##it may be used to compare partitions 
  ##or to see the assigment of a given comID during the 
  ##iteration of the algorithm (object intermClusterData when
  ##trace is not NULL in the findClusters
  bmp(paste(bmpfile,".bmp",sep=""),width=1200,height=1200)
  par(mfrow=c(1,2))
  
counter=1
    lma=list.lma[[counter]]
    ##piece for the first element

    x=CreateLMAShape(lma,comIDs="community"
                     ,lmaIDs=lmaIDs
                     ,shp_com=shp_com
                     ,shp_com_name=NULL
                     ,id_shp_com=id_shp
                     ,proj4string=CRS("+proj=longlat +datum=WGS84")
                     ,outdir=NULL
                     ,outfile=NULL
                     ,bmpfile=NULL
                     ,plot_opt=c("green",1,2,"red",1,2,0.8,2))
    
    indice=grep(lmaIDs,names(lma$clusterList))
    current.lma=lma$clusterList[community%in%communityID,indice,with=F]
    com.in.current.lma=lma$clusterList[unlist(lma$clusterList[,indice,with=F])%in%unlist(current.lma),community]
    
    current.lma2=list.lma[[2]]$clusterList[community%in%communityID,indice,with=F]
    com.in.current.lma2=list.lma[[2]]$clusterList[unlist(list.lma[[2]]$clusterList[,indice,with=F])%in%unlist(current.lma2),community]
    
    new.com=setdiff(com.in.current.lma,com.in.current.lma2)
    
    indicecom=grep(id_shp,names(shp_com@data))
    indicelma=grep(lmaIDs,names(x$shp_lma))
    
    #plot(x$shp_lma[x$shp_lma@data[,indicelma]%in%current.lma,],border="red")
    

    plot(shp_com[shp_com@data[,indicecom]%in%com.in.current.lma,],border="gray", main=counter)
    plot(x$shp_lma[x$shp_lma@data[,indicelma]%in%current.lma,],border="red",add=T)
    plot(shp_com[shp_com@data[,indicecom]%in%communityID,],border="gray",col=col.vec[1],add=T)
    
    if(length(new.com)>0){
      plot(shp_com[shp_com@data[,indicecom]%in%new.com,],border="gray",col=col.vec[2],add=T)
      #endif
    }
   
    

    ###
    counter=2
    lma=list.lma[[counter]]
    
      x=CreateLMAShape(lma,comIDs="community"
                       ,lmaIDs=lmaIDs
                       ,shp_com=shp_com
                       ,shp_com_name=NULL
                       ,id_shp_com=id_shp
                       ,proj4string=CRS("+proj=longlat +datum=WGS84")
                       ,outdir=NULL
                       ,outfile=NULL
                       ,bmpfile=NULL
                       ,plot_opt=c("green",1,2,"red",1,2,0.8,2))
      
      indice=grep(lmaIDs,names(lma$clusterList))
      current.lma=lma$clusterList[community%in%communityID,indice,with=F]
      com.in.current.lma=lma$clusterList[unlist(lma$clusterList[,indice,with=F])%in%unlist(current.lma),community]
      
      current.lma1=list.lma[[1]]$clusterList[community%in%communityID,indice,with=F]
      com.in.current.lma1=list.lma[[1]]$clusterList[unlist(list.lma[[1]]$clusterList[,indice,with=F])%in%unlist(current.lma1),community]
      
      new.com=setdiff(com.in.current.lma,com.in.current.lma1)

      indicecom=grep(id_shp,names(shp_com@data))
      indicelma=grep(lmaIDs,names(x$shp_lma))
      
      #plot(x$shp_lma[x$shp_lma@data[,indicelma]%in%current.lma,],border="red")
      
      
      plot(shp_com[shp_com@data[,indicecom]%in%com.in.current.lma,],border="gray", main=counter)
      plot(x$shp_lma[x$shp_lma@data[,indicelma]%in%current.lma,],border="red",add=T)
      plot(shp_com[shp_com@data[,indicecom]%in%communityID,],border="gray",col=col.vec[1],add=T)
      
      if(length(new.com)>0){
        plot(shp_com[shp_com@data[,indicecom]%in%new.com,],border="gray",col=col.vec[3],add=T)
        #endif
      }
      
      
      dev.off()
      
      
      #world.map <- world.map[world.map$AREA > 30000,]
      #plot(ita2001[ita2001@data[,2]==20,])
      
      criteria=FALSE
      if(!identical(list.lma[[counter]]$clusterList[community%in%communityID,],
                    list.lma[[counter-1]]$clusterList[community%in%communityID,])){
        criteria=TRUE
      #end if equal partitions
      }


    
    ##end function PlotLmaCommunity
}

CreateClusterData=function(LWCom,residents=NULL){
  ###creates the clusterData structure starting from
  #flows data
  
  LIST.COM=data.table(c(LWCom[,community_live],LWCom[,community_work]))
  LIST.COM=unique(LIST.COM)[order(V1)]
  
  if(is.null(residents)){
    residents=setcolorder(LWCom[,list(residents=sum(amount)),by=list(Code=community_live)],c("residents","Code"))
    residents=merge(residents,LIST.COM,by.x="Code",by.y="V1",all=T)
    residents[is.na(residents),residents:=0]
    #end if null residents
  }
  
  
  communitiesList <- unique(c(LWCom[,community_live], LWCom[,community_work]))
  
  clusterList <- 1:length(communitiesList)
  
  result <- data.table(community = communitiesList, cluster = clusterList)
  
  clusterList     <- merge(result, residents, by.x = "community", by.y = "Code")
  
  rm(result)
  
  # LWCLus
  names(LWCom)[grep("_live",names(LWCom))][1]="community_live"
  names(LWCom)[grep("_work",names(LWCom))][1]="community_work"
  
  LWClus <- merge(LWCom, data.table(clusterList), by.x = "community_live", by.y = "community",all=T)
  LWClus <- merge(LWClus, data.table(clusterList), by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  out<-LWClus[, list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  out[is.na(amount),amount:=0]
  
  #13/01/2016
  out[,col:=(is.na(cluster_live) & is.na(cluster_work) & amount==0)]
  out=out[!(col)]
  out[,col:=NULL]
  
  #13/01/2016
  #dt
  LWClus <- out
  rm(out)
  
  # marginals
  
  
  # 14/12/15 ATTENZIONE la riga sotto non ha senso. forse ereditata e nella versione attuale 
  # superata 
  #   marginals[is.na(marginals)] <- 0
  
  liveMarginal <- LWClus[, list(amount=sum(amount)), by = list(cluster=cluster_live)]
  workMarginal <- LWClus[, list(amount=sum(amount)), by = list(cluster=cluster_work)]
  #DT
  # 20140109: EHVL: added "all=TRUE" to perform full outer join. Without this all clusters that did not
  # have both residents and workers were excluded from the algorithm
  ### in the original function the merge was loosing all the communities which had residents but not workers or
  ### viceversa community which had workers but not residents again it was solved by using the option exclude.
  
  marginals <- merge(liveMarginal, workMarginal, by="cluster",suffixes = c("_live", "_work"), all=TRUE)
  
  rm(liveMarginal)
  rm(workMarginal)
  
  
  
  
  clusterData <- list(clusterList = clusterList, LWClus = LWClus, marginals = marginals)
  
  return(clusterData)
  
  #end function createclusterData
}

#########################
Qmodularity<-function(lma)
{
  clusterList=copy(data.table(lma$clusterList[,1:3,with=F]))
  LWClus=copy(data.table(lma$LWClus[,1:3,with=F]))
  marginals=copy(data.table(lma$marginals[,1:3,with=F]))
  
  
  ###names
  n=names(clusterList[1])
  if(all(n==c("community", "LMA"  ,     "EMP_live"))){
    n=c("community", "cluster"  ,     "EMP_live")
    setnames(clusterList,n)
  }
  
  n=names(LWClus[1])
  if(all(n==c("LMA_live" , "LMA_work" , "commuters")) | 
     all(n==c("LMA_live" , "LMA_work" , "amount"))){
    n=c("cluster_live" , "cluster_work" , "amount")
    setnames(LWClus,n)
  }
  
  n=names(marginals[1])
  if(all(n==c("LMA" , "EMP_live"  , "EMP_work")) | 
     all(n==c("cluster" , "EMP_live"  , "EMP_work"))){
    n=c("cluster" ,  "amount_live" ,"amount_work")
    setnames(marginals,n)
  }
  
  area=unique(c(LWClus[,cluster_live],c(LWClus[,cluster_work])))
  flussi=matrix(0,length(area),length(area))
  flussi[cbind(match(LWClus[,cluster_live],area),match(LWClus[,cluster_work],area))]=LWClus[,amount]
  mat_ind<-flussi-as.matrix(rowSums(flussi))%*%t(as.matrix(colSums(flussi)))/sum(flussi)
  Q_index<-sum(diag(mat_ind))/sum(flussi)
  return(Q_index)
  #end function q_modularity
}

##########################################
CompareLMAsStat=function(list.lma,dat){
  
  stat.mat=matrix(0,1,28)
  for(i in 1:length(list.lma)){
    m=StatClusterData(list.lma[[i]]$lma,list.lma[[i]]$param,
                      threshold = 1000,dat=dat)$StatQuality
    m=c(list.lma[[i]]$param,unlist(m))
    names(m)[1:4]=c("minSZ","minSC","tarSZ","tarSC")
    stat.mat=rbind(stat.mat,m)
    
    #end for
  }
  stat.mat=stat.mat[-1,]
  colnames(stat.mat)=names(m)
  return(stat.mat)
  #end function
}
