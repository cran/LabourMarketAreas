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
                         ,"lma_commuter_percent"
                         ,"Home_Work_Ratio"
                         ,"link"
                         ,"cinque"
                         ,"sei"
                         ,"V5"
                         ,"V6"
                         ,"shape"
                         ,"shape_ref"
                         ,"shapi"
                         ,"shape_ref_ID"
                         ,"indice"
                         ,"stats"
                         ,"corrip"
                         ,"nome_shapi"
                         ,"shapi_good"
                         ,"area"
                         ,"selected_output"
                         ,"area_intersection"
                         ,"shape_area"
                         ,"shape_ref_area"
                         ,"shape_EMP_live"
                         ,"shape_ref_EMP_work"
                         ,"perc_intersection_shape"
                         ,"perc_intersection_shape_ref"
                         ,"shape_EMP_work"
                         ,"shape_ref_EMP_live"
                         ,"."
                         ,"xx"
                         
)
,add=T)





findClusters <- function(LWCom,minSZ,minSC,tarSZ,tarSC, verbose=F,sink.output=NULL,trace=NULL,PartialClusterData=NULL, idcom_type=NULL) {
  
  
  
  param=c(minSZ,minSC,tarSZ,tarSC)
  
  setDT(LWCom)
  
  n=names(LWCom[1])
  if(any(sort(n)!=c("amount","community_live","community_work"))){
    stop("ERROR: please check the names of the commuting flows data.")
  }
  
  LWCom=setcolorder(LWCom,c("community_live","community_work", "amount"))
  
  
  LIST.COM.alpha=NULL
  if(!is.null(idcom_type)){
    LIST.COM.alpha=data.table(c(LWCom[,community_live],LWCom[,community_work]))
    LIST.COM.alpha=unique(LIST.COM.alpha)[order(V1)] 
    LIST.COM.alpha$link=1:nrow(LIST.COM.alpha)
    LWCom=merge(LWCom,LIST.COM.alpha, by.x="community_live", by.y="V1", all.x=T)
    LWCom=LWCom[,community_live:=NULL]
    setnames(LWCom,"link","community_live")
    LWCom=merge(LWCom,LIST.COM.alpha, by.x="community_work", by.y="V1", all.x=T)
    LWCom=LWCom[,community_work:=NULL]
    setnames(LWCom,"link","community_work")
    setcolorder(LWCom,c("community_live","community_work", "amount") )
  }
  
  LWCom=LWCom[order(community_live)]
  
  
  
  
  if(!all(is.integer(LWCom[,community_live]))){
    stop("ERROR: community_live is not always integer.")
  }
  
  if(!all(is.integer(LWCom[,community_work]))){
    stop("ERROR: community_work is not always integer.")
  }
  
  if(!all(LWCom[,community_live>0])){
    stop("ERROR: community_live is not always positive.")
  }
  if(!all(LWCom[,community_work>0])){
    stop("ERROR: community_work is not always positive.")
  }
  
  if(!all(is.numeric(LWCom[,amount]))){
    stop("ERROR: amount is not always integer.")
  }
  
  if(minSZ<=0 | minSC<=0 | tarSZ<=0| tarSC<=0){
    stop("ERROR self-containment and target parameters should be positive numbers")
  }
  
  
  
  
  
  LIST.COM=data.table(c(LWCom[,community_live],LWCom[,community_work]))
  LIST.COM=unique(LIST.COM)[order(V1)]
  
 
  codes.0.full=-1
  criterio.zero=0
  zero.list=list(Communities=NULL,LWCom=NULL,Residents=NULL,
                 Workers=NULL)
  LWCom.origperzero=copy(LWCom)
  
  while(criterio.zero==0){
    
    residents=setcolorder(LWCom[,list(residents=sum(amount)),by=list(Code=community_live)],c("residents","Code"))
    residents=merge(residents,LIST.COM,by.x="Code",by.y="V1",all=T)
    residents[is.na(residents),residents:=0]
    
    workers=setcolorder(LWCom[,list(workers=sum(amount)),by=list(Code=community_work)],c("workers","Code"))
    workers=merge(workers,LIST.COM,by.x="Code",by.y="V1",all=T)
    workers[is.na(workers),workers:=0]
    
    ####check number of residents and warning if resident=0
    ## communities with residents=0 will be written in the warning file, on the screen and in the sink file.
    
    codes.0<-sort(unique(residents$Code[residents$residents==0]))
    codes.0<-unique(c(codes.0,sort(unique(workers$Code[workers$workers==0]))))
   
    temp=LWCom
    temp=merge(temp,residents,by.x="community_live",by.y="Code",all=T)
    temp=merge(temp,workers,by.x="community_work",by.y="Code",all=T)
    temp=temp[amount==residents & amount==workers & community_live==community_work]
    codes.0=unique(c(codes.0,temp[,community_work]))
    
    
    codes.0=sort(codes.0)
   
    
    criterio.zero=1
    if(length(codes.0)>=1){
      criterio.zero=0
      codes.0.full=c(codes.0.full,codes.0)
      codes.0.full=unique(codes.0.full[codes.0.full>0])
      print("WARNING:")
      print("Please check the zero.list component of the output. It is not empty.")
      print("The following communities will NOT assigned by the algorithm:")
      print(codes.0.full)
     
      
      
      residents<-residents[!(Code%in%codes.0.full),]
      workers<-workers[!(Code%in%codes.0.full),]
      LWCom<-LWCom[!(community_live%in%codes.0.full),]
      LWCom<-LWCom[!(community_work%in%codes.0.full),]
      LIST.COM<-LIST.COM[!(V1%in%codes.0.full),]
      #end if length(codes.0)>1
    }
    
    #end while criterio.zero
  } 
  
  codes.0.full=codes.0.full[codes.0.full>0]
  
  
  zero.list$Communities=codes.0.full
  zero.list$LWCom=LWCom.origperzero[community_work%in%codes.0.full | community_live%in%codes.0.full ,]
  zero.list$Residents=setcolorder(zero.list$LWCom[,list(residents=sum(amount)),by=list(Code=community_live)],c("residents","Code"))
  zero.list$Workers=setcolorder(zero.list$LWCom[,list(workers=sum(amount)),by=list(Code=community_work)],c("workers","Code"))
  
  
  rm(LWCom.origperzero) ;gc()
  
  
  fict.community=LIST.COM[,max(V1)*10]
  LWCom=rbind(LWCom,data.table(matrix(rep(fict.community,ncol(LWCom)),1)),use.names=F)
  LWCom[community_live==fict.community,amount:=0]
  residents=rbind(residents,data.table(matrix(c(fict.community,0),1)),use.names=F)
  
  
  clusterData=CreateClusterData(LWCom,residents=residents)
  communitiesList <- clusterData$clusterList[,community]
  
  
  
  fict.cluster=clusterData$clusterList[community==fict.community, cluster]
  clusterData$clusterList[cluster==fict.cluster, cluster:=0]
  clusterData$LWClus[cluster_live==fict.cluster, cluster_live:=0]
  clusterData$LWClus[cluster_work==fict.cluster, cluster_work:=0]
  clusterData$marginals[cluster==fict.cluster, cluster:=0]
  
  CommunityWorkersOrig=merge(clusterData$clusterList,clusterData$marginals,by="cluster")[,list(community,amount_work)]
  
  Community_live_work= LWCom[community_live==community_work,list(community=community_live,live_work=amount)]
  
  if(!is.null(sink.output)){
    sink(file=sink.output,split=T)
  }
  counter <- 1
  
  reserve.list=list()
  counter.list=1
  ComNotAssigned.list=list()
  clusterDataInterm=list()
  communitiesMovements=data.table(community=communitiesList)
  communitiesMovements[,moves:=0]
  setkey(communitiesMovements,community)
  traceeval=!is.null(trace)
  
  
  if(!is.null(PartialClusterData)){
    clusterData=copyClusterData(PartialClusterData)
  }
  
  
  repeat {
    leastSelfContainedFULL <- getLeastSelfContained(clusterData$LWClus, clusterData$marginals,minSZ,minSC,tarSZ,tarSC)
    leastSelfContained=leastSelfContainedFULL[[1]]
    
    if (verbose) {
      print(leastSelfContained$validity, digits = 20)
    }
    if (leastSelfContained$validity>=1) { 
      print("The algorithm has converged.")
      break
    }
    
    
    com.cluster2dissolve<-clusterData$clusterList[cluster%in%leastSelfContained$cluster,]
    n.com.cluster2dissolve<-nrow(com.cluster2dissolve)
    
    
    if(n.com.cluster2dissolve>1){
      WorkersFromOutside=LWCom[xor(LWCom$community_live%in%com.cluster2dissolve$community, LWCom$community_work%in%com.cluster2dissolve$community),]
      WorkersFromOutside2=WorkersFromOutside[,list(NoToFrom=sum(amount)),by=list(community=community_work)]
      
      com.cluster2dissolve=merge(com.cluster2dissolve,WorkersFromOutside2,by="community",all.x=T)
      com.cluster2dissolve[is.na(NoToFrom),NoToFrom:=0]
      com.cluster2dissolve= merge(com.cluster2dissolve,Community_live_work, by="community",all.x=T)
      com.cluster2dissolve[is.na(live_work),live_work:=0]
      com.cluster2dissolve=com.cluster2dissolve[,orderRegroup:=NoToFrom + residents -live_work ][order(-orderRegroup)][,list(community,cluster,residents)]
      
    }
    
    
    
    lwcom=LWCom[community_live%in% com.cluster2dissolve$community | community_work%in% com.cluster2dissolve$community,]
    clusterData <- dissolveClusterSel(clusterData, leastSelfContained[,cluster], lwcom)
    
    
    
    
    if(n.com.cluster2dissolve==1){
      
      otim=regroupDissolved(copy(clusterData))
      if(is.list(otim)){
        clusterData.new <- otim[[1]]
        candcluster=otim[[2]]
      }else{clusterData.new=otim}
      
      
      
      
      if(is.list(clusterData.new)){
        
        sotto=c(candcluster$cluster2,com.cluster2dissolve$cluster)
        validity.new=getLeastSelfContained(copy(clusterData.new)$LWClus[cluster_live%in%sotto | cluster_work%in%sotto,], copy(clusterData.new)$marginals[cluster%in%sotto,], minSZ,minSC,tarSZ,tarSC)
        
        
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][cluster%in%sotto]
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][!cluster%in%leastSelfContainedFULL[[1]][,cluster]]
        
        if(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity])){
          
          rm(clusterData)
          clusterData=copy(clusterData.new)
          communitiesMovements[community==com.cluster2dissolve[1,community],moves:=moves+1]
          
          
        }
        if(!(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity]))){  
          vec2out0=c("A",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[1,community])
          reserve.list[[counter.list]]=vec2out0
          counter.list=counter.list+1
          
          clusterData$clusterList[cluster==-1,cluster:=0]
          clusterData$LWClus[cluster_live==-1,cluster_live:=0]
          clusterData$LWClus[cluster_work==-1,cluster_work:=0]
          clusterData$marginals[cluster==-1,cluster:=0]
          
          
          
          clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
          clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
          
        }
        
      }
      
      if(is.numeric(clusterData.new)){
        vec2out0=c("B",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[1,community])
        
        reserve.list[[counter.list]]=vec2out0
        counter.list=counter.list+1
        
        clusterData$clusterList[cluster==-1,cluster:=0]
        clusterData$LWClus[cluster_live==-1,cluster_live:=0]
        clusterData$LWClus[cluster_work==-1,cluster_work:=0]
        clusterData$marginals[cluster==-1,cluster:=0]
        
        
        
        clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
        clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
        
        
      }
    }
    
    
    if(n.com.cluster2dissolve>1){
      
      ncom=1
      index.com.2diss=clusterData$clusterList[community==com.cluster2dissolve[ncom,community],cluster]
      
      otim=regroupDissolved.ncom(copy(clusterData),index.com.2diss)
      if(is.list(otim)){
        clusterData.new <- otim[[1]]
        candcluster=otim[[2]]
      }else{clusterData.new=otim}
      
      
      
      if(is.list(clusterData.new)){
        
        sotto=c(candcluster$cluster2,com.cluster2dissolve$cluster)
        validity.new=getLeastSelfContained(copy(clusterData.new)$LWClus[cluster_live%in%sotto | cluster_work%in%sotto,], copy(clusterData.new)$marginals[cluster%in%sotto,], minSZ,minSC,tarSZ,tarSC)
        
        
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][cluster%in%sotto]
        
        leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][!cluster%in%leastSelfContainedFULL[[1]][,cluster]]
        
        
        if(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity])){
          
          rm(clusterData)
          clusterData=copy(clusterData.new)
          communitiesMovements[community==com.cluster2dissolve[ncom,community],moves:=moves+1]
          
          
          
          clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
          clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
          
        }
        
        if(!(all(validity.new[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity]))){
          
          vec2out0=c("C",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community],com.cluster2dissolve[ncom+1,community])
          
          reserve.list[[counter.list]]=vec2out0
          counter.list=counter.list+1
          
          clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
          clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
          clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
          clusterData$marginals[cluster==index.com.2diss,cluster:=0]
          
          
          
          
          clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
          clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
          
        }
        
        
        clusterData$clusterList[cluster<0, cluster:=leastSelfContainedFULL[[1]][1,cluster]]
        clusterData$LWClus[cluster_live<0,cluster_live:=leastSelfContainedFULL[[1]][1,cluster]]
        clusterData$LWClus[cluster_work<0, cluster_work:=leastSelfContainedFULL[[1]][1,cluster]]
        clusterData$marginals[cluster<0,cluster:=leastSelfContainedFULL[[1]][1,cluster]]
        
      }
      
      if(is.numeric(clusterData.new)){
        
        vec2out0=c("D",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community],com.cluster2dissolve[ncom+1,community])
        
        reserve.list[[counter.list]]=vec2out0
        counter.list=counter.list+1 
        
        clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
        clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
        clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
        clusterData$marginals[cluster==index.com.2diss,cluster:=0]
        
        
        clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
        clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
        
        
        
        if(n.com.cluster2dissolve>1){
        for(ncom in 2:n.com.cluster2dissolve){
          
          leastSelfContainedFULL <- getLeastSelfContained(clusterData$LWClus, clusterData$marginals, minSZ,minSC,tarSZ,tarSC)
          leastSelfContained=leastSelfContainedFULL[[1]]
          
          
          rm(index.com.2diss)
          index.com.2diss=leastSelfContained[,cluster]
          
          otim=regroupDissolved.ncom(copy(clusterData),index.com.2diss)
          if(is.list(otim)){
            clusterData.p <- otim[[1]]
            candcluster=otim[[2]]
          }else{clusterData.p=otim}
          
          
          if(is.list(clusterData.p)){
            
            
            if(is.list(otim)){sotto=c(candcluster$cluster2)}else{sotto=0}
            
            validity.p=getLeastSelfContained(clusterData.p$LWClus[cluster_live%in%sotto | cluster_work%in%sotto,], clusterData.p$marginals[cluster%in%sotto,], minSZ,minSC,tarSZ,tarSC)
            
            
            
            leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][cluster%in%sotto]
            
            leastSelfContainedFULL[[2]]=leastSelfContainedFULL[[2]][!cluster%in%leastSelfContainedFULL[[1]][,cluster]]
            
            if(all(validity.p[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity])){
              rm(clusterData)        
              clusterData=copy(clusterData.p)
              communitiesMovements[community==com.cluster2dissolve[ncom,community],moves:=moves+1]
              
            }
            
            if(!(all(validity.p[[2]][cluster>0,validity]>=leastSelfContainedFULL[[2]][cluster>0,validity]))){
              
              vec2out0=c("E",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community])
              reserve.list[[counter.list]]=vec2out0
              counter.list=counter.list+1
              
              clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
              clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
              clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
              clusterData$marginals[cluster==index.com.2diss,cluster:=0]
              
              clusterData$LWClus[,temp:=NULL]
              
              
              
              clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
              clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
              
              
              
            }
            
          }
          if(is.numeric(clusterData.p)){
            vec2out0=c("F",counter,leastSelfContainedFULL[[1]][1,cluster],leastSelfContainedFULL[[1]][1,validity],com.cluster2dissolve[ncom,community])
            reserve.list[[counter.list]]=vec2out0
            counter.list=counter.list+1
            
            clusterData$clusterList[cluster==index.com.2diss,cluster:=0]
            clusterData$LWClus[cluster_live==index.com.2diss,cluster_live:=0]
            clusterData$LWClus[cluster_work==index.com.2diss,cluster_work:=0]
            clusterData$marginals[cluster==index.com.2diss,cluster:=0]
            
            clusterData$LWClus[,temp:=NULL]
            clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
            clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
            
            
          }
          
        }
        }
        
      }
      
    }
    
    
    
    
    
    if (verbose) {
      print("end of loop number ")
      print(counter)
    }
    
    if(traceeval){ 
      if(counter%%trace==0){
        clusterDataInterm[[length(clusterDataInterm)+1]]=list(lma=clusterData,param=param)
      }
    }
    counter <- counter + 1
    
  }
  
  
  if(traceeval){
    clusterDataIterations=clusterDataInterm
    save(clusterDataIterations,file=paste(minSZ,minSC,tarSZ,tarSC,"interm.clusterData.Rdata",sep=" "))
  }
  
  clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
  clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  
  if(!is.null(sink.output)){
    sink()
  }
  
  
  clusterData$clusterList=clusterData$clusterList[community!=fict.community]
  clusterDataBeforeZeroCluster=copy(clusterData)
  com.cluster2dissolve<-clusterData$clusterList[cluster==0]
  n.com.cluster2dissolve<-nrow(com.cluster2dissolve)
  if(n.com.cluster2dissolve>0){
    
    clusterData <- dissolveCluster(clusterData, 0, LWCom)
    
    if(n.com.cluster2dissolve>1){
      
      WorkersFromOutside=LWCom[xor(community_live%in%com.cluster2dissolve[,community], community_work%in%com.cluster2dissolve[,community])]
      
      WorkersFromOutside2=WorkersFromOutside[,list(NoToFrom=sum(amount)),by=list(community=community_work)]
      com.cluster2dissolve=merge(com.cluster2dissolve,WorkersFromOutside2,by="community",all.x=T)
      com.cluster2dissolve[is.na(NoToFrom),NoToFrom:=0]
      
      com.cluster2dissolve= merge(com.cluster2dissolve,Community_live_work, by="community",all.x=T)
      
      com.cluster2dissolve[is.na(live_work),live_work:=0]
      
      com.cluster2dissolve[,orderRegroup:=  NoToFrom + residents -live_work ]
      
      com.cluster2dissolve=com.cluster2dissolve[order(-orderRegroup)]
      
      com.cluster2dissolve=com.cluster2dissolve[,list(community,cluster,residents)]
      
    }
    
   
    jj=1
    while(jj==1){
      jj=0
      com.cluster2dissolve<-clusterData$clusterList[cluster<0]
      n.com.cluster2dissolve2<-nrow(com.cluster2dissolve)
      if(n.com.cluster2dissolve2>0){
      for(j in 1:n.com.cluster2dissolve2){
         index.com.2diss=clusterData$clusterList[community==com.cluster2dissolve[j,community],cluster]
        clusterData.new <- regroupDissolved.ncom(clusterData,index.com.2diss)[[1]]
        if(is.list(clusterData.new)){clusterData=clusterData.new
        jj=1}
      }
      #end for j ncom.2dissolve
      #end if nrow>1
      }
      # end for jj
    }
    #end if com2dissolve>0
  }
  
 ComNotAssigned.list=list(clusterData$clusterList[cluster<0, community])
  clusterData$clusterList[cluster<0,cluster:=0]
  clusterData$LWClus[cluster_live<0, cluster_live:=0]
  clusterData$LWClus[cluster_work<0, cluster_work:=0]
  clusterData$marginals[cluster<0,cluster:=0]
 
  clusterData$marginals=clusterData$marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
  clusterData$LWClus=clusterData$LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
 
  
  
  
  
  
  
  if (verbose) {
    print(clusterData$clusterList)
  }
  
  
  
  setorder(clusterData$clusterList,community)
  setorder(clusterData$LWClus,cluster_live, cluster_work)
  setorder(clusterData$marginals,cluster)
  
  setorder(clusterDataBeforeZeroCluster$clusterList,community)
  setorder(clusterDataBeforeZeroCluster$LWClus,cluster_live, cluster_work)
  setorder(clusterDataBeforeZeroCluster$marginals,cluster)
  
  
  Y=copy(clusterData$LWClus[!(cluster_live==0 & cluster_work==0)])
  Z=copy(clusterData$marginals[cluster!=0])
  
  if(!is.null(idcom_type)){setnames(LIST.COM.alpha,c("community","link"))}
  
  out=list(lma=list(clusterList=clusterData$clusterList,LWClus=Y,marginals=Z),
           lma.before0=clusterDataBeforeZeroCluster,
           reserve.list=reserve.list,
           comNotAssigned=ComNotAssigned.list,
           zero.list=zero.list,
           communitiesMovements=communitiesMovements,
           param=c(minSZ,minSC,tarSZ,tarSC),
           idcom_rel=LIST.COM.alpha)
  
  if(!is.null(idcom_type)){
    out$lma$clusterList=merge(LIST.COM.alpha,out$lma$clusterList,by.x="link",by.y="community",all.y=T)
    out$lma$clusterList[,link:=NULL]
  
    out$lma.before0$clusterList=merge(LIST.COM.alpha,out$lma.before0$clusterList,by.x="link",by.y="community",all.y=T)
    out$lma.before0$clusterList[,link:=NULL]
  
    out$communitiesMovements=merge(LIST.COM.alpha,out$communitiesMovements,by.x="link",by.y="community",all.y=T)
    out$communitiesMovements[,link:=NULL]
    
    if(!is.null(out$zero.list)){
      if(length(out$zero.list)>0){
        out$zero.list$Communities=merge(data.table(link=out$zero.list$Communities),LIST.COM.alpha,by="link")$community
        
        out$zero.list$Residents=merge(out$zero.list$Residents,LIST.COM.alpha,by.x="Code",by.y="link")
        out$zero.list$Residents[,Code:=NULL]
        
        out$zero.list$Workers=merge(out$zero.list$Workers,LIST.COM.alpha,by.x="Code",by.y="link")
        out$zero.list$Workers[,Code:=NULL]
        
        
      if(nrow(out$zero.list$LWCom>0)){
      
        out$zero.list$LWCom=merge(out$zero.list$LWCom,LIST.COM.alpha,by.x="community_live",by.y="link")
        out$zero.list$LWCom[,community_live:=NULL]
        setnames(out$zero.list$LWCom,"community","community_live")
        
        out$zero.list$LWCom=merge(out$zero.list$LWCom,LIST.COM.alpha,by.x="community_work",by.y="link")
        out$zero.list$LWCom[,community_work:=NULL]
        setnames(out$zero.list$LWCom,"community","community_work")
        
      }
      }
    }
    
    temp=unlist(out$comNotAssigned)
    length(temp)
    if(!is.null(temp)){
      if(length(temp)>0){
        temp=data.table(link=temp)
        out$comNotAssigned=list(merge(temp,LIST.COM.alpha,by="link")$community)
      }
    } 
    rm(temp)
    
    temp=lapply(out$reserve.list, length)
    sei=out$reserve.list[temp==6]
    cinque=out$reserve.list[temp==5]
    
    ####QUI QUANDO SEI VUOTO E IDCOM_TYPE NON VUOTO, QUI DA ERRORE
    #DICENDO CHE SI ASPETTA UN VEC NON UN DATO NULLO
    if(!is.null(sei) & is.list(sei)){
      sei=data.table(matrix(unlist(sei),length(sei),6,byrow=T))
      sei[,V6:=as.integer(V6)]
      sei=merge(sei,LIST.COM.alpha,by.x="V6",by.y="link")
    
      sei[,V6:=NULL]
      sei=split(sei,list(sei$community))
      
      
      }
    if(is.null(sei) | !is.list(sei)){sei=0}
    
    if(!is.null(cinque) & is.list(cinque)){
      cinque=data.table(matrix(unlist(cinque),length(cinque),5,byrow=T))
      cinque[,V5:=as.integer(V5)]
      cinque=merge(cinque,LIST.COM.alpha,by.x="V5",by.y="link")
      cinque[,V5:=NULL]
            cinque=split(cinque,list(cinque$community))
      
      }
    
    if(is.null(cinque) | !is.list(cinque)){cinque=0}
    
    out$reserve.list=list(cinque,sei)
    
  out$communitiesMovements=out$communitiesMovements[!is.na(community)]
  
  }
  
  
  
  
  return(out)
}


mergeCluster <- function(clusterData, cluster1, cluster2) {
  
  
  
  clusterList=clusterData$clusterList
  clusterList[cluster==cluster1,cluster:=cluster2]
  
  
  
  
  LWClus=clusterData$LWClus
  LWClus[cluster_live==cluster1, cluster_live:=cluster2]
  LWClus[cluster_work==cluster1, cluster_work:=cluster2]
  LWClus=LWClus[,list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  
  
  marginals=clusterData$marginals
  marginals[cluster==cluster1, cluster:=cluster2] 
  marginals=marginals[,list(amount_live=sum(amount_live),amount_work=sum(amount_work)),by=cluster]
  
  
  
  
  return(list(clusterList=clusterList,  LWClus=LWClus , marginals=marginals))
}






getLeastSelfContained <- function(LWClus, marginals,minSZ,minSC,
                                  tarSZ,tarSC) {
  
  setDT(LWClus)
  setDT(marginals)
  
  
  
  
  LWClus=LWClus[!is.na(cluster_live) & !is.na(cluster_work),]
  marginals=marginals[!is.na(cluster)]
  
  setkey(LWClus,cluster_live,cluster_work)
  setkey(marginals,cluster)
  
  indi=1:nrow(LWClus)
  indi=indi[LWClus$cluster_live==0 | LWClus$cluster_work==0]
  if(length(indi)>0){LWClus=LWClus[-indi,]}
  
  indi=1:nrow(marginals)
  indi=indi[marginals$cluster==0]
  if(length(indi)>0){marginals=marginals[-indi,]}
  
  LWSelf <- LWClus[cluster_live == cluster_work, list(cluster_live, amount)]
  
  
  x=try(LWSelf.temp <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T),silent=T)
  if("try-error"%in%class(x)){
    LWSelf <- merge(marginals, LWSelf, by.x = "cluster", by.y = "cluster_live",all.x=T,allow.cartesian = T)
  }
  
  if(!"try-error"%in%class(x)){
    LWSelf=x
  }
  
  
  LWSelf[is.na(amount),amount:=0]
  
  LWSelf[,msc:=min(amount/amount_live,amount/amount_work), by=1:nrow(LWSelf)]
  
  
  LWSelf[,':='(Y=msc/tarSC, sizeMeasure=(tarSZ - amount_live) / (tarSZ - minSZ))]
  LWSelf[Y > 1, Y:=1]
  LWSelf[sizeMeasure < 0, sizeMeasure:=0]
  LWSelf[,Z:=1 - ((1 - (minSC / tarSC))*sizeMeasure)]
  LWSelf[,validity:=Y * Z * (tarSC / minSC)]
  
  
  
  LWSelf[msc==1, validity:=1]
  
  
  LWSelf=rbind(LWSelf,data.table(matrix(rep(0,ncol(LWSelf)),1)),use.names=F)
  
  LWSelf[cluster==0,validity:=1]
  
  minivalout=LWSelf[validity==min(validity,na.rm=T),list(cluster,validity)][1,]
  
  
  
  setorder(LWSelf,cluster)
  out=list(minvalout=minivalout,LWSelf=LWSelf )
  
  return(out)
  
  
}

dissolveCluster <- function(clusterData, cluster, LWCom) {
  
  setDT(LWCom)
  setcolorder(LWCom,c("community_live","community_work","amount"))
  
  temp=cluster
  sizeDissolve=clusterData$clusterList[cluster%in%temp,.N]
  clusterData$clusterList[cluster%in%temp,cluster:=-(1:sizeDissolve)]
  setkey(clusterData$clusterList,cluster)
  
  setnames(LWCom,grep("_live",names(LWCom[1]))[1],"community_live")
  setnames(LWCom,grep("_work",names(LWCom[1]))[1],"community_work")
  
  LWClus <- merge(LWCom, data.table(clusterData$clusterList), by.x = "community_live", by.y = "community",all=T)
  LWClus <- merge(LWClus, data.table(clusterData$clusterList), by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  out<-LWClus[, list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  out[is.na(amount),amount:=0]
  
  
  clusterData$LWClus=out[!(is.na(cluster_live) & is.na(cluster_work) & amount==0),]
  
  setkey(clusterData$LWClus,cluster_live,cluster_work)
  
  liveMarginal <- clusterData$LWClus[, list(amount=sum(amount)), keyby = list(cluster=cluster_live)]
  workMarginal <- clusterData$LWClus[, list(amount=sum(amount)), keyby = list(cluster=cluster_work)]
  
  clusterData$marginals  <- merge(liveMarginal, workMarginal, by="cluster",suffixes = c("_live", "_work"), all=TRUE)
  
  rm(liveMarginal)
  rm(workMarginal)
  
  return(clusterData)
}



dissolveClusterSel <- function(clusterData, cluster, lwcom) {
  setDT(lwcom)
  setcolorder(lwcom,c("community_live","community_work","amount"))
  
  temp=cluster
  sizeDissolve=clusterData$clusterList[cluster%in%temp,.N]
  set(clusterData$clusterList,which(clusterData$clusterList$cluster%in%temp),"cluster",-(1:sizeDissolve))
  
  pezzodanonmodificare=clusterData$LWClus[cluster_live!=cluster & cluster_work!=cluster]
  
  lwclus <- merge(lwcom, clusterData$clusterList[community%in%lwcom[,community_live]], by.x = "community_live", by.y = "community",all=T)
  lwclus <- merge(lwclus,clusterData$clusterList[community%in%lwcom[,community_work]], by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  out<-lwclus[, list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  out=rbindlist(list(pezzodanonmodificare,out))
  set(out,which(is.na(out$amount)),"amount",0)
  out=out[!(is.na(cluster_live) & is.na(cluster_work) & amount==0),]
  
  clusterData$LWClus=out[!(is.na(cluster_live) & is.na(cluster_work) & amount==0),]
  
  
  
  liveMarginal <- clusterData$LWClus[, list(amount=sum(amount)), keyby = list(cluster=cluster_live)]
  workMarginal <- clusterData$LWClus[, list(amount=sum(amount)), keyby = list(cluster=cluster_work)]
  
  clusterData$marginals  <- merge(liveMarginal, workMarginal, by="cluster",suffixes = c("_live", "_work"), all=TRUE)
  
  rm(liveMarginal)
  rm(workMarginal)
  
  return(clusterData)
}





determineCohesion <- function(LWClus, marginals) {
  setDT(LWClus)
  setDT(marginals)
  setkey(LWClus,cluster_live,cluster_work)
  setkey(marginals,cluster)
  
  cohesion <- LWClus[cluster_live != cluster_work, ]
  
  
  cohesion <- merge(cohesion, marginals[, list(cluster, amount_live)], 
                    by.x = "cluster_live", by.y = "cluster")
  cohesion <- merge(cohesion, marginals[, list(cluster, amount_work)], 
                    by.x = "cluster_work", by.y = "cluster")
  
  
  
  
  cohesion[,strengthOnesided:=0]
  cohesion[!((amount_live==0)|(amount_work==0)),strengthOnesided:=amount / amount_live * amount / amount_work] 
  
  
  
  
  cohesion[,setClusterSwap:=cluster_live > cohesion$cluster_work]
  cohesion[setClusterSwap==TRUE, temp:=cluster_live][setClusterSwap==TRUE,cluster_live:=cluster_work] [setClusterSwap==TRUE,cluster_work:=temp][,':='(temp=NULL,setClusterSwap=NULL)]
  
  
  cohesion[,list(strength=sum(strengthOnesided)),by=list(cluster1 = cluster_live, cluster2 = cluster_work)]
}

determineRegroupList <- function(LWClus, marginals) {
  
  setDT(LWClus)
  setDT(marginals)
  
  cohesion <- determineCohesion(LWClus, marginals)
  cohesion <- cohesion[order(cohesion$cluster1, cohesion$strength),]
  
  cohesion[!duplicated(cohesion$cluster1, fromLast = TRUE), list(cluster1, cluster2)]
}



regroupDissolved <- function(clusterData) {
  
  neg.cluster=clusterData$marginals[cluster<0,cluster]
  
  
  LWClusDissolve=clusterData$LWClus[xor(cluster_live < 0, cluster_work < 0),]
  
  
  LWClusDissolve=LWClusDissolve[cluster_live!=0 & cluster_work!=0]
  
  
  if(nrow( LWClusDissolve)==0){out=1}
  
  if(nrow( LWClusDissolve)>0){
    uno=LWClusDissolve[cluster_live>0,cluster_live]
    due=LWClusDissolve[cluster_work>0,cluster_work]
    involvedclusters=sort(unique(c(uno,due,neg.cluster,0)))
    regroupList <- determineRegroupList(LWClusDissolve, clusterData$marginals[cluster%in%involvedclusters,])
    
    out=list(mergeCluster(clusterData, regroupList$cluster1, regroupList$cluster2),regroupList)
  }
  return(out)
}


regroupDissolved.ncom <- function(clusterData,index.com.2diss) {
  
  
  LWClusDissolve <- clusterData$LWClus[xor(clusterData$LWClus$cluster_live == index.com.2diss, clusterData$LWClus$cluster_work == index.com.2diss), ]
  LWClusDissolve <- LWClusDissolve[LWClusDissolve$cluster_live*LWClusDissolve$cluster_work<0,]
  
  neg.cluster=clusterData$marginals[cluster==index.com.2diss,cluster]
  
  
  indi=1:nrow(LWClusDissolve)
  indi=indi[LWClusDissolve$cluster_live!=0]
  if(length(indi)>0){LWClusDissolve=LWClusDissolve[indi,]}
  
  indi=1:nrow(LWClusDissolve)
  indi=indi[LWClusDissolve$cluster_work!=0]
  if(length(indi)>0){LWClusDissolve=LWClusDissolve[indi,]}
  
  if(nrow( LWClusDissolve)==0){out=1}
  
  if(nrow( LWClusDissolve)>0){
    
    uno=LWClusDissolve[cluster_live>0,cluster_live]
    due=LWClusDissolve[cluster_work>0,cluster_work]
    involvedclusters=sort(unique(c(uno,due,neg.cluster,0)))
    regroupList <- determineRegroupList(LWClusDissolve, clusterData$marginals[cluster%in%involvedclusters,])
    
    out=list(mergeCluster(clusterData, regroupList$cluster1, regroupList$cluster2),regroupList)
  }
  
  return(out)
}



LMAwrite=function(out,path_wd=NULL,suff=NULL){
  s=getwd()
  if(is.null(path_wd)){path_wd=s}
  s=getwd()
  setwd(path_wd)
  if(!is.null(suff)){fname=paste("LMA clusterList ",suff,".csv",sep="")}
  if(is.null(suff)){fname=paste("LMA clusterList ","",".csv",sep="")}
  write.table(out$lma$clusterList,file=fname,sep=";",row.names=F,col.names=T,append=F,quote=F)
  rm(fname)
  if(!is.null(suff)){fname=paste("LMA marginals ",suff,".csv",sep="")}
  if(is.null(suff)){fname=paste("LMA marginals ","",".csv",sep="")}
  write.table(out$lma$marginals,file=fname,sep=";",row.names=F,col.names=T,append=F,quote=F)
  rm(fname)
  if(!is.null(suff)){fname=paste("LMA LWClus ",suff,".csv",sep="")}
  if(is.null(suff)){fname=paste("LMA LWClus ","",".csv",sep="")}
  write.table(out$lma$LWClus,file=fname,sep=";",row.names=F,col.names=T,append=F,quote=F)
  rm(fname)
  
  
  
  
  
  
  if(!is.null(suff)){fname=paste("LMA list ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("LMA list ","",".RData",sep="")}
  lma=out$lma
  save(lma,file=fname)
  rm(lma)
  rm(fname)
  if(!is.null(suff)){fname=paste("LMA list before assignment ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("LMA list before assignment ","",".RData",sep="")}
  lma.before0=out$lma.before0
  save(lma.before0,file=fname)
  rm(lma.before0)
  rm(fname)
  
  if(!is.null(suff)){fname=paste("reserve list ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("reserve list ","",".RData",sep="")}
  reserve.list=out$reserve.list
  save(reserve.list,file=fname)
  rm(reserve.list)
  rm(fname)
  
  if(!is.null(suff)){fname=paste("ComNotAssigned ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("ComNotAssigned ","",".RData",sep="")}
  comNotAssigned=out$comNotAssigned
  save(comNotAssigned,file=fname)
  rm(comNotAssigned)
  rm(fname)
  
  if(!is.null(suff)){fname=paste("zero.codes ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("zero.codes ","",".RData",sep="")}
  zero.list=out$zero.list
  save(zero.list,file=fname)
  rm(zero.list)
  rm(fname)
  
  
  
  if(!is.null(suff)){fname=paste("communitiesMovements ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("communitiesMovements ","",".RData",sep="")}
  communitiesMovements=out$communitiesMovements
  save(communitiesMovements,file=fname)
  rm(communitiesMovements)
  rm(fname)
  
  
  if(!is.null(suff)){fname=paste("param ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("param ","",".RData",sep="")}
  param=out$param
  save(param,file=fname)
  rm(param)
  rm(fname)
  
  
  if(!is.null(suff)){fname=paste("idcom_rel ",suff,".RData",sep="")}
  if(is.null(suff)){fname=paste("idcom_rel ","",".RData",sep="")}
  idcom_rel=out$idcom_rel
  save(idcom_rel,file=fname)
  rm(fname)
  
  
  setwd(s)
  
}




AssignLmaName=function(LWCom,lma,ComNames){
  
  setDT(LWCom)
  setcolorder(LWCom,c("community_live","community_work","amount"))
  
  setDT(lma$clusterList)
  setDT(lma$LWClus)
  setDT(lma$marginals)
  setDT(ComNames)
  setnames(lma$clusterList,c("community","LMA","EMP_live"))
  setnames(lma$LWClus,c("LMA_live","LMA_work","commuters"))
  setnames(lma$marginals,c("LMA","EMP_live","EMP_work"))
  
  
  
  
  LmaNames=copy(ComNames)
  LmaNames[,lma.name:=toupper(com.name)][,com.name:=NULL]
  
  worker=LWCom[,list(worker=sum(amount)),by=list(community=community_work)]
  worker=merge(worker,ComNames,by.x="community",by.y="Code",all.y=T)
  worker$worker[is.na(worker$worker)]=0
  
  lma$clusterList<-merge(lma$clusterList,worker,by="community")
  lma$clusterList=merge(lma$clusterList,LmaNames, by.x="community",by.y="Code")
  
  
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
}


CreateLMAshape=function (lma,
                         comIDs = "community", lmaIDs = "LMA",
                         shp_com = NULL, 
                              dsn = NULL, shp_com_name = NULL, 
                         id_shp_com, outd = NULL, 
                              outf = NULL, bf = NULL,
                         po = c("green", 1, 2, "red", 1, 
                                                             2, 0.8, 2)
                              
                         ) 
{
  
  
  
  
  if (!all(c("com.name", "lma.name") %in% names(lma$clusterList))) {
    stop("ERROR: please check the names of the lma$clusterList object.")
  }
  if (!all(c("lma.name.live", "lma.name.work") %in% names(lma$LWClus))) {
    stop("ERROR: please check the names of the lma$LWClus object.")
  }
  if (!all(c("lma.name") %in% names(lma$marginals))) {
    stop("ERROR: please check the names of the lma$marginals object.")
  }
  if (is.null(shp_com_name) & is.null(shp_com)) {
    stop("ERROR: please provide an input shape object OR path (dsn) and file to be read.")
  }
  if (!is.null(shp_com_name) & is.null(dsn)) {
    stop("ERROR: please provide a valid dsn.")
  }
  if (is.null(shp_com_name) & !is.null(dsn)) {
    stop("ERROR: please provide a valid filename.")
  }
  
  if (is.null(shp_com_name) & !is.null(shp_com)) {
    
    ##commento daniela 22 febb 2023, per eliminare sp e comunque non usiamo proj4string
    
    # if (is.na(shp_com@proj4string@projargs)) {
    #   stop("ERROR: please provide an input file OR a shape file containing a valid proj4string.")
    # }
    # if (!is.na(shp_com@proj4string@projargs)) {
    #   shp = shp_com
    #   proj4string = shp_com@proj4string
    # }
    
    #daniela 22 febb 2023 aggiunta la riga sotto per creare oggetto di tipo sf
    if(any(class(shp_com)=="SpatialPolygonsDataFrame")){shp=st_as_sf(shp_com)}
    if(any(class(shp_com)=="sf")){shp=shp_com}
    if(length(intersect(class(shp_com),c("sf","SpatialPolygonsDataFrame")))==0){
      stop("ERROR: please provide a shape file that is an object of class sf or, only for older versions of the LabourMarketAreas package, a SpatialPolygonsDataFrame. Please consider updates.")
    }
    
    
  }
  
  
  if (!is.null(shp_com_name) & !is.null(dsn)) {
    #daniela 22febb2023 cambiata la lettura del file in sf (da RGDAL)
    shp <- st_read(dsn = dsn, layer = shp_com_name)
  }
  if (!is.null(shp_com_name) & !is.null(shp_com)) {
    print("Warning: Inputs shp_com and shp_com_name (file) are both provided. Only the second is considered.")
  }
  if (comIDs %in% colnames(lma$clusterList) & lmaIDs %in% 
      colnames(lma$clusterList) & lmaIDs %in% colnames(lma$marginals)) {
    shp <- merge(shp, data.frame(lma$clusterList), by.x = id_shp_com, 
                 by.y = comIDs)
    # new_shp <- aggregate(shp, by = list(shp[[as.character(colnames(lma$clusterList)[colnames(lma$clusterList) == 
    #                                                                                                    lmaIDs])]]),FUN=head,n=1)
    butta=ls()
    if(length(grep("new_shp",butta))){rm(new_shp) }
    
    new_shp <- aggregate(shp[,eval(lmaIDs)]
                         , by =st_drop_geometry(shp[,eval(lmaIDs)]),FUN=head,n=1,do_union=T)
    
    new_shp=st_make_valid(new_shp)
    
    #daniela 22febb2023
    #IDs <- sapply(slot(new_shp, "polygons"), function(x) slot(x, "ID"))
    IDs=as.character(1:length(new_shp$LMA))
    
    indi = grep(lmaIDs, names(lma$clusterList))
    df <- data.frame(AREA = 1:length(unique(data.frame(lma$clusterList)[, 
                                                                        indi])), row.names = IDs)
    
    df[, as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList)) == 
                                                              lmaIDs])] <- as.character(unique(data.frame(lma$clusterList)[, 
                                                                                                                           as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList)) == 
                                                                                                                                                                                lmaIDs])]))
    
    df <- df[order(df[, as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList)) == 
                                                                             lmaIDs])]), ]
    row.names(df) <- IDs
    df[, as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList)) == 
                                                              lmaIDs])] <- as.integer(df[, as.character(colnames(data.frame(lma$clusterList))[colnames(data.frame(lma$clusterList)) == 
                                                                                                                                                lmaIDs])])
    
    ##daniela 22febb2023
    #join_com <- sp::SpatialPolygonsDataFrame(new_shp, df)
    join_com=merge(new_shp,df)
    
    #daniela 22febb2023
    # shp_lma <- sp::merge(join_com, data.frame(lma$marginals), 
    #                      by = lmaIDs)
    
    shp_lma=merge(join_com,data.frame(lma$marginals), 
                  by = lmaIDs)
    
    
    #daniela 22febb2023
    shp_lma=shp_lma[,-grep(".1",names(shp_lma))]
    
    #print(class(shp_lma))
    if (!is.null(outf)) {
      st_write(shp_lma, outd, outf, driver = "ESRI Shapefile")
    }
    if (!is.null(bf)) {
      bmp(bf, width = 1200, height = 1200)
      plot(shp_lma, col = po[1], lwd = as.numeric(po[2]), 
           lty = as.numeric(po[3]))
      centroids <- coordinates(join_com)
      text(centroids, label = shp_lma$lma.name, col = po[4], 
           lwd = as.numeric(po[5]), lty = as.numeric(po[6]), 
           cex = as.numeric(po[7]), font = as.numeric(po[8]))
      dev.off()
    }
  }
  if (!(comIDs %in% colnames(lma$clusterList) & lmaIDs %in% 
        colnames(lma$clusterList))) {
    stop("ERROR: please check the column names of the lma object, especially the clusterList component")
  }
  if (!(lmaIDs %in% colnames(lma$marginals))) {
    stop("ERROR: please check the column names of the lma object, especially the marginals component")
  }
  #QUI 27 APRILE 2023 HO USATO IL GET NELLE DIFFERENZE
  comID.in.LMA.not.in.SHP = setdiff(lma$clusterList[, get(comIDs)], 
                                    data.table(shp)[, get(id_shp_com)])
  comID.in.SHP.not.in.LMA = setdiff(data.table(shp)[, get(id_shp_com)], lma$clusterList[, get(comIDs)])
  
  
  
  
  
  return(list(shp_lma = shp_lma, comID.in.LMA.not.in.SHP = comID.in.LMA.not.in.SHP, 
              comID.in.SHP.not.in.LMA = comID.in.SHP.not.in.LMA))
}


AssignSingleComToSingleLma=function(lma,comID,lmaID,dat){
  
  setDT(dat)
  setcolorder(dat,c("community_live","community_work","amount"))
  
  if(!lmaID%in%lma$marginals$cluster){
    stop("ERROR: the lmaID is not in the list of possible LMA.")
  }
  
  if(!comID%in%lma$clusterList$community){
    stop("ERROR: the comID is not in the list of possible communities.")
  }
  
  
  
  clusterList=copy(lma$clusterList[,list(community,cluster,residents)])
  
  clusterList[community==comID,cluster:=lmaID]
  
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
}
EqualLmaPartition=function(lma1,lma2){
  
  out=TRUE
  setDT(lma1$clusterList)
  setDT(lma2$clusterList)
  if((length(setdiff(lma1$community,lma2$community))>0)|(length(setdiff(lma2$community,lma2$community))>0)){
    print("The lists of communities are different.")
    out=FALSE
  }
  else{
    setkey(lma1$clusterList,community,LMA)
    setkey(lma2$clusterList,community,LMA)
    if(!all(lma1$clusterList$cluster==lma2$clusterList$cluster)){
      out=FALSE
      print("The same communities, but different clusters.")
    }
  }
  return(out)
}

CreateClusterData=function(LWCom,residents=NULL){
  setDT(LWCom)
  setcolorder(LWCom,c("community_work","community_live","amount"))
  
  LIST.COM=data.table(c(LWCom[,community_live],LWCom[,community_work]))
  LIST.COM=unique(LIST.COM)[order(V1)]
  
  if(is.null(residents)){
    residents=setcolorder(LWCom[,list(residents=sum(amount)),by=list(Code=community_live)],c("residents","Code"))
    residents=merge(residents,LIST.COM,by.x="Code",by.y="V1",all=T)
    residents[is.na(residents),residents:=0]
  }
  
  
  communitiesList <- unique(c(LWCom[,community_live], LWCom[,community_work]))
  
  clusterList <- 1:length(communitiesList)
  
  result <- data.table(community = communitiesList, cluster = clusterList)
  
  clusterList     <- merge(result, residents, by.x = "community", by.y = "Code")
  
  rm(result)
  
  names(LWCom)[grep("_live",names(LWCom))][1]="community_live"
  names(LWCom)[grep("_work",names(LWCom))][1]="community_work"
  
  LWClus <- merge(LWCom, data.table(clusterList), by.x = "community_live", by.y = "community",all=T)
  LWClus <- merge(LWClus, data.table(clusterList), by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  out<-LWClus[, list(amount=sum(amount)),by=list(cluster_live,cluster_work)]
  out[is.na(amount),amount:=0]
  
  out[,col:=(is.na(cluster_live) & is.na(cluster_work) & amount==0)]
  out=out[!(col)]
  out[,col:=NULL]
  
  LWClus <- out
  rm(out)
  
  
  
  
  liveMarginal <- LWClus[, list(amount=sum(amount)), by = list(cluster=cluster_live)]
  workMarginal <- LWClus[, list(amount=sum(amount)), by = list(cluster=cluster_work)]
  
  marginals <- merge(liveMarginal, workMarginal, by="cluster",suffixes = c("_live", "_work"), all=TRUE)
  
  rm(liveMarginal)
  rm(workMarginal)
  
  
  
  
  clusterData <- list(clusterList = clusterList, LWClus = LWClus, marginals = marginals)
  
  return(clusterData)
  
}






FindIsolated=function(lma,lma_shp=NULL,lma_shp_path=NULL,lma_shp_name=NULL,
                      com_shp=NULL,com_shp_path=NULL,com_shp_name=NULL,
                      id_com){
  
  
  
  
  
  
  if(is.null(lma_shp_name) & is.null(lma_shp)){
    stop("ERROR: please provide an input shape object or path (dsn) and file to be read.")
  }
  
  if(!is.null(lma_shp_name) & is.null(lma_shp_path)){
    stop("ERROR: please provide a valid lma_shp_path.")
  }
  
  if(is.null(lma_shp_name) & !is.null(lma_shp_path)){
    stop("ERROR: please provide a valid filename.")
  }
  
  if(is.null(lma_shp_name) & !is.null(lma_shp)){
    
    # if(is.na(lma_shp@proj4string@projargs)){
    #   stop("ERROR: please provide an input file or a shape file containing a valid proj4string.")
    # }
    # if(!is.na(lma_shp@proj4string@projargs)){
    #   shp=lma_shp
    #   proj4string=lma_shp@proj4string
    # }
    
    #daniela 22 febb 2023 aggiunta la riga sotto per creare oggetto di tipo sf
    if(any(class(lma_shp)=="SpatialPolygonsDataFrame")){shp=sf::st_as_sf(lma_shp)}
    if(any(class(lma_shp)=="sf")){shp=lma_shp}
    if(length(intersect(class(lma_shp),c("sf","SpatialPolygonsDataFrame")))==0){
      stop("ERROR: please provide a shape file that is an object of class sf or, only for older versions of the LabourMarketAreas package, a SpatialPolygonsDataFrame. Please consider updates.")
    }
    
  }
  
  
  if(!is.null(lma_shp_name) & !is.null(lma_shp_path)){
    shp <- sf::st_read(dsn=lma_shp_path, layer=lma_shp_name)
  }
  
  if(!is.null(lma_shp_name) & !is.null(lma_shp)){
    print("Warning: Inputs lma_shp and lma_shp_name are both provided. Only the second is considered.")
  }
  
  
  
  #daniela 22febb2023
  # if(length(grep("lma_nam",names(shp@data)))>0){
  #   names(shp@data)[names(shp@data)=="lma_nam"]="lma.name"}
  # if(length(grep("EMP_liv",names(shp@data)))>0){  
  #   names(shp@data)[names(shp@data)=="EMP_liv"]="EMP_live"}
  # if(length(grep("EMP_wrk",names(shp@data)))>0){    
  #   names(shp@data)[names(shp@data)=="EMP_wrk"]="EMP_work"}
  # if(length(grep("EMP_lv_",names(shp@data)))>0){    
  #   names(shp@data)[names(shp@data)=="EMP_lv_"]="EMP_live_work"}
  
  if(length(grep("lma_nam",names(shp)))>0){
    names(shp)[names(shp)=="lma_nam"]="lma.name"}
  if(length(grep("EMP_liv",names(shp)))>0){  
    names(shp)[names(shp)=="EMP_liv"]="EMP_live"}
  if(length(grep("EMP_wrk",names(shp)))>0){    
    names(shp)[names(shp)=="EMP_wrk"]="EMP_work"}
  if(length(grep("EMP_lv_",names(shp)))>0){    
    names(shp)[names(shp)=="EMP_lv_"]="EMP_live_work"}
  
  
  if(is.null(com_shp_name) & is.null(com_shp)){
    stop("ERROR: please provide an input shape object or path (com_shp_path) and file to be read.")
  }
  
  if(!is.null(com_shp_name) & is.null(com_shp_path)){
    stop("ERROR: please provide a valid com_shp_path.")
  }
  
  if(is.null(com_shp_name) & !is.null(com_shp_path)){
    stop("ERROR: please provide a valid filename.")
  }
  

 if(is.null(com_shp_name) & !is.null(com_shp)){
  #   
  #   
  #   # if(is.na(com_shp@proj4string@projargs)){
  #   #   stop("ERROR: please provide an input file or a shape file containing a valid proj4string.")
  #   # }
  #   # if(!is.na(com_shp@proj4string@projargs)){
  #   #   comuni91=com_shp
  #   #   proj4string=com_shp@proj4string
  #   }
    
  #daniela 22 febb 2023 aggiunta la riga sotto per creare oggetto di tipo sf
  if(any(class(com_shp)=="SpatialPolygonsDataFrame")){comuni91=sf::st_as_sf(com_shp)}
  if(any(class(com_shp)=="sf")){comuni91=com_shp}
  if(length(intersect(class(com_shp),c("sf","SpatialPolygonsDataFrame")))==0){
    stop("ERROR: please provide a shape file that is an object of class sf or, only for older versions of the LabourMarketAreas package, a SpatialPolygonsDataFrame. Please consider updates.")
  }
    
  }
  
  
  if(!is.null(com_shp_name) & !is.null(com_shp_path)){
    comuni91 <- sf::st_read(dsn=com_shp_path, layer=com_shp_name)
  }
  
  if(!is.null(com_shp_name) & !is.null(com_shp)){
    print("Warning: Inputs com_shp and com_shp_name are both provided. Only the second is considered.")
  }
  
  
  
  
 
  #daniela 22febb2023 sf
  # kiki=grep(id_com,names(comuni91@data))
kiki=grep(id_com,names(comuni91))
  
#daniela 22febb2023 sf
  # if(length(setdiff(lma$clusterList$community,comuni91@data[,kiki]))>0){
  #   stop("ERROR: there are communities in the flows data but not in the shape file. Please correct.")
  # }

if(length(setdiff(lma$clusterList$community,as.data.frame(comuni91)[,kiki]))>0){
  stop("ERROR: there are communities in the flows data but not in the shape file. Please correct.")
}
  
#daniela 22febb2023 sf
  # if(length(setdiff(comuni91@data[,kiki],lma$clusterList$community))>0){
  #   stop("ERROR: there are communities in the shape file but not in the flows data. Please correct.")
  # }

if(length(setdiff(as.data.frame(comuni91)[,kiki],lma$clusterList$community))>0){
  stop("ERROR: there are communities in the shape file but not in the flows data. Please correct.")
}
    
  comuni91<-merge(comuni91,data.frame(lma$clusterList),by.x = names(comuni91)[kiki], by.y = "community")
  
  
  #daniela 22febb2023
  # nbpolygsll <- spdep::poly2nb(shp, row.names =shp@data$LMA,queen=TRUE)
  #nbpolygsll <- spdep::poly2nb(shp, row.names =shp$LMA,queen=TRUE)
  nbpolygsll <- st_relate(shp,  pattern = "F***T****")
  as.nb.sgbp <- function(x, ...) {
    attrs <- attributes(x)
    x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
    attributes(x) <- attrs
    class(x) <- "nb"
    x
  }
  nbpolygsll=as.nb.sgbp(nbpolygsll)
  
  Wsll=spdep::nb2mat(nbpolygsll,style="B", zero.policy=TRUE)
  rownames(Wsll[rowSums(Wsll)==0,])
  #daniela 22febb2023 sf
  # colnames(Wsll)=shp@data$LMA
  colnames(Wsll)=shp$LMA
  rownames(Wsll)=shp$LMA
  #daniela 22febb2023 sf
  # nolinksll=shp@data$LMA[shp@data$LMA%in%rownames(Wsll)[rowSums(Wsll)==0]]
  # nolinksllname=shp@data$lma.name[shp@data$LMA%in%rownames(Wsll)[rowSums(Wsll)==0]]
  nolinksll=shp$LMA[shp$LMA%in%rownames(Wsll)[rowSums(Wsll)==0]]
  nolinksllname=shp$lma.name[shp$LMA%in%rownames(Wsll)[rowSums(Wsll)==0]]
  
  
  shp=shp[!shp$LMA%in%nolinksll,]
  
  
  setDT(lma$clusterList)
  lma$clusterList=lma$clusterList[!(lma$clusterList$LMA%in%nolinksll),]
  lma$LWCom=lma$LWCom[!(lma$LWCom$LMA_live%in%nolinksll),]
  lma$LWCom=lma$LWCom[!(lma$LWCom$LMA_work%in%nolinksll),]
  lma$marginals=lma$marginals[!(lma$marginals$LMA%in%nolinksll),]
  
  lma$clusterList[,Ncom:=.N,by=LMA]
  badsll=lma$clusterList[Ncom==1,]
  setorder(badsll,-EMP_live)
  
  badsll=badsll$LMA
  badsllname="0"
  if(length(badsll)>0){
  for(kiki in 1:length(badsll)){
    #daniela 22febb2023 sf
    # badsllname=c(badsllname,
    #              as.character(shp@data$lma.name[shp$LMA==badsll[kiki]]))
    
    badsllname=c(badsllname,
                 as.character(shp$lma.name[shp$LMA==badsll[kiki]]))
  }
  }
  badsllname=badsllname[-1]
  
  
  ##QUI INTERVENIRE: 
  #d=sp::disaggregate(shp)
  d=st_cast(shp,"POLYGON")
  
  # d@data=data.table(d@data)
  # d@data[,x:=.N,by=lma.name]
  # d@data[x>=2,ID_PiecesLMA:=paste(as.character(LMA),1:max(x),sep="_"),by=lma.name]
  # d@data[is.na(ID_PiecesLMA),ID_PiecesLMA:=as.character(LMA)]
  
  d1=data.table(d)
  d1[,x:=.N,by=lma.name]
  d1[x>=2,ID_PiecesLMA:=paste(as.character(LMA),1:max(x),sep="_"),by=lma.name]
  d1[is.na(ID_PiecesLMA),ID_PiecesLMA:=as.character(LMA)]
  d$x=NULL
  d1[,x:=.N,by=lma.name]
  d=merge(d,d1)
  
  #QUI INTERVENIRE
  sll.shp.nb <- spdep::poly2nb(d, row.names =d$ID_PiecesLMA,queen=TRUE)
  
  W=spdep::nb2mat(sll.shp.nb,style="B", zero.policy=TRUE)
  
  
  
  rownames(W[rowSums(W)==0,])
  colnames(W)=d$ID_PiecesLMA
  rownames(W)=d$ID_PiecesLMA
  
  nolinkpolig=d$ID_PiecesLMA[d$ID_PiecesLMA%in%rownames(W)[rowSums(W)==0]]
  nolinkpoligname=d$lma.name[d$ID_PiecesLMA%in%rownames(W)[rowSums(W)==0]]
  
  
  badlmalist=sort(unique(d$LMA[d$x>1]))
  zeris=c(0,0)
  df.mun.poly=data.frame(t(zeris))
  colnames(df.mun.poly)=c("community","Polygon")
  rm(zeris)
  
  
  indice.ident=grep(id_com,names(comuni91))
  for(badlma in badlmalist){
    dev.new(noRStudioGD = TRUE)
    par(mfrow=c(1,2))
    zz=d[d$LMA==badlma,]
    plot(sf::st_geometry(zz),main=paste("polygons",unique(zz$lma.name),sep=" "),col=c("yellow","red","cyan","orange"),col.main="red")
    centroids <- st_centroid(zz)
    text(st_coordinates(centroids), label=zz$ID_PiecesLMA,
         cex = 0.8,col="black")
    
    plot(sf::st_geometry(comuni91[comuni91$LMA==badlma,]),main=paste("communities",unique(zz$lma.name),sep=" "))
    centroids <- st_centroid(comuni91[comuni91$LMA==badlma,])
    text(st_coordinates(centroids), label=unlist(st_drop_geometry(comuni91[comuni91$LMA==badlma,indice.ident])),     
         cex = 0.8)
    
    par(mfrow=c(1,1))
    if((max(zz$x)-1)>0){
    for(kiki in 1:(max(zz$x)-1)){
      mimi=readline(paste("LMA ID ",badlma ," please type com ID:     ",sep=""))
      
      cici=readline(paste("LMA ID ",badlma ," please type polygon label:     ",sep=""))
      df.mun.poly=rbind(df.mun.poly,c(mimi,cici))
    }
      }
    dev.off()
  }
  df.mun.poly=data.table(df.mun.poly[-1,])
  df.mun.poly=df.mun.poly[!is.na(community) & community!=""]
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
  
  df.mun.poly=merge(df.mun.poly[,.(community=as.character(community),Polygon)],lma$clusterList[,list(community=as.character(community),LMA,EMP_live)],by="community")
  setDT(df.mun.poly)
  setorder(df.mun.poly,-EMP_live)
  
  if(!all(df.mun.poly$Polygon%in%rownames(W))){
    repeat{
      print("Please control the following polygons IDs in the association table.")
      print(setdiff(df.mun.poly$Polygon,rownames(W)))
      fix(df.mun.poly)
      if(all(df.mun.poly$Polygon%in%rownames(W))){break}
    }
  }
  
  if(!all(df.mun.poly$community%in%lma$clusterList$community)){
    repeat{
      print("Please control the following communities ID in the association table.")
      print(setdiff(df.mun.poly$community,lma$clusterList$community))
      fix(df.mun.poly)
      if(all(df.mun.poly$community%in%lma$clusterList$community)){break}
    }
  }
  
  
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
  
}


FindContig=function(type="poly",lma,contig.matrix,isolated){
  
  if(!type%in%c("poly","lma")){
    stop("ERROR: type must be poly or lma")
  }
  
  if(type=="poly"){
    lista.contigui.poly=list()
    i=1
    for(sgd in isolated$Polygon){
      if(!is.na(match("temp",ls()))){rm(temp)}
      
      row.index=which(rownames(contig.matrix)==as.character(sgd))
      temp=names(which(contig.matrix[row.index,]==1))
      # OLD VERSION temp=names(which(contig.matrix[as.character(sgd),]==1))
      
      lista.contigui.poly[[i]]=temp
      names(lista.contigui.poly)[i]=sgd
      i=i+1
    }
    
    names(lista.contigui.poly)=isolated$community
    if(length(lista.contigui.poly)>0){
      for(i in 1:length(lista.contigui.poly)){
        if("cici"%in%ls()){rm(cici)}
        cici=grep("_",lista.contigui.poly[[i]])
        tutti=1:length(lista.contigui.poly[[i]])
        diff=setdiff(tutti,cici)
        lista.contigui.poly[[i]]=lista.contigui.poly[[i]][diff]
      }
    }
    numvicini=unlist(lapply(lista.contigui.poly,FUN=length))
    
    com_onlyenclave=names(numvicini)[numvicini==0]
    
    lista.contigui.poly=lista.contigui.poly[numvicini!=0]
    
    out=list(list.contig.poly=lista.contigui.poly
             ,com_no.LMA.neigh=com_onlyenclave)
  }
  
  if(type=="lma"){
    lista.contigui.sll1=list()
    i=1
    for(sgd in isolated){
      if(!is.na(match("temp",ls()))){rm(temp)}
      row.index=which(rownames(contig.matrix)==as.character(sgd))
      temp=names(which(contig.matrix[row.index,]==1))
      if(!is.null(temp)){
        lista.contigui.sll1[[i]]=temp
        names(lista.contigui.sll1)[i]=sgd
        i=i+1
      }
      
    }
    
    cici=merge(data.frame(isolated),data.frame(lma$clusterList),by.x="isolated",
               by.y="LMA")
    setDT(cici)
    setorder(cici,-EMP_live)
    
    if(length(lista.contigui.sll1)>0){names(lista.contigui.sll1)=cici$community}
    
    
    out=list(list.contig.lma=lista.contigui.sll1)
    
  }
  
  return(out)
}


DeleteLmaName=function(lma){
  
  clusterData=copyClusterData(lma)
  
  clusterData$clusterList[,com.name:=NULL]
  clusterData$clusterList[,lma.name:=NULL]
  
  clusterData$marginals[,lma.name:=NULL]
  
  clusterData$LWClus[,lma.name.live:=NULL]
  clusterData$LWClus[,lma.name.work:=NULL]
  
  if(length(grep("LMA",names(clusterData$clusterList)))>0){
    setnames(clusterData$clusterList,"LMA","cluster")
  }
  
  if(length(grep("EMP_live",names(clusterData$clusterList)))>0){
    setnames(clusterData$clusterList,"EMP_live","residents")
  }
  
  
  if(length(grep("LMA_live",names(clusterData$LWClus)))>0){
    setnames(clusterData$LWClus,"LMA_live","cluster_live")
  }
  
  if(length(grep("LMA_work",names(clusterData$LWClus)))>0){
    setnames(clusterData$LWClus,"LMA_work","cluster_work")
  }
  
  if(length(grep("commuters",names(clusterData$LWClus)))>0){
    setnames(clusterData$LWClus,"commuters","amount")
  }
  
  
  if(length(grep("LMA",names(clusterData$marginals)))>0){
    setnames(clusterData$marginals,"LMA","cluster")
  }
  
  if(length(grep("EMP_live",names(clusterData$marginals)))>0){
    setnames(clusterData$marginals,"EMP_live","amount_live")
  }
  
  if(length(grep("EMP_work",names(clusterData$marginals)))>0){
    setnames(clusterData$marginals,"EMP_work","amount_work")
  }
  
  
  return(clusterData)
}


copyClusterData=function(lma){
  out=list()
  out$clusterList=copy(data.table(lma$clusterList))
  out$LWClus=copy(data.table(lma$LWClus))
  out$marginals=copy(data.table(lma$marginals))
  return(out)
}



FineTuning=function(dat,out.ini,list.contiguity){
  setcolorder(dat,c("community_live","community_work","amount"))
  not.tunned.commID=-1
  
  
  
  
  
  if(length(grep("cluster",names(out.ini$clusterList)))>0){
    setnames(out.ini$clusterList,"cluster","LMA")
  }
  
  if(length(grep("residents",names(out.ini$clusterList)))>0){
    setnames(out.ini$clusterList,"residents","EMP_live")
  }
  
  out=copyClusterData(out.ini)
  
  out.orig=out
  dat.orig=dat
  if(length(list.contiguity)>0){
  for(i in 1:length(list.contiguity)){
    out=out.orig
    dat=dat.orig
    
    com.to.assign=names(list.contiguity)[i]
    candidates=as.numeric(list.contiguity[[i]])
    
    out$clusterList=data.table(out$clusterList)
    out$clusterList$LMA[out$clusterList$community==com.to.assign]=-1
    
    out$clusterList=out$clusterList[LMA%in%c(-1,candidates)]
    setnames(out$clusterList,c("community","cluster", "EMP_live"))
    
    
    
    dat.f=dat
    dat.f=merge(dat.f, out$clusterList ,by.x="community_live", by.y="community" )  
    dat.f[,EMP_live:=NULL]
    dim(dat.f)
    setnames(dat.f,c("community_live", "community_work", "amount",  "LMA_live"))
    dat.f=merge(dat.f, out$clusterList ,by.x="community_work", by.y="community" ) 
    dat.f[,EMP_live:=NULL]
    setnames(dat.f,c("community_work","community_live",  "amount",  "LMA_live", "LMA_work"))
    
    
    out$LWClus=dat.f[,list(commuters=sum(amount)),by=list(LMA_live,LMA_work)]
    setnames(out$LWClus,c("cluster_live", "cluster_work", "amount"))
    
    liveMarginal=out$LWClus[,sum(amount),by=list(LMA=cluster_live)]
    workMarginal=out$LWClus[,sum(amount),by=list(LMA=cluster_work)]
    liveMarginal$LMA=factor(liveMarginal$LMA, exclude="")
    workMarginal$LMA=factor(workMarginal$LMA, exclude="")
    out$marginals=merge(liveMarginal,workMarginal, by="LMA", suffixes=c("_live","_work"), all=TRUE)
    out$marginals$LMA=as.numeric(as.vector(out$marginals$LMA))
    out$marginals$LMA[is.na(out$marginals$LMA)]=0
    setnames(out$marginals,c("cluster","amount_live", "amount_work"))
    
    
    
    cosa=regroupDissolved(out)
    
    
    if(is.list(cosa)){
      l.new=cosa[[1]]$clusterList$cluster[cosa[[1]]$clusterList$community==com.to.assign]
    }else{
      l.new=out.ini$clusterList$LMA[out.ini$clusterList$community==com.to.assign]
      not.tunned.commID=c(not.tunned.commID,com.to.assign)
    }
    
    
    
    out.orig$clusterList[community==com.to.assign,LMA:=l.new]
    
    dat=merge(dat.orig, out.orig$clusterList ,by.x="community_live", by.y="community" )  
    dat=dat[, EMP_live:=NULL]
    setnames(dat,c("community_live", "community_work", "amount",  "LMA_live"))
    
    dat=merge(dat, out.orig$clusterList ,by.x="community_work", by.y="community" ) 
    dat=dat[, EMP_live:=NULL]
    setnames(dat,c("community_work","community_live",  "amount",  "LMA_live", "LMA_work"))
    
    out.orig$LWClus=dat[,list(commuters=sum(amount)),by=list(LMA_live,LMA_work)]
    
    
    liveMarginal=out.orig$LWClus[,sum(commuters),by=list(LMA=LMA_live)]
    workMarginal=out.orig$LWClus[,sum(commuters),by=list(LMA=LMA_work)]
    
    liveMarginal$LMA=factor(liveMarginal$LMA, exclude="")
    workMarginal$LMA=factor(workMarginal$LMA, exclude="")
    marginals=merge(liveMarginal,workMarginal, by="LMA", suffixes=c("_live","_work"), all=TRUE)
    marginals$LMA=as.numeric(as.vector(marginals$LMA))
    marginals$LMA[is.na(marginals$LMA)]=0
    setnames(marginals,c("LMA","EMP_live", "EMP_work"))
    out.orig$marginals=marginals
    
  }
  }
  
  setnames(out.orig$clusterList,c("community","cluster","residents"))
  setnames(out.orig$LWClus,c("cluster_live","cluster_work","amount"))
  setnames(out.orig$marginals,c("cluster","amount_live","amount_work"))
  not.tunned.commID=not.tunned.commID[-1]
  return(list(tunned.lma=out.orig,not.tunned.commID=not.tunned.commID))
  
}



StatReserveList=function(reserve.list,dat){
  if(length(reserve.list)>0){
  setDT(dat)
  setcolorder(dat,c("community_live","community_work","amount"))
  ExtractComp=function(vec,n){vec[n]}
  NbCom=length(unique(unlist(lapply(reserve.list, ExtractComp,5))))
  
  NbClus=length(unique(unlist(lapply(reserve.list, ExtractComp,3))))
  SummValid=summary(as.numeric(unlist(lapply(reserve.list, ExtractComp,4))))
  
  TypesTable=table(unlist(lapply(reserve.list, ExtractComp,1)))
  
  cluster=unlist(lapply(reserve.list, ExtractComp,3))
  community=unlist(lapply(reserve.list, ExtractComp,5))
  tre=data.table(cluster,community)
  dt=tre[,list(Ncom=.N),by=cluster]
  setorder(dt,Ncom)
  NbUniqueClus=nrow(dt[Ncom==1])
  SummNComByClust=summary(dt[Ncom>1,Ncom])
  
  
  
  Resid.Com=dat[community_live%in%as.numeric(community),list(Residents=sum(amount)),by=community_live]
  setorder(Resid.Com,Residents)
  SummResidByCom=summary(Resid.Com[,Residents])
  
  Workers.Com=dat[community_work%in%as.numeric(community),list(Workers=sum(amount)),by=community_work]
  setorder(Workers.Com,Workers)
  SummWorkersByCom=summary(Workers.Com[,Workers])
  
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
  #end if length>0
  }
  if(length(reserve.list)==0){ out=list()}
  
  return(out)
  
}


StatClusterData=function(lma,param,threshold,dat){
  
  setDT(dat)
  setcolorder(dat,c("community_live","community_work","amount"))
  
    if(any(names(lma$clusterList)!=c("community","cluster","EMP_live"))){
      print("Warning: names of clusterList were changed into community cluster residents")
      setnames(lma$clusterList,c("community" ,"cluster", "residents"))
    }
    if(any(names(lma$LWClus)!=c("cluster_live","cluster_work","amount"))){
      print("Warning: names of LWClus were changed into cluster_live cluster_work amount")
      setnames(lma$LWClus,c("cluster_live" ,"cluster_work" ,"amount"))
    }
    if(any(names(lma$marginals)!=c("cluster","amount_live","amount_work"))){
      print("Warning: names of marginals were changed into cluster_live amount_live amount_work")
      setnames(lma$marginals,c("cluster" ,"amount_live" ,"amount_work"))
    }
  
  
  
  
  minSZ=param[1]
  minSC=param[2]
  tarSZ=param[3]
  tarSC=param[4]
  
  
  clusterList=copy(data.table(lma$clusterList[,1:3,with=F]))
  LWClus=copy(data.table(lma$LWClus[,1:3,with=F]))
  marginals=copy(data.table(lma$marginals[,1:3,with=F]))
  
  
  vali=getLeastSelfContained(LWClus, marginals,minSZ,minSC,tarSZ,tarSC)
  if(all(lma$clusterList$cluster>0)){
    vali[[2]]=vali[[2]][cluster>0,]
  }
  marginals=merge(marginals,vali[[2]][,list(cluster,validity)],by="cluster",all=T)
  
  amount_live_work<-LWClus[cluster_live==cluster_work,list(cluster_live,amount)]
  marginals=merge(marginals,amount_live_work,by.x="cluster",by.y="cluster_live",all=T)
  setnames(marginals,"amount","EMP_live_work")
  setnames(marginals,"amount_live","EMP_live")
  setnames(marginals,"amount_work","EMP_work")
  
  marginals[, lma_commuter_percent:=((EMP_live-EMP_live_work)+(EMP_work-EMP_live_work))/(2*EMP_live_work)]
  marginals[, Home_Work_Ratio:=(( EMP_live-EMP_live_work)-( EMP_work-EMP_live_work))/EMP_live_work]
 
  marginals[,SC_demand_side:=EMP_live_work/EMP_work]
  marginals[,SC_supply_side:=EMP_live_work/EMP_live]
  ncom=clusterList[,list(N_com=.N),by=cluster]
  marginals=merge(marginals,ncom,by="cluster",all=T)
  
  uno=merge(clusterList,dat,by.x="community",by.y="community_live")
  if(length(grep("EMP_live",names(uno)))>0){uno[,EMP_live:=NULL]}
  setnames(uno,"cluster","cluster_live")
  setnames(uno,"community","community_live")
  
  due=merge(uno,clusterList,by.x="community_work",by.y="community")
  if(length(grep("EMP_live",names(uno)))>0){due[,EMP_live:=NULL]}
  
  setnames(due,"cluster","cluster_work")
  
  tre=due[cluster_live==cluster_work & community_live!=community_work,.N,by=cluster_live]
  marginals=merge(marginals,tre,by.x="cluster",by.y="cluster_live",all=T)
  marginals[,InternalCohesionLink:=N/(N_com*(N_com-1))]
  
  
  quattro=due[cluster_live==cluster_work & community_live!=community_work,list(ic=sum(amount)),by=cluster_live]
  cinque=due[cluster_live==cluster_work ,list(icc=sum(amount)),by=cluster_live]
  cinque=merge(cinque,quattro,by="cluster_live")
  cinque[,InternalCohesionFlows:=ic/icc*100]
  marginals=merge(marginals,cinque,by.x="cluster",by.y="cluster_live",all=T)
  
  marginals[,N:=NULL]
  marginals[,ic:=NULL]
  marginals[,icc:=NULL]
  
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
  
  
  StatFlows=list()
  StatFlows$N_links=nrow(LWClus)
  StatFlows$PercNbLinksLessThreshold=nrow(LWClus[amount<threshold])/nrow(LWClus)*100
  
  StatFlows$summFlows=summary(LWClus$amount)
  StatFlows$summFlowsNoItself=LWClus[cluster_live!=cluster_work,summary(amount)]
  
  nlinks_in=LWClus[,list(N_links_in=.N),by=cluster_work]
  marginals=merge(marginals,nlinks_in,by.x="cluster",by.y="cluster_work",all=T)
  nlinks_out=LWClus[,list(N_links_out=.N),by=cluster_live]
  marginals=merge(marginals,nlinks_out,by.x="cluster",by.y="cluster_live",all=T)
  
  StatFlows$summLinks_in=nlinks_in[,summary(N_links_in)]
  StatFlows$summLinks_out=nlinks_out[,summary(N_links_out)]
  
  
  StatFlows$clusterMaxNlinks_in=nlinks_in[N_links_in==max(N_links_in),cluster_work]
  StatFlows$clusterMaxNlinks_out=nlinks_out[N_links_out==max(N_links_out),cluster_live]
  
  StatFlows$clusterMinNlinks_in=nlinks_in[N_links_in==min(N_links_in),cluster_work]
  StatFlows$clusterMinNlinks_out=nlinks_out[N_links_out==min(N_links_out),cluster_live]
  
  
  StatQuality=list()
  
  StatQuality$NbClusters=nrow(marginals)
  StatQuality$NbClusterUniqueCom=nrow(marginals[N_com==1])
  StatQuality$NbClustersValidLess1=nrow(marginals[validity<1])
  StatQuality$NbClustersNoCentralCom=nrow(marginals[NbCentralComm==0])
  
  StatQuality$Mean.SC_demand_side=mean(marginals$SC_demand_side,na.rm=T)
  StatQuality$Std.SC_demand_side=sd(marginals$SC_demand_side,na.rm=T)
  
  StatQuality$Mean.SC_supply_side=mean(marginals$SC_supply_side,na.rm=T)
  StatQuality$Std.SC_supply_side=sd(marginals$SC_supply_side,na.rm=T)
  
  StatQuality$Q1.InternalCohesionFlows=quantile(marginals$InternalCohesionFlows,probs=0.25,na.rm=T)
  StatQuality$Q2.InternalCohesionFlows=median(marginals$InternalCohesionFlows,na.rm=T)
  StatQuality$Q3.InternalCohesionFlows=quantile(marginals$InternalCohesionFlows,probs=0.75,na.rm=T)
  
  StatQuality$Q1.InternalCohesionLink=quantile(marginals$InternalCohesionLink,probs=0.25,na.rm=T)
  StatQuality$Q2.InternalCohesionLink=median(marginals$InternalCohesionLink,na.rm=T)
  StatQuality$Q3.InternalCohesionLink=quantile(marginals$InternalCohesionLink,probs=0.75,na.rm=T)
  
  StatQuality$Q1.EMP_live=quantile(marginals$EMP_live,probs=0.25,na.rm=T)
  StatQuality$Q2.EMP_live=median(marginals$EMP_live,na.rm=T)
  StatQuality$Q3.EMP_live=quantile(marginals$EMP_live,probs=0.75,na.rm=T)
  
  StatQuality$Mean.EMP_live=mean(marginals$EMP_live,na.rm=T)
  StatQuality$Std.EMP_live=sd(marginals$EMP_live,na.rm=T)
  StatQuality$Min.EMP_live=min(marginals$EMP_live,na.rm=T)
  StatQuality$Max.EMP_live=max(marginals$EMP_live,na.rm=T)
  
  StatQuality$Q1.EMP_work=quantile(marginals$EMP_work,probs=0.25,na.rm=T)
  StatQuality$Q2.EMP_work=median(marginals$EMP_work,na.rm=T)
  StatQuality$Q3.EMP_work=quantile(marginals$EMP_work,probs=0.75,na.rm=T)
  
  StatQuality$Mean.EMP_work=mean(marginals$EMP_work,na.rm=T)
  StatQuality$Std.EMP_work=sd(marginals$EMP_work,na.rm=T)
  StatQuality$Min.EMP_work=min(marginals$EMP_work,na.rm=T)
  StatQuality$Max.EMP_work=max(marginals$EMP_work,na.rm=T)
  
  StatQuality$Q1.EMP_live_work=quantile(marginals$EMP_live_work,probs=0.25,na.rm=T)
  StatQuality$Q2.EMP_live_work=median(marginals$EMP_live_work,na.rm=T)
  StatQuality$Q3.EMP_live_work=quantile(marginals$EMP_live_work,probs=0.75,na.rm=T)
  StatQuality$Min.EMP_live_work=min(marginals$EMP_live_work,na.rm=T)
  StatQuality$Max.EMP_live_work=max(marginals$EMP_live_work,na.rm=T)
  
  
  StatQuality$Mean.lma_commuter_percent=mean(marginals$lma_commuter_percent,na.rm=T)
  StatQuality$Std.lma_commuter_percent=sd(marginals$lma_commuter_percent,na.rm=T)
  
  StatQuality$Mean.Home_Work_Ratio=mean(marginals$Home_Work_Ratio,na.rm=T)
  StatQuality$Std.Home_Work_Ratio=sd(marginals$Home_Work_Ratio,na.rm=T)
  
  
  StatQuality$Q_modularity=Qmodularity(lma)
  
  
  setnames(marginals,"cluster","LMA")
  
  out=list(marginals=marginals,StatFlows=StatFlows,
           StatQuality=StatQuality,
           param=c(minSZ,minSC,tarSZ,tarSC))
  
  
  return(out)
}


BindPiecesLma=function(input1,input2,LWCom){
  setDT(LWCom)
  setcolorder(LWCom,c("community_live","community_work","amount"))
  
  
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
  
  listcom=data.table(community=unique(c(LWCom[,community_live],LWCom[,community_work])))
  fin$clusterList=merge(fin$clusterList,listcom,by.x="community",by.y="community",all.y=T)
  
  nnew=nrow(fin$clusterList[is.na(LMA)])
  fin$clusterList[is.na(LMA),LMA:=maxlma:(maxlma+nnew-1)]
  rm(maxlma);rm(nnew)
  
  cici=LWCom[,list(EMP_live=sum(amount)),by=list(community_live)]
  fin$clusterList=merge(fin$clusterList,cici,
                        by.x="community",by.y="community_live")
  rm(cici)
  
  fin$LWClus <- merge(LWCom, fin$clusterList, by.x = "community_live", by.y = "community",all=T)
  fin$LWClus <- merge(fin$LWClus, data.table(fin$clusterList), by.x = "community_work", by.y = "community", suffixes = c("_live", "_work"),all=T)
  
  fin$LWClus<-fin$LWClus[, list(commuters=sum(amount)),by=list(LMA_live,LMA_work)]
  fin$LWClus[is.na(commuters),commuters:=0]
  
  fin$LWClus[!((is.na(LMA_live) & is.na(LMA_work) & commuters==0))]
  
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
  
}

PlotLmaCommunity=function(list.lma,lmaIDs,communityID,
                          shp_com,
                          id_shp,
                          bmpfile,col.vec=c("red","orange","yellow")){
  
  if(!is.null(shp_com)){
    # if(is.na(shp_com@proj4string@projargs)){
    #   stop("ERROR: please provide a shape file containing a valid proj4string.")
    # }
    
    #daniela 22 febb 2023 aggiunta la riga sotto per creare oggetto di tipo sf
    if(any(class(shp_com)=="SpatialPolygonsDataFrame")){shp_com=sf::st_as_sf(shp_com)}
    if(any(class(shp_com)=="sf")){shp_com=shp_com}
    if(length(intersect(class(shp_com),c("sf","SpatialPolygonsDataFrame")))==0){
      stop("ERROR: please provide a shape file that is an object of class sf or, only for older versions of the LabourMarketAreas package, a SpatialPolygonsDataFrame. Please consider updates.")
    }
    
  }
  
  bmp(paste(bmpfile,".bmp",sep=""),width=1200,height=1200)
  par(mfrow=c(1,2))
  
  counter=1
  lma=list.lma[[counter]]
  
  
  
  x=CreateLMAshape(lma,comIDs="community"
                   ,lmaIDs=lmaIDs
                   ,shp_com=shp_com
                   ,dsn=NULL
                   ,shp_com_name=NULL
                   ,id_shp_com=id_shp
                   ,outd=NULL
                   ,outf=NULL
                   ,bf=NULL
                   ,po=c("green",1,2,"red",1,2,0.8,2))
  
  
  
  
  indice=grep(lmaIDs,names(lma$clusterList))
  current.lma=lma$clusterList[community%in%communityID,indice,with=F]
  com.in.current.lma=lma$clusterList[unlist(lma$clusterList[,indice,with=F])%in%unlist(current.lma),community]
  
  current.lma2=list.lma[[2]]$clusterList[community%in%communityID,indice,with=F]
  com.in.current.lma2=list.lma[[2]]$clusterList[unlist(list.lma[[2]]$clusterList[,indice,with=F])%in%unlist(current.lma2),community]
  
  new.com=setdiff(com.in.current.lma,com.in.current.lma2)
  
  #daniela 22febb2023 sf
  # indicecom=grep(id_shp,names(shp_com@data))
  indicecom=grep(id_shp,names(shp_com))
  indicelma=grep(lmaIDs,names(x$shp_lma))
  
  
  #daniela 22febb2023 sf
  # plot(shp_com[shp_com@data[,indicecom]%in%com.in.current.lma,],border="gray", main=counter)
  # plot(x$shp_lma[x$shp_lma@data[,indicelma]%in%current.lma,],border="red",add=T)
  # plot(shp_com[shp_com@data[,indicecom]%in%communityID,],border="gray",col=col.vec[1],add=T)
  
  ##check se si fa la selezione in questo modo
  
  plot(st_geometry(shp_com[unlist(st_drop_geometry(shp_com[,indicecom]))%in%com.in.current.lma,]),border="gray", main=counter)
  plot(st_geometry(x$shp_lma[unlist(st_drop_geometry(x$shp_lma)[,indicelma])%in%current.lma,]),border="red",add=T)
  plot(st_geometry(shp_com[unlist(st_drop_geometry(shp_com[,indicecom]))%in%communityID,]),border="gray",col=col.vec[1],add=T)
  
  if(length(new.com)>0){
    #daniela 22febb2023
    # plot(shp_com[shp_com@data[,indicecom]%in%new.com,],border="gray",col=col.vec[2],add=T)
    #check se si fa la selezione in questo modo
    plot(st_geometry(shp_com[unlist(st_drop_geometry(shp_com[,indicecom]))%in%new.com,]),border="gray",col=col.vec[2],add=T)
  }
  
  
  
  counter=2
  lma=list.lma[[counter]]
  
 
  
  x=CreateLMAshape(lma,comIDs="community"
                   ,lmaIDs=lmaIDs
                   ,shp_com=shp_com
                   ,dsn=NULL
                   ,shp_com_name=NULL
                   ,id_shp_com=id_shp
                   ,outd=NULL
                   ,outf=NULL
                   ,bf=NULL
                   ,po=c("green",1,2,"red",1,2,0.8,2))
  
  
  indice=grep(lmaIDs,names(lma$clusterList))
  current.lma=lma$clusterList[community%in%communityID,indice,with=F]
  com.in.current.lma=lma$clusterList[unlist(lma$clusterList[,indice,with=F])%in%unlist(current.lma),community]
  
  current.lma1=list.lma[[1]]$clusterList[community%in%communityID,indice,with=F]
  com.in.current.lma1=list.lma[[1]]$clusterList[unlist(list.lma[[1]]$clusterList[,indice,with=F])%in%unlist(current.lma1),community]
  
  new.com=setdiff(com.in.current.lma,com.in.current.lma1)
  
  #daniela 22febb2023
  # indicecom=grep(id_shp,names(shp_com@data))
  indicecom=grep(id_shp,names(shp_com))
  indicelma=grep(lmaIDs,names(x$shp_lma))
  
  
  #daniela 22febb2023 sf
  # plot(shp_com[shp_com@data[,indicecom]%in%com.in.current.lma,],border="gray", main=counter)
  # plot(x$shp_lma[x$shp_lma@data[,indicelma]%in%current.lma,],border="red",add=T)
  # plot(shp_com[shp_com@data[,indicecom]%in%communityID,],border="gray",col=col.vec[1],add=T)
  plot(st_geometry(shp_com[unlist(st_drop_geometry(shp_com[,indicecom]))%in%com.in.current.lma,]),border="gray", main=counter)
  plot(st_geometry(x$shp_lma[unlist(st_drop_geometry(x$shp_lma)[,indicelma])%in%current.lma,]),border="red",add=T)
  plot(st_geometry(shp_com[unlist(st_drop_geometry(shp_com[,indicecom]))%in%communityID,]),border="gray",col=col.vec[1],add=T)
  
  if(length(new.com)>0){
    #daniela 22febb2023
    # plot(shp_com[shp_com@data[,indicecom]%in%new.com,],border="gray",col=col.vec[3],add=T)
    #check se si fa la selezione in questo modo
    plot(st_geometry(shp_com[unlist(st_drop_geometry(shp_com[,indicecom]))%in%new.com,]),border="gray",col=col.vec[2],add=T)
    
    }
  
  
  dev.off()
  
  
  
  criteria=FALSE
  if(!identical(list.lma[[counter]]$clusterList[community%in%communityID,],
                list.lma[[counter-1]]$clusterList[community%in%communityID,])){
    criteria=TRUE
  }
  
  
  
}


Qmodularity<-function(lma)
{
  clusterList=copy(data.table(lma$clusterList[,1:3,with=F]))
  LWClus=copy(data.table(lma$LWClus[,1:3,with=F]))
  marginals=copy(data.table(lma$marginals[,1:3,with=F]))
  
  
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
}

CompareLMAsStat=function(list.lma,dat){
  setcolorder(dat,c("community_live","community_work","amount"))
  if(length(list.lma)>0){
  for(i in 1:length(list.lma)){
    if(any(names(list.lma[[i]]$lma$clusterList)!=c("community","cluster","EMP_live"))){
      print(i)
      print("Warning: names of clusterList were changed into community cluster residents")
      setnames(list.lma[[i]]$lma$clusterList,c("community" ,"cluster", "residents"))
    }
    
    if(any(names(list.lma[[i]]$lma$LWClus)!=c("cluster_live","cluster_work","amount"))){
      print(i)
      print("Warning: names of LWClus were changed into cluster_live cluster_work amount")
      setnames(list.lma[[i]]$lma$LWClus,c("cluster_live" ,"cluster_work" ,"amount"))
    }
    if(any(names(list.lma[[i]]$lma$marginals)!=c("cluster","amount_live","amount_work"))){
      print(i)
      print("Warning: names of marginals were changed into cluster_live amount_live amount_work")
      setnames(list.lma[[i]]$lma$marginals,c("cluster" ,"amount_live" ,"amount_work"))
    }
  }
  }
  
  stat.mat=matrix(0,1,42)
  if(length(list.lma)>0){
  for(i in 1:length(list.lma)){
    m=StatClusterData(list.lma[[i]]$lma,list.lma[[i]]$param,
                      threshold = 1000,dat=dat)$StatQuality
    m=c(list.lma[[i]]$param,unlist(m))
    names(m)[1:4]=c("minSZ","minSC","tarSZ","tarSC")
    stat.mat=rbind(stat.mat,m)
    
  }
  }
  stat.mat=stat.mat[-1,]
  colnames(stat.mat)=names(m)
  rownames(stat.mat)=1:length(list.lma)
  return(stat.mat)
}

AddStatistics=function(statData,comID.file,lma,comID.lma){
  if(any(class(statData)=="sf")){statData=st_drop_geometry(statData)}
  setDT(statData)
  Y=copy(lma$clusterList)
  Y[,residents:=NULL]
  x=merge(Y,statData,by.x=comID.lma,by.y=comID.file)
  nomi=setdiff(names(statData[1]),c(comID.file,"cluster"))
  x=x[,lapply(.SD, sum),by=cluster,.SDcol=nomi]
  
  return(x)
}

LmaSpatialComparison=function(shape,shape_ref,lma.id="LMA"){


  #daniela 22 febb 2023 aggiunta la riga sotto per creare oggetto di tipo sf
  if(any(class(shape)=="SpatialPolygonsDataFrame")){shape=sf::st_as_sf(shape)}
  if(any(class(shape)=="sf")){shape=shape}
  if(length(intersect(class(shape),c("sf","SpatialPolygonsDataFrame")))==0){
    stop("ERROR: please provide a shape file that is an object of class sf or, only for older versions of the LabourMarketAreas package, a SpatialPolygonsDataFrame. Please consider updates.")
  }

  #daniela 22 febb 2023 aggiunta la riga sotto per creare oggetto di tipo sf
  if(any(class(shape_ref)=="SpatialPolygonsDataFrame")){shape_ref=sf::st_as_sf(shape_ref)}
  if(any(class(shape_ref)=="sf")){shape_ref=shape_ref}
  if(length(intersect(class(shape_ref),c("sf","SpatialPolygonsDataFrame")))==0){
    stop("ERROR: please provide a shape file that is an object of class sf or, only for older versions of the LabourMarketAreas package, a SpatialPolygonsDataFrame. Please consider updates.")
  }

  shape=st_make_valid(shape)
  shape_ref=st_make_valid(shape_ref)

FindMaxIntersection=function(single_shp,many_shp,lma.id=lma.id){
  selected_output=NULL
  y=NULL
  xx=NULL

  ##QUI INTERVENIRE
  xx=try(sf::st_intersection(single_shp,many_shp ))
  if(any(class(xx)=="sf")){
  if(!is.null(xx)){
    #daniela 22febb2023
    #y=gArea(x,byid=T)
    y=sf::st_area(xx)
    selected_output=st_drop_geometry(xx)[y==max(y),]
    selected_output=st_drop_geometry(selected_output)[,names(selected_output)==paste(lma.id,".1",sep="")]
    #end if is null xx
  }

  if(is.null(selected_output) | is.null(y)){
    return(list(selected_output=-1,area_intersection_max=-1,perc_intersection_shapi=-1,perc_intersection_lma=-1))
    }
  if(!is.null(selected_output) & !is.null(y)){
    #daniela 22febb2023 sf
    #QUI INTERVENIRE GAREA
    # return(list(selected_output=selected_output,area_intersection_max=max(y),perc_intersection_shapi=max(y)/sum(y),perc_intersection_lma=max(y)/gArea(many_shp[many_shp@data$LMA==selected_output,])))
    return(list(selected_output=selected_output,area_intersection_max=as.numeric(max(y)),perc_intersection_shapi=as.numeric(max(y))/as.numeric(sum(y))*100,perc_intersection_lma=as.numeric(max(y))/as.numeric(sf::st_area(many_shp[st_drop_geometry(many_shp)[,names(many_shp)==lma.id]==selected_output,]))*100))
  }

    #end class xx polygon
  }

  if(inherits(xx,"try-error")){
    return(list(selected_output=-1,area_intersection_max=-1,perc_intersection_shapi=-1,perc_intersection_lma=-1))
  }
  #end function
}

  rm(shapi);rm(shapi_good)

  #daniela 22febb2023 use sf
  #shapi=spTransform(shape,proj4string(shape_ref))
  shapi=sf::st_transform(shape,sf::st_crs(shape_ref))
  shapi_good=shapi

  #daniela 22febb2023
  # shapi_good@data=data.table(shapi_good@data)
  #area=gArea(shapi_good,byid=T)

  area=sf::st_area(shapi_good)
  shapi_good$area_shapi=area

  ###QUI INTERVENIRE GAREA
  #shape_ref=data.table(shape_ref@data)
  #area=gArea(shape_ref,byid=T)
  area=sf::st_area(shape_ref)
  shape_ref$area_lma_com=area


  stats=c(0,11)

  #daniela 22febb2023 sf
  #indice=1:nrow(shapi_good@data)
  indice=1:nrow(shapi_good)
  for(i in indice){
    #print(i)

    #daniela 22febb2023 sf
    # nome_shapi=shapi_good@data$LMA[i]
    # corrisp=FindMaxIntersection(shapi_good[i,],shape_ref)
    #
    # stats=rbind(stats,c(nome_shapi
    #                     ,as.character(shape_ref@data$LMA[shape_ref@data$LMA==corrisp$selected_output])
    #                     ,corrisp$area_intersection
    #                     ,as.numeric(as.character(shapi_good@data$area[i]))
    #                     ,as.numeric(as.character(shape_ref@data$area_lma_com[shape_ref@data$LMA==corrisp$selected_output]))
    #                     ,as.numeric(as.character(shapi_good@data$EMP_live[i]))
    #                     ,as.numeric(as.character(shapi_good@data$EMP_work[i]))
    #                     ,as.numeric(as.character(shape_ref@data$EMP_live[shape_ref@data$LMA==corrisp$selected_output]))
    #                     ,as.numeric(as.character(shape_ref@data$EMP_work[shape_ref@data$LMA==corrisp$selected_output]))
    #                     ,corrisp$perc_intersection_shapi
    #                     ,corrisp$perc_intersection_lma)
    # )

    nome_shapi=st_drop_geometry(shapi_good)[i,names(shapi_good)==lma.id]
    corrisp=FindMaxIntersection(shapi_good[i,],shape_ref,lma.id=lma.id)

    stats=rbind(stats,c(nome_shapi
                        ,as.character(shape_ref$LMA[st_drop_geometry(shape_ref)[,names(shape_ref)==lma.id]==corrisp$selected_output])
                        ,corrisp$area_intersection
                        ,as.numeric(shapi_good$area[i])
                        ,as.numeric(shape_ref$area_lma_com[st_drop_geometry(shape_ref)[,names(shape_ref)==lma.id]==corrisp$selected_output])
                        ,as.numeric(shapi_good$EMP_live[i])
                        ,as.numeric(shapi_good$EMP_work[i])
                        ,as.numeric( shape_ref$EMP_live[st_drop_geometry(shape_ref)[,names(shape_ref)==lma.id]==corrisp$selected_output])
                        ,as.numeric(shape_ref$EMP_work[st_drop_geometry(shape_ref)[,names(shape_ref)==lma.id]==corrisp$selected_output])
                        ,corrisp$perc_intersection_shapi
                        ,corrisp$perc_intersection_lma)
    )

  }

  stats=stats[-1,]
  stats=data.table(stats)
  #stats[,shape_ref_ID:=NULL]
  if(nrow(stats)>0){  
  setnames(stats,c("shape_lma","shape_ref_lma","area_intersection"
                   ,"shape_area","shape_ref_area",
                   "shape_EMP_live","shape_EMP_work"
                   ,"shape_ref_EMP_live","shape_ref_EMP_work"
                   ,"perc_intersection_shape","perc_intersection_shape_ref"))
  }


    stats[,area_intersection:=as.numeric(as.character(area_intersection))]
    stats[,shape_area:=as.numeric(as.character(shape_area))]
    stats[,shape_ref_area:=as.numeric(as.character(shape_ref_area))]
    stats[,shape_EMP_live:=as.numeric(as.character(shape_EMP_live))]
    stats[,shape_EMP_work:=as.numeric(as.character(shape_EMP_work))]
    stats[,shape_ref_EMP_live:=as.numeric(as.character(shape_ref_EMP_live))]
    stats[,shape_ref_EMP_work:=as.numeric(as.character(shape_ref_EMP_work))]
    stats[,perc_intersection_shape:=as.numeric(as.character(perc_intersection_shape))]
    stats[,perc_intersection_shape_ref:=as.numeric(as.character(perc_intersection_shape_ref))]

    return(stats)
  #end function LmaSpatialComparison
}
