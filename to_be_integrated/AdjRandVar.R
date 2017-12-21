AdjRandVar<-function(Partition1,Partition2){

  ##################################################
  # Computation of Rand, Adjusted Rand and variance
  # of ARI between two partitions 
  ###################################################
  # Partition1 and Partition2 are two partitions
  # coded as Partition1<-c(1,1,1,2,2,3) for 6 items
  # and 3 classes
  ###################################################
  # Values:
  # Rand: Rand Index
  # ExpectedRand: expected value of Rand Index
  # AdjustedRand: Adjusted Rand Index
  # varARI: variance of Rand Index
  # NARI: NARI
  # p.value: probablity of the test on NARI
  ###################################################
  
  
  
  nitem<-length(Partition1)

  Table<-table(Partition1,Partition2)
  nt<-rowSums(Table)
  pt<-colSums(Table)

  t<-nitem*(nitem-1)/2
  P<-sum(nt^2)-nitem
  Q<-sum(pt^2)-nitem

  Pprime<-sum(nt*(nt-1)*(nt-2))
  Qprime<-sum(pt*(pt-1)*(pt-2))
  Q-sum(pt^2)-nitem
  
  a<-sum(nt *(nt-1)/2)
  b<-sum(pt *(pt-1)/2)

  n<-sum( Table*(Table-1)/2)
  
  varB<-(1/t)+(4*Pprime*Qprime/(nitem*(nitem-1)*(nitem-2)*P*Q))+((P-2-4*(Pprime/P))*(Q-2-4*(Qprime/Q))/(nitem*(nitem-1)*(nitem-2)*(nitem-3)))-(P*Q/(nitem^2*(nitem-1)^2))
  varRI<-4*P*Q*varB/(nitem^2*(nitem-1)^2)
                                                                                                                                               
  expR<-1-((P+Q)/(2*t))+((P*Q)/(2*t^2))
  
  varARI<-varRI*(1/((1-expR)^2))  
  T<-sum(Table^2)-nitem
  
  R<-(T-(P/2)-(Q/2)+t)/t
  
  #expectedrandindex<-a*b/t
  #RandAdjusted<- (n-expectedrandindex)/(((a+b)/2)- expectedrandindex) 
  
  RandAdjusted<- (R-expR)/(1-expR)
  
  NARI=RandAdjusted/sqrt(varARI)
  p.valNARI=1-pnorm(q=NARI)
  
  res<-list(Rand=R, ExpectedRand=expR, AdjustedRand=RandAdjusted,varARI=varARI,NARI=NARI,p.value=p.valNARI)
  
  return(res)
  }
