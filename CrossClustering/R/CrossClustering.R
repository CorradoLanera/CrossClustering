CrossClustering<-function(d,k.w.min,k.w.max,k.c.max,out=TRUE)
{
  require(cluster)
  n <- 1+sqrt(1+8*length(d))/2
  beta.clu.ward<- hclust(d, method="ward")
  beta.clu.complete<-hclust(d, method="complete")
  grid<-as.matrix(expand.grid(k.w.min:k.w.max,k.w.min:k.c.max))
  if (out==T) 
    grid<-grid[grid[,2]>grid[,1],] 
  else grid<-grid[grid[,2]>=grid[,1],]
  grid<-cbind(grid,0)
  colnames(grid)<-c("Ward","Complete","N. classified")
  n.clu <- NULL
  for(i in 1:dim(grid)[1]){
    n.clu[i] <- max.proportion.function(grid[i,],  beta.clu.ward=beta.clu.ward,
                                        beta.clu.complete=beta.clu.complete)
  }
  grid[,3] <- n.clu
  grid.star<-which(grid== max(grid[,3]), arr.ind = TRUE)[,1] 
  k.star<-rbind(grid[grid.star,1:2]) 
  if(is.null(dim(k.star))){
    cluster.list <- max.proportion.function(k.star, beta.clu.ward=beta.clu.ward, 
                                            beta.clu.complete=beta.clu.complete, return.list=T)
    clustz <- sapply(1:n, geneinlista, cluster.list$beta.list)
  }else{
    cluster.list <- apply(k.star, 1, max.proportion.function, beta.clu.ward=beta.clu.ward, 
                          beta.clu.complete=beta.clu.complete, return.list=T)
    clustz <- sapply(cluster.list, function(lasim) sapply(1:n, geneinlista, lista=lasim$beta.list)) 
    
  }
  clustz[clustz=="integer(0)"]=0
  if(is.null(dim(clustz))){
    clustz <- matrix(clustz, ncol=1)
  }
  Sil <- list()
  for (c in 1:ncol(clustz)){
    Sil[c] <- mean(silhouette(as.numeric(clustz[,c]), dist=d)[,3])
  }
  
  if(is.null(dim(k.star))){
    k.star.star <- k.star[which.max(Sil)]
  }else{
    k.star.star <- k.star[which.max(Sil),]
  }
  return(list("Optimal.cluster"=length(cluster.list[[which.max(Sil)]]$beta.list), 
              "Cluster.list"=cluster.list[[which.max(Sil)]]$beta.list, 
 #             "A.star"=cluster.list[[which.max(Sil)]]$A.star,
              "Silhouette"=max(unlist(Sil))))
}
