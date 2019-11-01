Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/data2.csv')
X = Y[1:2]
#X = Y[1:15154]
NAN <- function(dataset=X, NaN_Edges=FALSE){
  
  dataset <- as.matrix(dataset)
  
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }
  
  n <- nrow(dataset)
  r <- 1
  nn <- ceiling(sqrt(n))
  dist.obj <- dbscan::kNN(dataset, nn)
  
  while(TRUE){
    
    if(r>nn){
      nn <- r + 10
      dist.obj <- dbscan::kNN(dataset, nn)
    }
    
    NaN_Num <- tabulate(dist.obj$id[,1:r])
    NaN_Num_0 <- length(NaN_Num[NaN_Num==0])
    
    if(r==1){
      Nan_Num_0_Upd <- NaN_Num_0
    }
    
    if(r>1 & Nan_Num_0_Upd==NaN_Num_0){
      break
    }
    Nan_Num_0_Upd <- length(NaN_Num[NaN_Num==0])
    
    r=r+1
    
    print(paste('r update:', r))
    
  }
  
  if(NaN_Edges==TRUE){
    
    NNpairs <- cbind(rep(1:nrow(dataset),each=r), as.vector(t(dist.obj$id[,1:r])))
    nLinks <- nrow(NNpairs)
    func.pairs <- function(x, y){x %in% y}
    
    pairsMatrix <-
      sapply(1:nLinks, function(i)
        sapply(1:nLinks, function(j) sum(func.pairs(NNpairs[i,], NNpairs[j,]))))
    
    diag(pairsMatrix) <- 0
    n_NaN <- sum(pairsMatrix[upper.tri(pairsMatrix)]==2)
    
    NaN_Edges <- NNpairs[which(pairsMatrix==2, arr.ind = TRUE)[,2],]
    
  } else {
    n_NaN <- NULL
    NaN_Edges <- NULL
  }
  
  return_list <- list(NaN_Num=max(NaN_Num), r=r, NaN_Edges=NaN_Edges, n_NaN=n_NaN, options(max.print=1000000))
  #print(return_list)
  return(return_list)
  #print(max(NaN_Num))
  return(max(NaN_Num))
  
} #requires dbscan
