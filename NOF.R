# Create dataset
Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/Ovarian.csv')
X = Y[1:15154]
#X = Y[1:2]
#X <- iris[,1:4]
NOF <- function(dataset=X){
  
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
    
    nb_0 <- tabulate(dist.obj$id[,1:r])
    numb <- length(nb_0[nb_0==0])
    
    if(r==1){
      numb_upd <- numb
    }
    
    if(r!=1 & numb_upd==numb){
      break
    }
    numb_upd <- length(nb_0[nb_0==0])
    
    r=r+1
    
    print(paste('r is now:', r))
    
  }
  
  max_nb <- max(nb_0)
  
  if(max_nb>nn){
    dist.obj <- dbscan::kNN(dataset, max_nb)
  }
  
  rNN <- sapply(1:n, function(i){as.vector(which(dist.obj$id[,1:max_nb]==i, arr.ind = TRUE)[,1])})
  NIS <- list()
  
  for(i in 1:n){
    NIS[[i]] <- union(rNN[[i]], dist.obj$id[i,])
  }
  
  lrd <- NULL
  for(i in 1:n){
    
    k_dist <- as.vector(dist.obj$dist[NIS[[i]],max_nb])
    dist_po <- as.vector(apply(rbind(dataset[NIS[[i]],]), 1, function(x){sqrt(sum((x-dataset[i,])^2))}))
    
    reach_dist <- apply(cbind(k_dist, dist_po), 1, max)
    
    lrd[i] <- 1/(sum(reach_dist)/max_nb)
    
  }
  
  NOF <- NULL
  
  for(i in 1:n)
  {
    NOF[i] <- sum(lrd[NIS[[i]]])/(length(NIS[[i]])*lrd[i])
  }
  
  #return_list <- list(nb=nb_0, max_nb=max_nb, r=r, NOF=NOF)
  return(NOF)
  
}
getDataFrame <- function(v)
{
  res <- NULL
  for(i in v)
  {
    res <- rbind(res, data.frame(i))
  }
  
  return(res)
}

savePlot <- function(myPlot) 
{
  #enter name of plot in here
  #plot will be saved under the name dummy_1
  jpeg("plots/dummy_1.jpeg")
  print(myPlot)
  dev.off()
}

  outlier_score1 <- NOF(dataset=X)

  
  names(outlier_score1) <- 1:nrow(X)
  true_values <- Y[,15155]
  #threshold for all three functions is user defined according to the observations  
  #for parkinson
  #outlier_value1 <- ifelse(outlier_score1>1.0,0,1)
  #for all datasets
  outlier_value1 <- ifelse(outlier_score1>1.0, 1, 0)
  
  #save auc values of lof in Lof 
  pred.obj1 <- prediction(outlier_value1, true_values)
  auc.tmp1 <- performance(pred.obj1,"auc"); 
  auc1 <- as.numeric(auc.tmp1@y.values)
  NOF<- rbind(NOF, data.frame(auc1))
  result <- confusionMatrix(outlier_value1,true_values)
  # precision <- result$byClass['Pos Pred Value']    
  # recall <- result$byClass['Sensitivity']
  # f_measure <- 2 * ((precision * recall) / (precision + recall))
  # #specificity <- specificity(outlier_score, truth_value)
  # #specificity <- specificity(confusionMatrix)
  # fpr <- 1-specificity
  # auc <- result$byClass['Balanced Accuracy']
  # 
  # print("...Running RDos_gaussian...")
  # print(result)
  # print(precision)
  # print(recall)
  # print(f_measure)
  # print(specificity)
  print(auc1)
  View(outlier_value1)
  #write.csv(outlier_value1,"C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/nof_data2.csv")
