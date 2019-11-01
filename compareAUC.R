#................................
#....Data Preprocessing Part ....
#................................

#Choose Either a real dataset or a synthetic dataset

#Select a synthetic data set from Synthetic Datasets folder 
Y = read.csv('Synthetic Datasets/data1.csv')


#Select a real data set from Real Datasets folder. 
#Remove # from 3 lines below to select a real dataset

#data <- readMat('Real Datasets/mnist.mat')
#data = lapply(data, unlist, use.names=FALSE)
#Y <- as.data.frame(data)


#Choose the number of columns(features) from the Y
X = Y[,c(1:2)] 

#...........................
#......Functions Part ......
#...........................


#RDos_gaussian_function

RDOS <- function(dataset, k=5, h=1)
{
  n <- nrow(dataset)
  d <- ncol(dataset)
  dataset <- as.matrix(dataset)
  
  if(!is.numeric(k))
  {
    stop('k input must be numeric')
  }
  if(k>=n||k<1)
  {
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(h))
  {
    stop('h input must be numeric')
  }
  if(!is.numeric(dataset))
  {
    stop('dataset input is not numeric')
  }
  
  distMatrix <- as.matrix(dist(dataset))
  dist.obj <- dbscan::kNN(dataset, k)
  
  #sNN matrix
  func.dist <- function(x1, x2) 
  {
    length(intersect(x1, x2))
  }
  
  sNN_matrix <- as.matrix(proxy::dist(x = dist.obj$id, method = func.dist, diag = T, upper = T))
  
  neighborhood <- list()
  
  #neighborhood loop
  for(i in 1:n)
  {
    kNN <- dist.obj$id[i,]
    rNN <- as.numeric(which(dist.obj$id==i, arr.ind = TRUE)[,1])
    sNN <- as.numeric(names(sNN_matrix[i,][sNN_matrix[i,]>0]))
    neighborhood[[i]] <- union(kNN, c(rNN, sNN))
  }
  
  px <- NULL
  
  #gaussian kernel loop
  for(i in 1:n)
  {
    Kgaussian <- 1/((2*pi)^(d/2))*exp(-((distMatrix[i, neighborhood[[i]]])/(h^2)))
    px[i] <- (1/(length(neighborhood[[i]])+1))*sum((1/(h^d))*Kgaussian)
    
  }
  
  #RDOS <- NULL
  RDOS <- vector()
  
  #RDOS
  for(i in 1:n)
  {
    RDOS[i] <- (sum(px[neighborhood[[i]]]))/(length(neighborhood[[i]])*px[i])
  }
  return(RDOS)
}



### KNN FUNCTION
KNN_SUM <- function(dataset, k=5){
  
  dataset <- as.matrix(dataset)
  n <- nrow(dataset)
  
  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }
  
  dist.obj <- dbscan::kNN(dataset, k)
  
  knnSum <- NULL
  knnSum <- apply(dist.obj$dist, 1, sum)
  
  return(knnSum)
} #requires dbscan

### INFLO FUNCTION
INFLO <- function(dataset, k=5){
  
  n <- nrow(dataset)
  dataset <- as.matrix(dataset)
  
  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }
  
  dist.obj <- dbscan::kNN(dataset, k)
  
  obsDensity <- apply(dist.obj$dist, 1, function(x){1/max(x)})
  
  RNN <- matrix(data=NA, nrow=nrow(dataset), ncol = 1)
  avgDensityInfluSpace <- matrix(data=NA, nrow=nrow(dataset), ncol = 1)
  INFLO <- NULL
  
  for(i in 1:n){
    
    influSpace <- as.vector(which(dist.obj$id==i, arr.ind = TRUE)[,1])
    
    if(length(influSpace)==0){
      RNN[i] <- k
      influSpace <- dist.obj$id[i,]
    } else {
      RNN[i] <- length(influSpace)
    }
    
    sumRNNobsDensity <- NULL
    
    for(j in 1:length(influSpace)){
      RNNobsDensity <- obsDensity[influSpace[j]]
      sumRNNobsDensity[j] <- RNNobsDensity
      
    }
    
    avgDensityInfluSpace[i] <- sum(sumRNNobsDensity)/RNN[i]
    INFLO[i] <- avgDensityInfluSpace[i]/obsDensity[i]
  }
  
  return(INFLO)
} #requires dbscan


### LOF FUNCTION
LOF <- function(dataset, k=5)
{
  n <- nrow(dataset)
  dataset <- as.matrix(dataset)
  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }
  
  dist.obj <- dbscan::kNN(dataset, k)
  lrd <- NULL
  LOF <- NULL
  
  for(i in 1:n)
  {
    k_dist <- dist.obj$dist[dist.obj$id[i,], k]
    dist_po <- dist.obj$dist[i,]
    reach_dist <- apply(cbind(k_dist, dist_po), 1, max)
    lrd[i] <- 1/(sum(reach_dist)/k)
  }
  
  for(i in 1:n)
  {
    LOF[i] <- (sum(lrd[dist.obj$id[i,]]/k))/lrd[i]
  }
  
  return(LOF)
  
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


#..................................
#....Running the functions Part....
#..................................


true_values <- Y[,c(3)]
true_values <- ifelse(true_values==0,0,1)

#k values taken from 1 to 100
t <- 0;
X_k <- NULL;
for(i in 1:100)
{
  t <- t + 1;
  X_k <- c(X_k, t)
}

#lof will contain auc value for LOF function
Lof <- NULL;
Knn <- NULL;
Inflo <- NULL;
for(k in X_k)
{
  outlier_score1 <- LOF(dataset=X, k)
  outlier_score2 <- KNN_SUM(dataset = X, k)
  outlier_score3 <- INFLO(dataset = X, k)
 
  names(outlier_score1) <- 1:nrow(X)
  names(outlier_score2) <- 1:nrow(X)
  names(outlier_score3) <- 1:nrow(X)

  #threshold for all three functions is user defined according to the observations  
  outlier_value1 <- ifelse(outlier_score1>1.5, 1, 0)
  outlier_value2 <- ifelse(outlier_score2>4, 1, 0)
  outlier_value3 <- ifelse(outlier_score3>1.2, 1, 0)
  
  #save auc values of lof in Lof 
  pred.obj1 <- prediction(outlier_value1, true_values)
  auc.tmp1 <- performance(pred.obj1,"auc"); 
  auc1 <- as.numeric(auc.tmp1@y.values)
  Lof <- rbind(Lof, data.frame(auc1))
  
  #save auc values of inflo in Inflo 
  pred.obj2 <- prediction(outlier_value2, true_values)
  auc.tmp2 <- performance(pred.obj2,"auc"); 
  auc2 <- as.numeric(auc.tmp2@y.values)
  Knn <- rbind(Knn, data.frame(auc2))
  
  #save auc values of KNN in Knn 
  pred.obj3 <- prediction(outlier_value3, true_values)
  auc.tmp3 <- performance(pred.obj3,"auc"); 
  auc3 <- as.numeric(auc.tmp3@y.values)
  Inflo <- rbind(Inflo, data.frame(auc3))
  
}

X_k <- getDataFrame(X_k)

#df dataframe will contain four columns: column 1 - k_values, 
#column 2 - auc for lof, column 3 - auc for KNN, column 4 - auc for INFLO
df <- cbind(X_k, Lof)
df <- cbind(df, Knn)
df <- cbind(df, Inflo)
colnames(df) <- c("k", "LOF", "KNN", "INFLO")
#View(df)


#....................................
#...........Plot Graphs.............. 
#....................................

#plot AUC vs k graph for LOF
p1 <- ggplot(df, aes(k)) + geom_line(aes(y = LOF)) + 
      ylab(label = "AUC") + xlab(label = "k") +ggtitle("LOF")
print(p1)
#savePlot(p1)


#plot AUC vs k graph for KNN
p2 <- ggplot(df, aes(k)) + geom_line(aes(y = KNN)) + 
      ylab(label = "AUC") + xlab(label = "k") + ggtitle("KNN")
print(p2)
#savePlot(p1)


#plot AUC vs k graph for INFLO
p3 <- ggplot(df, aes(k)) + geom_line(aes(y = INFLO)) + 
      ylab(label = "AUC") + xlab(label = "k") +ggtitle("INFLO")
print(p3)
#savePlot(p3)



