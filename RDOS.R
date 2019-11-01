Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/ionosphere.csv')
X = Y[,c(1:32)]
RDOS <- function(dataset, k=16, h=0.1){
  
  n <- nrow(dataset)
  d <- ncol(dataset)
  dataset <- as.matrix(dataset)
  
  if(!is.numeric(k)){
    stop('k input must be numeric')
  }
  if(k>=n||k<1){
    stop('k input must be less than number of observations and greater than 0')
  }
  if(!is.numeric(h)){
    stop('h input must be numeric')
  }
  if(!is.numeric(dataset)){
    stop('dataset input is not numeric')
  }
  
  distMatrix <- as.matrix(dist(dataset))
  dist.obj <- dbscan::kNN(dataset, k)
  
  #sNN matrix
  func.dist <- function(x1, x2) {
    length(intersect(x1, x2))
  }
  
  sNN_matrix <- as.matrix(proxy::dist(x = dist.obj$id, method = func.dist, diag = T, upper = T))
  
  neighborhood <- list()
  
  #neighborhood loop
  for(i in 1:n){
    
    kNN <- dist.obj$id[i,]
    rNN <- as.numeric(which(dist.obj$id==i, arr.ind = TRUE)[,1])
    sNN <- as.numeric(names(sNN_matrix[i,][sNN_matrix[i,]>0]))
    
    neighborhood[[i]] <- union(kNN, c(rNN, sNN))
    
  }
  
  px <- NULL
  
  #gaussian kernel loop
  for(i in 1:n){
    
    Kgaussian <- 1/((2*pi)^(d/2))*exp(-((distMatrix[i, neighborhood[[i]]])/(2*h^2)))
    
    px[i] <- (1/(length(neighborhood[[i]])+1))*sum((1/(h^d))*Kgaussian)
    
  }
  
  RDOS <- NULL
  
  #RDOS
  for(i in 1:n){
    
    RDOS[i] <- (sum(px[neighborhood[[i]]]))/(length(neighborhood[[i]])*px[i])
    
  }
  
  return(RDOS)
  
}

#get data frame from numeric
getDataFrame <- function(v)
{
  res <- NULL
  for(i in v)
  {
    res <- rbind(res, data.frame(i))
  }
  return(res)
}

#Function to convert numeric to Factor
getFactor <- function(v)
{
  res <- factor(v)
  return(res)
}

#Function to print AUC vs h graph
plotAUC_h <- function(X, Y, k)
{
  t <- 0;
  X_h <- NULL;
  for(i in 1:10)
  {
    t <- t + 0.2;
    X_h <- c(X_h, t)
  }
  
  true_values <- Y[,c(33)]
  actual_values <- ifelse(true_values==0,0,1)
  
  Auc <- NULL
  for(h in X_h)
  {
    outlier_score <- RDOS_gaussian(dataset=X, k, h)
    names(outlier_score) <- 1:nrow(X)
    outlier_value <- ifelse(outlier_score>1.0, 1, 0)
    
    predicted_values <- outlier_value
    pred.obj <- prediction(predicted_values, actual_values)
    auc.tmp <- performance(pred.obj,"auc") 
    auc <- as.numeric(auc.tmp@y.values)
    Auc <- c(Auc, auc)
  }
  
  
  X_h <- getDataFrame(X_h)
  
  #df dataframe will contain two columns: col1 - h_values, col2 - auc_values  
  df <- cbind(X_h, Auc)
  colnames(df) <- c("h", "AUC")
  #View(df)
  
  
  p <- ggplot(df, aes(h)) + geom_line(aes(y = AUC)) + 
    ylab(label = "AUC") + xlab(label = "h") +ggtitle("AUC vs h")  
  print(p)
  #savePlot(p)
}

#Function to plot points
plotPoints <- function(df, outlier_val, k)
{
  df <- cbind(df, outlier_val)
  colnames(df) <- c("x", "y", "outlier")
  
  p <- ggplot(df, aes(x, y)) + scale_shape_identity() + 
    geom_point(aes(color = factor(outlier)), position = "jitter", size = 2) +
    ggtitle("Data 8") + scale_color_discrete(name="outlier")
  
  p <- p + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
  #print(p)
  return(p)
}

#Function to save graph in plots folfer
savePlot <- function(myPlot) 
{
  #enter name of plot in here
  #plot will be saved under the name dummy_1 in plots folder
  jpeg("plots/dummy_1.jpeg")
  print(myPlot)
  dev.off()
}

getPerformanceMetrics <- function(o_v, t_v)
{
  predicted_values = getFactor(o_v)
  actual_values = getFactor(t_v)
  
  result <- confusionMatrix(predicted_values, actual_values)
  #print(result)
  precision <- result$byClass['Neg Pred Value']    
  recall <- result$byClass['Specificity']
  fmeasure <- 2 * ((precision * recall)/(precision + recall))
  accuracy <- result$byClass['Accuracy']
  #auc <- result$byClass['Balanced Accuracy']
  
  pred.obj <- prediction(o_v, t_v)
  auc.tmp <- performance(pred.obj,"auc"); 
  auc <- as.numeric(auc.tmp@y.values)
  
  print("Precision")
  print(precision)
  print("Recall")
  print(recall)
  print("Fmeasure")
  print(fmeasure)
  print("AUC")
  print(auc)
}

#..................................
#....Running the functions Part....
#..................................

#..Get value of k from Natural Neighbor..

k <- NaN_k(dataset = X)
print(k)

plotAUC_h(X, Y, k)

outlier_score <- RDOS_gaussian(dataset=X, k, h=0.1)
#outlier_score <- RDOS_laplace(dataset=X, k, h=1.2)
names(outlier_score) <- 1:nrow(X)

#Threshold for outlier score is user defined from observation from dataset
outlier_score1 <- ifelse(outlier_score>1.1, 1, 0)


#.........................................
#........Performance metrics..............
#.........................................

#third row of Y is the labeled column showing weather object is an outlier or not
true_values <- Y[,c(33)]
true_values <- ifelse(true_values==0,0,1)
getPerformanceMetrics(outlier_score1, true_values)


#.........................................
#.............Plot points.................
#.........................................

#plot points based on outlier 
#0 means point is a normal point and 1 means point is an outlier
outlier_val <- getDataFrame(outlier_score1)
p <- plotPoints(X, outlier_val, k)
print(p)
#savePlot(p)