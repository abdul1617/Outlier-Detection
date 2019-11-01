# Create dataset
Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/Musk_version_1.csv')
X = Y[1:166]
#X = Y[1:2]
#X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score_KDEOS <- KDEOS(dataset = X,k_min=50,k_max=50,eps=NULL)

# Sort and find index for most outlying observations
names(outlier_score_KDEOS) <- 1:nrow(X)
sort(outlier_score_KDEOS, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score_KDEOS)

truth_value <- Y[,167]
#truth_value <- ifelse(truth_value==0,1,0)
truth_value <- factor(truth_value)
#for parkinson
#outlier_score <- ifelse(outlier_score>1.1,0,1)
#for all datasets
outlier_score_KDEOS <- ifelse(outlier_score_KDEOS>0.8, 1, 0)
outlier_score_KDEOS <- factor(outlier_score_KDEOS)
predicted_values_KDEOS <- outlier_score_KDEOS
#pred.obj <- predictionFunction(predicted_values, truth_value)
pred.obj <- prediction(predicted_values_KDEOS, truth_value)
auc.tmp <- performance(pred.obj,"auc")
auc <- as.numeric(auc.tmp@y.values)
#Auc <- c(Auc, auc)
result_KDEOS <- confusionMatrix(outlier_score_KDEOS, truth_value)
precision_KDEOS <- result_KDEOS$byClass['Pos Pred Value']
recall_KDEOS <- result_KDEOS$byClass['Sensitivity']
f_measure_KDEOS <- 2 * ((precision_KDEOS * recall_KDEOS) / (precision_KDEOS + recall_KDEOS))
#specificity <- specificity(outlier_score, truth_value)
#specificity <- specificity(confusionMatrix)
#fpr <- 1-specificity
auc <- result_KDEOS$byClass['Balanced Accuracy']

print("...Running RDos_gaussian...")
print(result_KDEOS)
print(precision_KDEOS)
print(recall_KDEOS)
print(f_measure_KDEOS)
#print(specificity)
print(auc)
#View(outlier_score_KDEOS)
#write.csv(outlier_score_KDEOS,"C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/KDEOS_data5.csv")