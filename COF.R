# Create dataset
Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/Musk_version_1.csv')
X = Y[1:166]
#X = Y[1:2]
#X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score_COF <- COF(dataset=X, k=100)

# Sort and find index for most outlying observations
names(outlier_score_COF) <- 1:nrow(X)
sort(outlier_score_COF, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score_COF)

truth_value <- Y[,167]
#truth_value <- ifelse(truth_value==0,1,0)
truth_value <- factor(truth_value)
#for parkinson
#outlier_score <- ifelse(outlier_score>1.1,0,1)
#for all datasets
outlier_score_COF <- ifelse(outlier_score_COF>1.04, 1, 0)
outlier_score_COF <- factor(outlier_score_COF)
predicted_values_COF <- outlier_score_COF
#pred.obj <- predictionFunction(predicted_values, truth_value)
pred.obj <- prediction(predicted_values_COF, truth_value)
auc.tmp <- performance(pred.obj,"auc")
auc <- as.numeric(auc.tmp@y.values)
#Auc <- c(Auc, auc)
result_COF <- confusionMatrix(outlier_score_COF, truth_value)
precision_COF <- result_COF$byClass['Pos Pred Value']
recall_COF <- result_COF$byClass['Sensitivity']
f_measure_COF <- 2 * ((precision_COF * recall_COF) / (precision_COF + recall_COF))
#specificity <- specificity(outlier_score, truth_value)
#specificity <- specificity(confusionMatrix)
#fpr <- 1-specificity
auc <- result_COF$byClass['Balanced Accuracy']

print("...Running RDos_gaussian...")
print(result_COF)
print(precision_COF)
print(recall_COF)
print(f_measure_COF)
#print(specificity)
print(auc)
#View(outlier_score_COF)
#write.csv(outlier_score,"C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/cof_data6.csv")