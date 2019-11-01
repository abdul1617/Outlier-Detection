# Create dataset
Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/data6.csv')
#X = Y[1:15154]
X = Y[1:2]
#X <- iris[,1:4]

# Find outliers by setting an optional k
outlier_score_INFLO <- INFLO(dataset = X, k=10)

# Sort and find index for most outlying observations
names(outlier_score_INFLO) <- 1:nrow(X)
sort(outlier_score_INFLO, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score_INFLO)

truth_value <- Y[,3]
#truth_value <- ifelse(truth_value==0,1,0)
truth_value <- factor(truth_value)
#for parkinson
#outlier_score <- ifelse(outlier_score>1.1,0,1)
#for all datasets
outlier_score_INFLO <- ifelse(outlier_score_INFLO>1.01, 1, 0)
outlier_score_INFLO <- factor(outlier_score_INFLO)
predicted_values_INFLO <- outlier_score_INFLO
#pred.obj <- predictionFunction(predicted_values, truth_value)
pred.obj <- prediction(predicted_values_INFLO, truth_value)
auc.tmp <- performance(pred.obj,"auc")
auc <- as.numeric(auc.tmp@y.values)
#Auc <- c(Auc, auc)
result_INFLO <- confusionMatrix(outlier_score_INFLO, truth_value)
precision_INFLO <- result_INFLO$byClass['Pos Pred Value']
recall_INFLO <- result_INFLO$byClass['Sensitivity']
f_measure_INFLO <- 2 * ((precision_INFLO * recall_INFLO) / (precision_INFLO + recall_INFLO))
#specificity <- specificity(outlier_score, truth_value)
#specificity <- specificity(confusionMatrix)
#fpr <- 1-specificity
auc <- result_INFLO$byClass['Balanced Accuracy']

print("...Running RDos_gaussian...")
print(result_INFLO)
print(precision_INFLO)
print(recall_INFLO)
print(f_measure_INFLO)
#print(specificity)
print(auc)
#View(outlier_score_INFLO)
#write.csv(outlier_score,"C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/INFLO_data6.csv")