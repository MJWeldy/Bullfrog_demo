library(caret)
library(RColorBrewer)


# Grep species, set colors for heatmap
speciesClass <- gsub(colnames(train$Y), pat = "species", rep = "")
cols <- colorRampPalette(rev(brewer.pal(n = 2, name = "RdGy")))

#Using completely withheld test data
predProb <- predict(model, test$X)
predClass <- speciesClass[apply(predProb, 1, which.max)]
trueClass <- speciesClass[apply(test$Y, 1, which.max)]

# Plot confusion matrix
confMat <- confusionMatrix(data = factor(predClass, levels = speciesClass),
                reference = factor(trueClass, levels = speciesClass),
                positive = "id1", mode = "prec_recall")
# Accuracy in validation set
mean(predClass == trueClass) 
