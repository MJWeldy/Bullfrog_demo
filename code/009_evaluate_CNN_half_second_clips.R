library(caret)
library(RColorBrewer)

colnames(train$Y) <- c("Background", "Bullfrog")
# Grep species, set colors for heatmap
speciesClass <- gsub(colnames(train$Y), pat = "species", rep = "")
cols <- colorRampPalette(rev(brewer.pal(n = 2, name = "RdGy")))

#Using completely withheld test data
predProb <- predict(model, test$X)
predClass <- speciesClass[apply(predProb, 1, which.max)]
trueClass <- speciesClass[apply(test$Y, 1, which.max)]

# Accuracy in validation set
mean(predClass == trueClass) 
confMat <- confusionMatrix(data = factor(predClass, levels = speciesClass),
                           reference = factor(trueClass, levels = speciesClass),
                           positive = "Bullfrog", mode = "prec_recall")
# Accuracy in validation set
mean(predClass == trueClass) 
