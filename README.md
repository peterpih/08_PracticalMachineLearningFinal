# PracticalMachineLearning
Peer Review project for predmachlearn-030 at Coursera

```{R}
install.packages("cart")
library(caret)

a_train
a_test

pca_train_obj <- preProcess(a_train[, -1], method=c('center', 'scale', 'pca'), thresh=0.8)

pca_train_pred <- predict(pca_train_obj, a_train[, -1])

pca_test_pred <- predict(pca_train_obj, a_test[, -1])

# compute the model with pca predictors

pca_model <- train(a_train$diagnosis ~ ., data=pca_train_pred, method="glm")

# apply the PCA model on the testing set

pca_result <- confusionMatrix(a_test[, 1], predict(pca_model, pca_test_pred))
pca_result
```
