library(caret)
library(ranger)

## Custom detailed ranger summary function -------------------------------------

mySummary <- function(data, lev = NULL, model = NULL) {
  # Accuracy
  acc <- mean(data$pred == data$obs)
  
  # ROC AUC (force positive class = lev[2], higher probs = more likely positive)
  roc_obj <- pROC::roc(
    response  = data$obs,
    predictor = data[, lev[2]],   # prob for positive class
    levels    = rev(lev),
    direction = "<"               # avoid "controls > cases" message
  )
  auc <- as.numeric(pROC::auc(roc_obj))
  
  # Sensitivity & Specificity
  sens <- caret::sensitivity(data$pred, data$obs, lev[1])
  spec <- caret::specificity(data$pred, data$obs, lev[2])
  
  # Kappa
  cm <- caret::confusionMatrix(data$pred, data$obs, positive = lev[2])
  kappa <- cm$overall["Kappa"]
  
  # Distance to (1,1)
  dist <- sqrt((1 - sens)^2 + (1 - spec)^2)
  
  c(
    ROC      = auc,
    Sens     = sens,
    Spec     = spec,
    Accuracy = acc,
    Kappa    = unname(kappa),
    Distance = dist
  )
}



## Confusion matrix dataframe summary function ---------------------------------

conf.as.df <- function(data) {

data.frame(acc = data$overall["Accuracy"][[1]],
           kappa = data$overall["Kappa"][[1]],
           acc.nir = data$overall["AccuracyPValue"][[1]],
           sens = data$byClass["Sensitivity"][[1]],
           spec = data$byClass["Specificity"][[1]],
           bal.acc = data$byClass["Balanced Accuracy"][[1]])
}
