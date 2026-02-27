## Homework 7 Question 10.3

library(ROCR)
library(boot)
library(ggplot2)

## loading the data
credit_data <- read.table(
  "germancredit.txt", 
  header = FALSE
)

#View(credit_data)
#str(credit_data)

## 1 = Good,  2 = Bad FROM UCI
## Subtracting 1 gives: 0 = good, 1 = bad

credit_data$response <- credit_data$V21 - 1  
table(credit_data$response)

## 700 = Good, 300 = Bad

## turning categorical columns to factors
categorical_columns <- c("V1","V3","V4","V6","V7","V9","V10","V12","V14","V15","V17","V19","V20")
for (column in categorical_columns) {
  credit_data[[column]] <- as.factor(credit_data[[column]])
}

#str(credit_data)
#levels(credit_data$V1)

#### LOGISTIC REGRESSION MODEL:
logistic_regression_model <- glm(response ~ . - V21,
                  data = credit_data,
                  family = binomial(link = "logit"),
                  control = glm.control(trace = TRUE))
summary(logistic_regression_model)

# removing non-significant predictors using AIC
reduced_AIC_model <- step(logistic_regression_model, 
                          direction = "backward", 
                          trace = 1)
summary(reduced_AIC_model)

## quality of Fit
AIC(reduced_AIC_model)
reduced_AIC_model$null.deviance
reduced_AIC_model$deviance
coef(reduced_AIC_model) 

## PSEUDO R2
pseudo_r2 <- 1 - (reduced_AIC_model$deviance / reduced_AIC_model$null.deviance)
pseudo_r2

# SEARCHING FOR A GOOD THRESHOLD
probabilities <- predict(reduced_AIC_model, type = "response")
thresholds <- seq(0.01, 0.99, by = 0.01)
costs <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  t <- thresholds[i]
  predicted_label <- ifelse(probabilities > t, 1, 0)
  
  # FN - false neg
  FN <- sum(predicted_label == 0 & credit_data$response == 1)
  
  # FP - false pos
  FP <- sum(predicted_label == 1 & credit_data$response == 0)
  
  costs[i] <- 5 * FN + 1 * FP
}

best_index <- which.min(costs)
best_threshold <- thresholds[best_index]
#best_threshold
#costs[best_index]

## plotting
plot(thresholds, costs, type = "l", xlab = "Threshold", ylab = "Total Cost",
     main = "Total Misclassification Cost vs. Threshold")
abline(v = best_threshold, lty = 2)
text(best_threshold + 0.05, max(costs) * 0.9, 
     paste("Best value =", best_threshold))

## confusion matrix
predicted_class <- ifelse(probabilities > best_threshold, 1, 0)
cm <- table(Actual = credit_data$response, Predicted = predicted_class)
print(cm)

accuracy <- sum(diag(cm)) / sum(cm)
sensitivity <- cm[2,2] / sum(cm[2,]) ## correctly detecting bad credit
specificity <- cm[1,1] / sum(cm[1,]) ## correctly detecting good credit

#accuracy
#sensitivity
#specificity

## plotting cm
cm_df <- as.data.frame(cm)
ggplot(cm_df, aes(x = Predicted, y = Actual)) +
  geom_tile(fill = "white", color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  labs(title = "Confusion Matrix",
       x = "Predicted", y = "Actual") +
  theme_minimal()

##### TRYING CV
cost_func <- function(actual, predicted) {
  thresholds <- seq(0.01, 0.99, by = 0.01)
  costs <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    pred_class <- ifelse(predicted > t, 1, 0)
    FN <- sum(pred_class == 0 & actual == 1)
    FP <- sum(pred_class == 1 & actual == 0)
    costs[i] <- 5 * FN + 1 * FP
  }
  
  best_t <- thresholds[which.min(costs)]
  
  pred_class <- ifelse(predicted > best_t, 1, 0)
  FN <- sum(pred_class == 0 & actual == 1)
  FP <- sum(pred_class == 1 & actual == 0)
  return((5 * FN + 1 * FP) / length(actual))
}
cv_error <- cv.glm(credit_data, reduced_AIC_model, cost = cost_func, K = 10)
cv_error$delta

##roc
probabilities <- predict(reduced_AIC_model, type = "response")
probabilities

prediction <- prediction(probabilities, credit_data$response)
prediction@predictions
prediction@labels

roc <- performance(prediction, "tpr", "fpr")
roc

auc <- performance(prediction, "auc")
auc

auc_value <- auc_perf@y.values[[1]]
auc_value

##plot
plot(roc, colorize = TRUE, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.6, 0.3, paste("AUC =", round(auc_value, 4)), cex = 1.2)







