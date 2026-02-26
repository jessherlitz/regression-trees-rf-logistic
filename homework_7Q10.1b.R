### Homework 7 10.1.b

library(randomForest)
library(caret)

# loading our data
crime_data <- read.table(
  "uscrime.txt", 
  header = TRUE
)

# initial random forest model
set.seed(1)
random_forest_model <- randomForest(
  Crime ~ ., 
  data = crime_data, 
  importance = TRUE, 
  ntree = 500
)
print(random_forest_model)
importance(random_forest_model)
varImpPlot(random_forest_model)

# out of bag R-squared for default model
y_out_of_bag_prediction_values <- random_forest_model$predicted
SSE <- sum((crime_data$Crime - y_out_of_bag_prediction_values)^2)
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
out_of_bag_R2 <- 1 - SSE/SST
out_of_bag_R2

# tuning mtry by averaging OOB MSE across multiple seeds
mtry_avg_errors <- rep(0, 15)
seeds <- c(1, 5, 11, 111, 511)
for (m in 1:15) {
  mse_vals <- c()
  for (s in seeds) {
    set.seed(s)
    rf <- randomForest(Crime ~ ., data = crime_data, ntree = 500, mtry = m)
    mse_vals <- c(mse_vals, mean((rf$predicted - crime_data$Crime)^2))
  }
  mtry_avg_errors[m] <- mean(mse_vals)
}
mtry_avg_errors

# plot
plot(1:15, mtry_avg_errors, type = "b", xlab = "mtry values", ylab = "out of bag MSE",
     main = "MTRY vs out of bag MSE")
best_mtry <- which.min(mtry_avg_errors)
best_mtry

# best random forest model
set.seed(1)
best_rf_model <- randomForest(
  Crime ~ ., 
  data = crime_data, 
  ntree = 500, 
  mtry = best_mtry, 
  importance = TRUE
)
print(best_rf_model)
importance(best_rf_model)
varImpPlot(best_rf_model, main = paste("mtry =", best_mtry))

# out of bag R-squared for best model
y_out_of_bag_predicted <- best_rf_model$predicted
SSE <- sum((crime_data$Crime - y_out_of_bag_predicted)^2)
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
out_of_bag_R2_value <- 1 - SSE/SST
out_of_bag_R2_value

# cross validation
set.seed(1)
ctrl <- trainControl(method = "cv", number = 5)
rf_cv <- train(Crime ~ ., 
               data = crime_data, 
               method = "rf", 
               trControl = ctrl, 
               ntree = 500)
print(rf_cv)

# testing new data
test_data <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
  LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120,
  U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)
predict(best_rf_model, newdata = test_data)


