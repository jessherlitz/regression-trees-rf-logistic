### Homework 7 Question 10.1.a

library(tree)

# loading data
crime_data <- read.table(
  "uscrime.txt", 
  header = TRUE
)

regression_tree_model <- tree(Crime ~ ., data = crime_data)
summary(regression_tree_model)
plot(regression_tree_model)
text(regression_tree_model, pretty = 0, cex = 0.7, font = 2)

par(mfrow = c(2, 3))  
###
set.seed(1)
cv1 <- cv.tree(regression_tree_model, K = 10)
plot(cv1$size, cv1$dev, type = "b", xlab = "Terminal Nodes", ylab = "CV RSS", main = "Seed 1")

###
set.seed(2)
cv2 <- cv.tree(regression_tree_model, K = 10)
plot(cv2$size, cv2$dev, type = "b", xlab = "Terminal Nodes", ylab = "CV RSS", main = "Seed 2")
cv2$dev

###
set.seed(3)
cv3 <- cv.tree(regression_tree_model, K = 10)
plot(cv3$size, cv3$dev, type = "b", xlab = "Terminal Nodes", ylab = "CV RSS", main = "Seed 3")

###
set.seed(4)
cv4 <- cv.tree(regression_tree_model, K = 10)
plot(cv4$size, cv4$dev, type = "b", xlab = "Terminal Nodes", ylab = "CV RSS", main = "Seed 4")

###
set.seed(5)
cv5 <- cv.tree(regression_tree_model, K = 10)
plot(cv5$size, cv5$dev, type = "b", xlab = "Terminal Nodes", ylab = "CV RSS", main = "Seed 5")

###
set.seed(6)
cv6 <- cv.tree(regression_tree_model, K = 10)
plot(cv6$size, cv6$dev, type = "b", xlab = "Terminal Nodes", ylab = "CV RSS", main = "Seed 6")
par(mfrow = c(1, 1))  


par(mfrow = c(2, 2))
#### choosing to split on 2
pruned_tree2 <- prune.tree(regression_tree_model, best = 2)
plot(pruned_tree2)
summary(pruned_tree2)
text(pruned_tree2, pretty = 0, cex = 0.7, font = 2)
title("2 Nodes")

#### choosing to split on 4
pruned_tree4 <- prune.tree(regression_tree_model, best = 4)
plot(pruned_tree4)
summary(pruned_tree4)
text(pruned_tree4, pretty = 0, cex = 0.7, font = 2)
title("4 Nodes")

#### choosing to split on 5
pruned_tree5 <- prune.tree(regression_tree_model, best = 5)
summary(pruned_tree5)
plot(pruned_tree5)
text(pruned_tree5, pretty = 0, cex = 0.7, font = 2)
title("5 Nodes")

#### keeping original tree with 7
summary(regression_tree_model)
plot(regression_tree_model)
text(regression_tree_model, pretty = 0, cex = 0.7, font = 2)
title("7 Nodes")
####
par(mfrow = c(1, 1))


### R2 SCORES ----------------------------------------------
pred <- predict(pruned_tree2)
SSres <- sum((crime_data$Crime - pred)^2)
SStot <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
R2 <- 1 - SSres / SStot
R2

pred <- predict(pruned_tree4)
SSres <- sum((crime_data$Crime - pred)^2)
SStot <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
R2 <- 1 - SSres / SStot
R2

pred <- predict(pruned_tree5)
SSres <- sum((crime_data$Crime - pred)^2)
SStot <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
R2 <- 1 - SSres / SStot
R2

pred <- predict(regression_tree_model)
SSres <- sum((crime_data$Crime - pred)^2)
SStot <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
R2 <- 1 - SSres / SStot
R2

##### testing new data

test_data <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
  LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120,
  U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)

predict(pruned_tree2, newdata = test_data)
predict(pruned_tree4, newdata = test_data)
predict(pruned_tree5, newdata = test_data)
predict(regression_tree_model, newdata = test_data)


