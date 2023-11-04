if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 1. Install and Load the Required Packages ----
## stats ----
if (require("stats")) {
  require("stats")
} else {
  install.packages("stats", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## MASS ----
if (require("MASS")) {
  require("MASS")
} else {
  install.packages("MASS", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## glmnet ----
if (require("glmnet")) {
  require("glmnet")
} else {
  install.packages("glmnet", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## rpart ----
if (require("rpart")) {
  require("rpart")
} else {
  install.packages("rpart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# Load and split the dataset
data(mtcars)

# Define an 80:20 train:test data split of the dataset.
set.seed(7)  # Setting a seed for reproducibility
train_index <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
mtcars_train <- mtcars[train_index, ]
mtcars_test <- mtcars[-train_index, ]

# Train the model with 5-fold cross validation
set.seed(7)
train_control <- trainControl(method = "cv", number = 5)
mtcars_caret_model_lm <- train(mpg ~ cyl + disp + hp + wt + qsec, data = mtcars_train,
                               method = "lm", metric = "RMSE", trControl = train_control)

# Display the model's details
print(mtcars_caret_model_lm)

# Make predictions
predictions <- predict(mtcars_caret_model_lm, newdata = mtcars_test)

# Display the model's evaluation metrics
##### RMSE ----
rmse <- sqrt(mean((mtcars_test$mpg - predictions)^2))
print(paste("RMSE =", sprintf(rmse, fmt = "%#.4f")))

##### R Squared ----
r_squared <- 1 - (sum((mtcars_test$mpg - predictions)^2) / sum((mtcars_test$mpg - mean(mtcars_test$mpg))^2))
print(paste("R Squared =", sprintf(r_squared, fmt = "%#.4f")))

##### MAE ----
mae <- mean(abs(predictions - mtcars_test$mpg))
print(paste("MAE =", sprintf(mae, fmt = "%#.4f")))

# Scatter plot of actual vs. predicted values
plot(mtcars_test$mpg, predictions, main = "Actual vs. Predicted (Linear Regression with CARET)",
     xlab = "Actual MPG", ylab = "Predicted MPG", pch = 16, col = "blue")
abline(a = 0, b = 1, col = "red")  # Adds a red line for reference






# Load necessary libraries
install.packages("mlbench")
library(mlbench)
library(e1071)
library(caret)

## 2. Naïve Bayes ----

### 2.a. Naïve Bayes Classifier for a Classification Problem without CARET ----

# Load and split the dataset ----
data(Sonar)

# Define a 70:30 train:test data split of the dataset.
train_index <- createDataPartition(Sonar$Class,
                                   p = 0.7,
                                   list = FALSE)
sonar_train <- Sonar[train_index, ]
sonar_test <- Sonar[-train_index, ]

#### Train the model ----
sonar_model_nb <- naiveBayes(Class ~ .,
                             data = sonar_train)

#### Display the model's details ----
print(sonar_model_nb)

#### Make predictions ----
predictions <- predict(sonar_model_nb,
                       sonar_test[, 1:60])

#### Display the model's evaluation metrics ----
confusion_matrix <- confusionMatrix(predictions,
                                    sonar_test$Class)
print(confusion_matrix)

fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")


### 2.b. Naïve Bayes Classifier for a Classification Problem with CARET ----

# Load and split the dataset ----
data(Sonar)

# Define a 70:30 train:test data split of the dataset.
train_index <- createDataPartition(Sonar$Class,
                                   p = 0.7,
                                   list = FALSE)
sonar_train <- Sonar[train_index, ]
sonar_test <- Sonar[-train_index, ]

#### Train the model ----
set.seed(7)
train_control <- trainControl(method = "cv", number = 5)
sonar_caret_model_nb <- train(Class ~ .,
                              data = sonar_train,
                              method = "nb", metric = "Accuracy",
                              trControl = train_control)

#### Display the model's details ----
print(sonar_caret_model_nb)

#### Make predictions ----
predictions <- predict(sonar_caret_model_nb,
                       sonar_test[, 1:60])

#### Display the model's evaluation metrics ----
confusion_matrix <- confusionMatrix(predictions,
                                    sonar_test$Class)
print(confusion_matrix)

fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")


## 3. k-Nearest Neighbours ----

### 3.a. kNN for a classification problem without CARET's train function ----

# Load and split the dataset ----
data(Sonar)

# Define a 70:30 train:test data split of the dataset.
train_index <- createDataPartition(Sonar$Class,
                                   p = 0.7,
                                   list = FALSE)
sonar_train <- Sonar[train_index, ]
sonar_test <- Sonar[-train_index, ]

#### Train the model ----
sonar_caret_model_knn <- knn3(Class ~ ., data = sonar_train, k=3)

#### Display the model's details ----
print(sonar_caret_model_knn)

#### Make predictions ----
predictions <- predict(sonar_caret_model_knn,
                       sonar_test[, 1:60],
                       type = "class")

#### Display the model's evaluation metrics ----
table(predictions, sonar_test$Class)

# Or alternatively:
confusion_matrix <- confusionMatrix(predictions,
                                    sonar_test$Class)
print(confusion_matrix)

fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")

#### Load and split the dataset ----
data(Sonar)

# Define a 70:30 train:test data split of the dataset.
set.seed(7)
train_index <- createDataPartition(Sonar$Class, p = 0.7, list = FALSE)
sonar_train <- Sonar[train_index, ]
sonar_test <- Sonar[-train_index, ]

#### Train the model (SVM for classification) ----
library(e1071)
sonar_model_svm <- svm(Class ~ ., data = sonar_train, kernel = "radial")

#### Display the model's details ----
print(sonar_model_svm)

#### Make predictions ----
predictions <- predict(sonar_model_svm, sonar_test[, 1:60])

#### Display the model's evaluation metrics ----
confusion_matrix <- caret::confusionMatrix(predictions, sonar_test$Class)
print(confusion_matrix)

fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")






# Step 1. Load and explore the dataset

# Load the arules package
install.packages("arules")
library(arules)

# Load the Groceries dataset
data("Groceries")


# STEP 2. Load and pre-process the dataset ----
# We'll skip this step as the Groceries dataset is already in a suitable format.

# STEP 3. Create the transaction data ----
transactions <- as(Groceries, "transactions")

# Check summary statistics of the transaction data
summary(transactions)

# STEP 4. Mine association rules ----
min_support <- 0.001
min_confidence <- 0.5

association_rules <- apriori(transactions,
                             parameter = list(support = min_support,
                                              confidence = min_confidence))

# Print summary of association rules
summary(association_rules)

# STEP 5. Print the association rules ----
inspect(head(association_rules))

# Remove redundant rules ----
subset_rules <- which(colSums(is.subset(association_rules, association_rules)) > 1)
association_rules_no_reps <- association_rules[-subset_rules]
summary(association_rules_no_reps)
inspect(association_rules_no_reps)

# Save the non-redundant rules to a CSV file
write(association_rules_no_reps, file = "rules/association_rules_groceries.csv")

# STEP 6. Find specific rules ----
# Which product(s), if bought, result in a customer purchasing "whole milk"?
whole_milk_association_rules <- apriori(transactions,
                                        parameter = list(supp = min_support,
                                                         conf = min_confidence),
                                        appearance = list(default = "lhs",
                                                          rhs = "whole milk"))
inspect(head(whole_milk_association_rules))

# STEP 7. Visualize the rules ----
rules_to_plot <- association_rules_no_reps[quality(association_rules_no_reps)$confidence > 0.85]
plot(rules_to_plot)

# Filter top 20 rules with highest lift
rules_to_plot_by_lift <- head(rules_to_plot, n = 20, by = "lift")
plot(rules_to_plot_by_lift, method = "paracoord")











