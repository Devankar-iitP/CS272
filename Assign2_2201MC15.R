# Basic Libraries
library(openxlsx)
library(caret)
library(pROC)

# Loading the dataset
file_name <- loadWorkbook("E:/Downloads/hurricanesNew.xlsx")
data <- read.xlsx(file_name, sheet = "hurricanes")
str(data)

# Map the target variable to binary format
# Making tropical region or type = 0 as 1 and else as 0
data$binary_target <- ifelse(data$Type == 0, 0, 1)

# Train-Test Split
# Typically, the data is split into a majority (80%) for training and a minority (20%) for testing.
set.seed(123) # for reproducibility
train_index <- createDataPartition(data$binary_target, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit logistic regression model using a single predictor 
# Let's say the predictor be FirstLat
# Train logistic regression model
logistic_model <- glm(binary_target~FirstLat, data = data, family = "binomial")

# Summarize the model
summary(logistic_model)

# Making the predictions on training data
train_predictions <- predict(logistic_model, type = "response")

# Convert predicted probabilities to class labels (Tropical or Non-Tropical)
train_predicted_classes <- ifelse(train_predictions > 0.5 , 1, 0)

# Compute accuracy on the training data
train_accuracy <- mean(train_predicted_classes == data$binary_target)
cat("We get Training Accuracy as:", train_accuracy, "\n")

# Define threshold values
thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# Required lists that will store the results
residual_differences <- numeric(length(thresholds))
confusion_matrices <- vector("list", length(thresholds))
accuracies <- numeric(length(thresholds))
precisions <- numeric(length(thresholds))
recalls <- numeric(length(thresholds))
f_measures <- numeric(length(thresholds))

# Evaluate the classifier for each threshold
for (i in seq_along(thresholds)) {
  # Adjust threshold for class prediction
  threshold <- thresholds[i]
  train_predicted_classes <- ifelse(train_predictions > threshold, 0, 1)
  
  # Calculate residual difference
  residual_differences[i] <- mean(train_predicted_classes != data$binary_target)
  
  # Compute confusion matrix
  confusion_matrices[[i]] <- table(Actual = data$binary_target, Predicted = train_predicted_classes)
  
  # Compute accuracy
  accuracies[i] <- mean(train_predicted_classes == data$binary_target)
  
  confusion_matrix <- table(Actual = data$binary_target, Predicted = train_predicted_classes)
  print(confusion_matrix)
  
  # Compute precision and recall
  if (0 %in% rownames(confusion_matrix) && 0 %in% colnames(confusion_matrix)) {
    TP <- confusion_matrix[1, 1]  # True Positives
    FP <- confusion_matrix[2, 1]  # False Positives
    FN <- confusion_matrix[1, 2]  # False Negatives
    if ((TP + FN) > 0) {
      recalls[i] <- TP / (TP + FN)
    } else {
      recalls[i] <- 0  # Set recall to zero if there are no true positives or false negatives
    }
    
    if ((TP + FP) > 0) {
      precisions[i] <- TP / (TP + FP)
    } else {
      precisions[i] <- 0  # Set precision to zero if there are no true positives
    }
  } 
  else {
    precisions[i] <- NA  # Set precision to NA if class '0' is not present in confusion matrix
    recalls[i] <- NA  # Set recall to NA if class '0' is not present in confusion matrix
  }
  
  # Compute F-measure
  if (precisions[i] + recalls[i] > 0) {
    f_measures[i] <- 2 * precisions[i] * recalls[i] / (precisions[i] + recalls[i])
  } else {
    f_measures[i] <- 0  # Set F-measure to zero if both precision and recall are zero
  }
}

# Print all the results for each threshold
for (i in seq_along(thresholds)) {
  cat("At threshold:", thresholds[i],"\n\n")
  cat("Residual Difference: ", residual_differences[i], "\n")
  cat("Confusion Matrix:\n", confusion_matrices[[i]], "\n")
  cat("Accuracy:", accuracies[i], "\n")
  cat("Precision:", precisions[i], "\n")
  cat("Recall:", recalls[i], "\n")
  cat("F-measure:", f_measures[i], "\n")
  cat("-----************---------\n\n")
}

unique(data$binary_target)
roc_curves <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  # Compute ROC curve
  roc_curves[[i]] <- roc(as.numeric(data$binary_target == 0), train_predictions, thresholds = thresholds,plot=TRUE)
}

# Plot ROC curves for each threshold ---> Arrange plots in a 2x3 grid
par(mfrow = c(2, 3))
for (i in seq_along(roc_curves)) {
  plot(roc_curves[[i]], main = paste("ROC Curve at (Threshold =", thresholds[i], ")"),xlim = c(0, 1),ylim=c(0,1),col='skyblue')
}

model_<-glm(binary_target ~ FirstLat+FirstLon, data = data, family = binomial)
summary(model)

# Making the predictions on training data
train_predictions <- predict(model_, type = "response")

# Convert predicted probabilities to class labels (Tropical or Non-Tropical)
train_predicted_classes <- ifelse(train_predictions > 0.5 , 1, 0)

# Compute accuracy on the training data
train_accuracy <- mean(train_predicted_classes == data$binary_target)
cat("We get Training Accuracy as:", train_accuracy, "\n")

# Define threshold values
thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# Required lists that will store the results
residual_differences <- numeric(length(thresholds))
confusion_matrices <- vector("list", length(thresholds))
accuracies <- numeric(length(thresholds))
precisions <- numeric(length(thresholds))
recalls <- numeric(length(thresholds))
f_measures <- numeric(length(thresholds))

# Evaluate the classifier for each threshold
for (i in seq_along(thresholds)) {
  # Adjust threshold for class prediction
  threshold <- thresholds[i]
  train_predicted_classes <- ifelse(train_predictions > threshold, 0, 1)
  
  # Calculate residual difference
  residual_differences[i] <- mean(train_predicted_classes != data$binary_target)
  
  # Compute confusion matrix
  confusion_matrices[[i]] <- table(Actual = data$binary_target, Predicted = train_predicted_classes)
  
  # Compute accuracy
  accuracies[i] <- mean(train_predicted_classes == data$binary_target)
  
  confusion_matrix <- table(Actual = data$binary_target, Predicted = train_predicted_classes)
  print(confusion_matrix)
  
  # Compute precision and recall
  if (0 %in% rownames(confusion_matrix) && 0 %in% colnames(confusion_matrix)) {
    TP <- confusion_matrix[1, 1]  # True Positives
    FP <- confusion_matrix[2, 1]  # False Positives
    FN <- confusion_matrix[1, 2]  # False Negatives
    if ((TP + FN) > 0) {
      recalls[i] <- TP / (TP + FN)
    } else {
      recalls[i] <- 0  # Set recall to zero if there are no true positives or false negatives
    }
    
    if ((TP + FP) > 0) {
      precisions[i] <- TP / (TP + FP)
    } else {
      precisions[i] <- 0  # Set precision to zero if there are no true positives
    }
  } 
  else {
    precisions[i] <- NA  # Set precision to NA if class '0' is not present in confusion matrix
    recalls[i] <- NA  # Set recall to NA if class '0' is not present in confusion matrix
  }
  
  # Compute F-measure
  if (precisions[i] + recalls[i] > 0) {
    f_measures[i] <- 2 * precisions[i] * recalls[i] / (precisions[i] + recalls[i])
  } else {
    f_measures[i] <- 0  # Set F-measure to zero if both precision and recall are zero
  }
}

# Print all the results for each threshold
for (i in seq_along(thresholds)) {
  cat("At threshold:", thresholds[i],"\n\n")
  cat("Residual Difference: ", residual_differences[i], "\n")
  cat("Confusion Matrix:\n", confusion_matrices[[i]], "\n")
  cat("Accuracy:", accuracies[i], "\n")
  cat("Precision:", precisions[i], "\n")
  cat("Recall:", recalls[i], "\n")
  cat("F-measure:", f_measures[i], "\n")
  cat("-----************---------\n\n")
}

unique(data$binary_target)
roc_curves <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  # Compute ROC curve
  roc_curves[[i]] <- roc(as.numeric(data$binary_target == 0), train_predictions, thresholds = thresholds,plot=TRUE)
}

# Plot ROC curves for each threshold ---> Arrange plots in a 2x3 grid
par(mfrow = c(2, 3))
for (i in seq_along(roc_curves)) {
  plot(roc_curves[[i]], main = paste("ROC Curve at (Threshold =", thresholds[i], ")"),xlim = c(0, 1),ylim=c(0,1),col='red')
}