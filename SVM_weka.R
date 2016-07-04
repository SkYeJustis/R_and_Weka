
######################################################################
# SVM Weka demo
# A dataset with numeric and nominal variables used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
######################################################################

library(RWeka)
data = read.csv(file="adult.csv", header=T)

#summary(data)
#WOW("SMO")

#Using convenient createDataPartition package to split data
#Rows are randomized in test and train sets. 
library(caret) 
inTraining <- createDataPartition(data$class, p = .75, list = FALSE)
train = data[inTraining, ] # 75% will be in training set
test = data[-inTraining, ] # 25% will be in testing set


# Weka note:
# This implementation globally replaces all missing values and transforms nominal attributes into binary ones. 
# It also normalizes all attributes by default. (In that case the coefficients in the output are based on the normalized data, not the original data --- this is important for interpreting the classifier.)

# No settings of interest to manipulate in SMO / SVM model
model = SMO(class ~ ., data = train)
  
eval = evaluate_Weka_classifier(model, 
                                       numFolds = 10, 
                                       complexity = FALSE, 
                                       seed = 1, 
                                       class = TRUE)
  
eval$details
eval$details['kappa'] 

# View model
model

# Obtaining predictions for test set - 8140 obs
predictions = predict(model, newdata = test)

predictions

# Confusion matrix for test set
table(test$class, predictions)

# Relative frequency 
table(test$class, predictions) / nrow(test)

# From the above table the following is extracted for the test data 
# using the resulting model.

# Indicates (lessThan50K) 
# TP: 71.4%
# FP: 4.6%
# TN: 13.9%
# FN: 10%
# Accuracy: 71.4 + 13.9 = 85.3%
# Sensitivity: 71.4 / (71.4 + 10) = 87.7%




  




