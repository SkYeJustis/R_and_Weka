######################################################################
# Logistic Regression - Weka
# A dataset with numeric and nominal variables was used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
######################################################################

library(RWeka)

data = read.csv(file="adult.csv", header=T)

# summary(data)

# Weka note: 
# Missing values are replaced using a ReplaceMissingValuesFilter,
# and nominal attributes are transformed into 
# numeric attributes using a NominalToBinaryFilter.

# WOW("Logistic")

#Using convenient createDataPartition package to split data
#Rows are randomized in test and train sets. 
library(caret) 
inTraining <- createDataPartition(data$class, p = .75, list = FALSE)
train = data[inTraining, ] # 75% will be in training set
test = data[-inTraining, ] # 25% will be in testing set

# No settings of interest to manipulate in Logistical model
model = Logistic(class ~ . , data = train) 
  
eval = evaluate_Weka_classifier(model, 
                                  numFolds = 10, 
                                  complexity = FALSE, 
                                  seed = 1, 
                                  class = TRUE)
  
model1_details = eval$details

model1_kappa = eval$details["kappa"]  

model1_kappa 
# Kappa = 56.4% is fair.
# Perhaps, some variables are not needed in the model. 
# Eliminating such noise variables may improve model performance.

## Feature Selection 
# Rank and select attributes based on information gain metric
 InfoGainAttributeEval(class ~. , data = train)
 
#Least information gain: fnlwgt
 
#PLAN: Exclude the above from the model and assess performance again
 
 train = subset(train, select = c(-fnlwgt))
 test = subset(test, select= c(-fnlwgt))
 
 model = Logistic(class ~ . , data = train) 
 
 eval = evaluate_Weka_classifier(model, 
                                 numFolds = 10, 
                                 complexity = FALSE, 
                                 seed = 1, 
                                 class = TRUE)
 
model2_details  = eval$details
model2_kappa = eval$details["kappa"]  

model2_kappa
# Same Kappa as previous model. 56.4% is fair.

model1_details
model2_details

# Barely change in models. 
# May be due to the 0.000 information gain value of fnlwgt. 

model

# After examining both model results side-by-side 
# the information offered by fnlwgt does help improve the model by a small amount.
# Feature selection did not help improve the predictive model in this case.

# A logistic regression model might not produce the best predictive model for this data set.

# Extra
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
# TP: 70.6%
# FP: 5.2%
# TN: 14.3%
# FN: 9.7%
# Accuracy: 70.6 + 14.3 =   84.9%
# Sensitivity: 70.6 / (70.6 + 9.7) = 87.9%

