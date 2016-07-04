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


# No settings of interest to manipulate in Logistical model
model = Logistic(class ~ . , data = data) 
  
eval = evaluate_Weka_classifier(model, 
                                  numFolds = 10, 
                                  complexity = FALSE, 
                                  seed = 1, 
                                  class = TRUE)
  
model1_details = eval$details

model2_kappa = eval$details["kappa"]  

# Kappa = 56.8% is not preferable. 
# Perhaps, some variables are not needed in the model.

# Feature Selection 
# Rank and select attributes based on information gain metric
 InfoGainAttributeEval(class ~. , data = data)
 
#Least information gain: fnlwgt
 
#PLAN: Exclude the above from the model and assess performance again
 
 data = subset(data, select = c(-fnlwgt))
 
 model = Logistic(class ~ . , data = data) 
 
 eval = evaluate_Weka_classifier(model, 
                                 numFolds = 10, 
                                 complexity = FALSE, 
                                 seed = 1, 
                                 class = TRUE)
 
model2_details  = eval$details
model2_kappa = eval$details["kappa"]  



model1_details
model2_details

model

# After examining both model results side-by-side 
# the information offered by fnlwgt does help improve the model by a small amount.
# Feature selection did not help improve the predictive model in this case.

# A logistic regression model might not produce the best predictive model for this data set.
