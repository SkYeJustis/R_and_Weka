######################################################################
# Association Rules (JRip) in Weka
# A dataset with numeric and nominal variables used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
######################################################################
library(RWeka)
data = read.csv(file="adult.csv", header=T)

#Using convenient createDataPartition package to split data
#Rows are randomized in test and train sets. 
library(caret) 
inTraining <- createDataPartition(data$class, p = .75, list = FALSE)
train = data[inTraining, ] # 75% will be in training set
test = data[-inTraining, ] # 25% will be in testing set


#summary(train)
#WOW("JRip")


# CV Parameter Optimization -- one way
# Parameter of interest: N <min. weights>
# Set the minimal weights of instances within a split.
controls = data.frame(N = numeric())

controls = rbind(controls, c(2))
controls = rbind(controls, c(3))
controls = rbind(controls, c(4))

results = data.frame(optionNum = numeric(),
                     details= numeric())


for (i in 1:nrow(controls)){
  adult_jrip = JRip(class ~ ., data = train, 
                  control = Weka_control(N = controls[i, 1]) )
  
  eval <- evaluate_Weka_classifier(adult_jrip, 
                                   numFolds = 10, 
                                   complexity = FALSE, 
                                   seed = 1, 
                                   class = TRUE)
  
  results = rbind(results, c(i , eval$details['kappa']) ) 
  
}

names (results) = c("optionNum", "Kappa" )
results

# No difference in Kappa. Use default.
model = JRip(class ~ ., data = train)

eval <- evaluate_Weka_classifier(model, 
                                 numFolds = 10, 
                                 complexity = FALSE, 
                                 seed = 1, 
                                 class = TRUE)

eval$details["kappa"] # 52% is fair

#Obtain formated details
eval$detailsClass

#Obtain confusionMatrix
eval$confusionMatrix


# View entire model - a set of rules
model$classifier



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
# TP: 71.6%
# FP: 4.3%
# TN: 11.1%
# FN: 12.9%
# Accuracy: 71.6 + 4.3 =   75.9%
# Sensitivity: 71.6 / (71.6 + 12.9) = 84.7%


