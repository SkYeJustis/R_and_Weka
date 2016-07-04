######################################################################
# Decision Trees - J48 - Weka
# A dataset with numeric and nominal variables used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
######################################################################


library(RWeka)
data = read.csv(file="adult.csv", header=T)

#summary(data)
#WOW("J48")

#Using convenient createDataPartition package to split data
#Rows are randomized in test and train sets. 
library(caret) 
inTraining <- createDataPartition(data$class, p = .75, list = FALSE)
train = data[inTraining, ] # 75% will be in training set
test = data[-inTraining, ] # 25% will be in testing set



# CV Parameter Optimization -- one way
# Parameter of interest: C <pruning confidence>
#                        M <minimum number of instances>

controls = data.frame(C = numeric(),
                      M = numeric())

controls = rbind(controls, c(0.25, 2))
controls = rbind(controls, c(0.20, 2))
controls = rbind(controls, c(0.15, 2))
controls = rbind(controls, c(0.05, 2))
controls = rbind(controls, c(0.15, 3))
controls = rbind(controls, c(0.20, 3))

results = data.frame(option = numeric(),
                     details= numeric())

# Each option will be evaluated via a 10-Fold CV
for (i in 1:nrow(controls)){
  adult_j48 = J48(class ~ ., data = train, 
                   control = Weka_control(C = controls[i, 1] ,
                                          M = controls[i, 2]) )
  
  eval_j48 <- evaluate_Weka_classifier(adult_j48, 
                                       numFolds = 10, 
                                       complexity = FALSE, 
                                       seed = 1, 
                                       class = TRUE)
  
  results = rbind(results, c(i , eval_j48$details['kappa']) ) 
  
}

# View the results of each [model option number, kappa rating]
names (results) = c("optionNum", "Kappa" )
results

# Best kappa: C= 0.20 M = 2
model = J48(class ~ ., data = train, 
                control = Weka_control(C = 0.15,
                                       M = 2) )

eval = evaluate_Weka_classifier(model, 
                                     numFolds = 10,
                                     complexity = FALSE, 
                                     seed = 1, 
                                     class = TRUE)

# Results might not be exactly like "Results" df due to CV process
eval$details

# View text version of the model
model

# View image of tree

# 1. Write a .dot file
write_to_dot(model, "adult_j48.dot")

# 2. Download GraphViz to view and create .dot file as a image 
# (e.g., .png  .jpeg)
# http://www.graphviz.org/


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
# TP: 71%
# FP: 4%
# TN: 15%
# FN: 9%
# Accuracy: 71 + 15 =   86%
# Sensitivity: 71 / (71 + 9) = 88.8%


