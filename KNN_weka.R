######################################################################
# K-Nearest Neighbors -- Weka
# A dataset with numeric and nominal variables was used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
######################################################################

library(RWeka)
data = read.csv(file="adult.csv", header=T)

#summary(data)
#WOW("IBk")

#Using convenient createDataPartition package to split data
#Rows are randomized in test and train sets. 
library(caret) 
inTraining <- createDataPartition(data$class, p = .75, list = FALSE)
train = data[inTraining, ] # 75% will be in training set
test = data[-inTraining, ] # 25% will be in testing set


# CV Parameter Optimization -- one way
# Parameter of interest: K (number of nearest neighbors)
controls = data.frame(K = numeric())

controls = rbind(controls, c( 1)) #default
controls = rbind(controls, c( 5)) 
controls = rbind(controls, c( 10))


results = data.frame(optionNum = numeric(),
                     details= numeric())


for (i in 1:nrow(controls)){
  model = IBk(class ~ ., data = train, 
                  control = Weka_control(K = controls[i, 1] ))
  
  eval <- evaluate_Weka_classifier( model, 
                                       numFolds = 10, 
                                       complexity = FALSE, 
                                       seed = 1, 
                                       class = TRUE)
  
  results = rbind(results, c(i , eval$details['kappa']) ) 
  
}

names (results) = c("optionNum", "Kappa" )
results

# Option 1 / K = 5 had the best kappa rating
model = IBk(class ~ ., data = train, 
                control = Weka_control(K = 5))

eval <- evaluate_Weka_classifier(model, 
                                 numFolds = 10, 
                                 complexity = FALSE, 
                                 seed = 1, 
                                 class = TRUE)

eval$details['kappa']
# Kappa: 0.498 - A higher kappa would be ideal but this is not a bad rating.
# Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
# Indicates how well a model does (observed accuracy) against random chance (expected accuracy)

eval$details
# Accuracy: 83.2%


# View setting used to create this "lazy" classifier
# A different training set would preduce different results
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
# TP: 68.4%
# FP: 7.5%
# TN: 10.1%
# FN: 13.9%
# Accuracy: 68.4 + 10.1 =   78.5%
# Sensitivity: 68.4 / (68.4 + 13.9) = 83.1%



