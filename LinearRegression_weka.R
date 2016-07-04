######################################################################
# Linear Regression - Weka
# A dataset with numeric variables was used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
#   Only numeric data from this dataset was used
######################################################################

library(RWeka)

# The dataset no longer uses nominal variables for the purpose of 
# demonstrating the implementation of the LinearRegression 
data = read.csv(file="adultNum.csv", header=T)

#summary(data)
#WOW("LinearRegression")


controls = data.frame(
  N = numeric())

controls = rbind(controls, c(1))
controls = rbind(controls, c(2))
controls = rbind(controls, c(0))

results = data.frame(optionNum = numeric(),
                     details= numeric())


for (i in 1:nrow(controls)){
  model = LinearRegression(class ~ . , data = data, 
                    control = Weka_control(
                      S = controls[i, 1]) )
  
  eval = evaluate_Weka_classifier(model, 
                                   numFolds = 10, 
                                   complexity = FALSE, 
                                   seed = 1, 
                                   class = TRUE)
  
  results = rbind(results, c(i , eval$details['correlationCoefficient']) ) 
  
}


results
# All variations yield the same correlation coefficient


# Implement and examine the default model
model = LinearRegression(class ~ . , data = data, 
                         control = Weka_control(
                           S = 0) )

eval = evaluate_Weka_classifier(model, 
                                numFolds = 10, 
                                complexity = FALSE, 
                                seed = 1, 
                                class = TRUE)


# View evaluation results
eval$details
# A correlation of 0.477 is not very good as it is close to 0.5, 
# indicating that this model does no better than a random predictor. 

 
# View the linear regression model
model

# This is not a particularly good model. 
# Since the nominal variables were removed to allow this linear regression, 
# the removed nominal variables are likely to be important predictors. 