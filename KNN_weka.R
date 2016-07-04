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


# CV Parameter Optimization -- one way
# Parameter of interest: K (number of nearest neighbors)
controls = data.frame(K = numeric())

controls = rbind(controls, c( 1)) #default
controls = rbind(controls, c( 5)) 
controls = rbind(controls, c( 10))


results = data.frame(optionNum = numeric(),
                     details= numeric())


for (i in 1:nrow(controls)){
  adult_IBk = IBk(class ~ ., data = data, 
                  control = Weka_control(K = controls[i, 1] ))
  
  eval <- evaluate_Weka_classifier(adult_IBk, 
                                       numFolds = 10, 
                                       complexity = FALSE, 
                                       seed = 1, 
                                       class = TRUE)
  
  results = rbind(results, c(i , eval$details['kappa']) ) 
  
}

results

# Option 1 / K = 5 had the best kappa rating
model = IBk(class ~ ., data = data, 
                control = Weka_control(K = 5))

eval <- evaluate_Weka_classifier(model, 
                                 numFolds = 10, 
                                 complexity = FALSE, 
                                 seed = 1, 
                                 class = TRUE)

eval$details['kappa']
# Kappa: 0.502 - A higher kappa would be ideal but this is not a bad rating.
# Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
# Indicates how well a model does (observed accuracy) against random chance (expected accuracy)

eval$details
# Accuracy: 82.5%


# View setting used to create this "lazy" classifier
# A different training set would preduce different results
model



