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


for (i in 1:nrow(controls)){
  adult_j48 = J48(class ~ ., data = data, 
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
results

# Best kappa: C= 0.15 M = 2
model = J48(class ~ ., data = data, 
                control = Weka_control(C = 0.15,
                                       M = 2) )

eval = evaluate_Weka_classifier(model, 
                                     numFolds = 10,
                                     complexity = FALSE, 
                                     seed = 1, 
                                     class = TRUE)

eval$details

# View text version of the model
model

# View image of tree

# 1. Write a .dot file
write_to_dot(adult_j48, "adult_j48.dot")

# 2. Download GraphViz to view and create .dot file as a image 
# (e.g., .png  .jpeg)
# http://www.graphviz.org/
