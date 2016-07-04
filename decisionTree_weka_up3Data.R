######################################################################
# Decision Tree with Jawbone Data and CARET
# Goal: Find a predictor for more REM sleep (over 30 minutes)
# Author: Skyejustis
#
# Dataset: Author's Jawbone data
# Column descriptions here: https://jawbone.com/support/articles/000011483/historical-data
######################################################################

library(rpart)
data = read.csv(file="2016_jawbone_edited.csv", header=T)

# Note: Some columns were deleted for privacy reasons
# Note2: Other columns with minimal information was also deleted
# Note3: Added a column indicating weekend as my sleep pattern differs throughout the week
colnames(data)


str(data)


library(caret)
inTraining <- createDataPartition(data$Coded.s_rem, p = .75, list = FALSE)
train = data[inTraining, ] 
test = data[-inTraining, ]

formula = Coded.s_rem ~ .


library(mlbench)
library(party)
library(e1071)

# Cross Validation setting to assess model performance on future unseen data
fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

# Adjusting setting of interest (maxdepth)
treeGrid = expand.grid(maxdepth = c(6, 5, 4, 3, 2))


model <- train(formula, 
               data = train, 
               # Use the trainSet dataframe as the training data
               method = "ctree2",  
               # Decision tree implementation in CARET
               # Other model types here: http://topepo.github.io/caret/modelList.html
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 10), # Use 10 folds for cross-validation
               tuneGrid = treeGrid
)


plot(model, main="Cross Validation Results for Jawbone Data CTree")
# Consistent performance across all adjustment parameters -- most likely due to small dataset

model$results
# Consistent performance across all adjustment parameters -- most likely due to small dataset

model$bestTune
# maxdepth = 2 was chosen 
# Given that results were same across all settings, this was the speediest setting among all. 


# Evaluate model on test data
testPred = predict(model, test)


# Obtain accuracy and kappa
postResample(testPred, test$Coded.s_rem)
# Accuracy    Kappa 
# 0.6      0.2 
# Overall: Not so good model

# Recall = TP / (TP + FN) = pick up what is true
sensitivity(testPred, test$Coded.s_rem)
# 0.67 



# All evaluation information
confusionMatrix(testPred, test$Coded.s_rem)
# The model does not seem to do well.

# Percentages - for the model trained on the training data
confusionMatrix(model)



# View model -- See "REMSleepConditionalTree.pdf" in repo
# Model notes that my s_duration (Duration of primary sleep (in seconds).) 
# and s_clinical_deep (Length of primary Deep sleep (in seconds, UP3 and UP4 only))
# determines whether I get less than 30 minutes (Coded.s_rem=0) 
# or 30+ minutes (Coded.s_rem=1) of sleep at night.

# Rules:
# (1) s_duration <= 22658 seconds (6.29 hours) 
#     | more likely Less REM sleep
# (2) s_duration > 22658 seconds (6.29 hours) and s_clinical_deep > 2.13 hrs 
#     | almost certainly More REM sleep
# (3) s_duration > 22658 seconds and s_clinical_deep <= 2.13 hrs
#     | more likely certainly More REM sleep

plot(model$finalModel, main= "REM Sleep Conditional Tree" )

# It looks like this model recommends that I sleep more than 6.5 hours and 
# make sure that I am sleeping somewhere that does not interrupt sleep so that
# I can obtain at least 2.13 hours of deep sleep.

# According to sleep literature, these recommendations make sense. 
# REM sleep occurs more often as there is more uninterrupted sleep.
# Reference:
# http://www.huffingtonpost.com/tim-ferriss/11-tricks-for-perfect-sle_b_2527454.html
# http://www.wikihow.com/Get-More-REM-Sleep



