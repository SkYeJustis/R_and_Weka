######################################################################
# A large dataset - Weka
# A dataset with numeric and nominal variables was used.
# Author: Skyejustis
#
# Dataset can be found online. You may need to convert it to csv form
# The 25mb limit prevents me from posting the dataset.
# Dataset Description: http://wwwdasis.samhsa.gov/teds08/TEDS2k8DWeb.pdf
######################################################################

# Address heap space issues for large dataset (1.7 million rows)
# Computer in use has at least 8gb RAM
# Requesting 7gb RAM allocated to RJava for RWeka
options( java.parameters = "-Xmx7g" )
library( "RWeka" )

# Large dataset that does not work via Weka 3.7 GUI 
data = read.csv(file="TEDS2008DischargedPatients.csv", header=T)

# Rows: 1,677,937 Cols: 66
dim(data)

colnames(data)



#Using convenient createDataPartition package to split data
#Rows are randomized in test and train sets. 
library(caret) 
inTraining <- createDataPartition(data$LOS, p = .75, list = FALSE)
train = data[inTraining, ] # 75% will be in training set
test = data[-inTraining, ] # 25% will be in testing set

# Check for a way to split data along a Length of Stay (LOS) 
hist(train$LOS)

# Creating a binary variable
train$LOSnom <- as.numeric(train$LOS > 30)
hist(train$LOSnom)


# Alter test in the same way 
test$LOSnom <- as.numeric(test$LOS > 30)
hist(test$LOSnom)


# Get rid of unneccessary predictors
colnames(train)
train = subset(train, select = c(-X, -CASEID, -LOS))
test = subset(test, select= c(-X, -CASEID, -LOS))


## Feature Selection 
# Rank and select attributes based on information gain metric

train$LOSnom = as.factor(train$LOSnom)
test$LOSnom = as.factor(test$LOSnom)

# Remove scientific notation
options(scipen=999)

infoGainRes = InfoGainAttributeEval(LOSnom ~. , data = train)

infoGainResdf = as.data.frame(infoGainRes)

infoGainResdf$variables = rownames(infoGainResdf)

infoGainResdf = infoGainResdf[ order(infoGainResdf$infoGainRes, decreasing = TRUE), ]  

infoGainResdf


#Least information gain: 

# PLAN: Take top 15 variables 
# REASON: Too many variables and 1.2 million rows (train) causes model-building 
# and evaluation to take a VERY long time. (It is doable though.)

train = subset(train, select = c(SERVSETD, FREQ1, PSOURCE, PMSA, CBSA,
                                 STFIPS, FREQ2, DETCRIM, DSMCRIT, LIVARAG,
                                 SUB1, NOPRIOR, DETNLF, DIVISION, MARFLG, LOSnom))

test = subset(test, select= c(SERVSETD, FREQ1, PSOURCE, PMSA, CBSA,
                              STFIPS, FREQ2, DETCRIM, DSMCRIT, LIVARAG,
                              SUB1, NOPRIOR, DETNLF, DIVISION, MARFLG, LOSnom))


# Weka note: 
# Missing values are replaced using a ReplaceMissingValuesFilter,
# and nominal attributes are transformed into 
# numeric attributes using a NominalToBinaryFilter.

# WOW("Logistic")


# No settings of interest to manipulate in Logistical model
model = Logistic(LOSnom ~ . , data = train) 

eval = evaluate_Weka_classifier(model, 
                                numFolds = 10, 
                                complexity = FALSE, 
                                seed = 1, 
                                class = TRUE)


eval$details
# Kappa = 58.3%
# Accuracy = 79.6% 
# Overall, the model does a fair job of classifying unseen data


# View logistic regression
model

predictions = predict(model, newdata = test)

predictions

# Confusion matrix for test set
table(test$LOSnom, predictions)

# Relative frequency 
table(test$LOSnom, predictions) / nrow(test)

# From the above table the following is extracted for the test data 
# using the resulting model.

# Indicates (lessThan30Days) 
# TP: 31.1%
# FP: 14%
# TN: 48.4%
# FN: 6.4%
# Accuracy: 31.1 + 48.4 =   79.5%
# Sensitivity: 31.1 / (31.1 + 6.4) = 82.9%

