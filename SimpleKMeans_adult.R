######################################################################
# SimpleKMeans -  Weka
# A dataset with numeric and nominal variables used.
# Author: Skyejustis
#
# Dataset Description: https://archive.ics.uci.edu/ml/datasets/Adult
######################################################################

library(RWeka)
data = read.csv(file="adult.csv", header=T)

#summary(data)
#WOW("SimpleKMeans")

# Model the data for the inputs only
# Omit the class variable (last column of data)
model = SimpleKMeans(data[,-15], 
                     control = Weka_control(N = 2, 
                                            init = 0 #random initialization method
                                            ))


#View cluster and class number/proportions per cluster
table(predict(model), data$class) 
prop.table(table(predict(model), data$class), 1) #row-wise proportion

# lessThan50K  moreThan50K
# 0        10064         6668
# 1        14656         1173

# lessThan50K  moreThan50K
# 0   0.60148219   0.39851781
# 1   0.92589551   0.07410449

###The results do not show that a cluster does not show skew towards one class or another

# Does 4 clusters show a more pronounced difference betw the 2 groups
model = SimpleKMeans(data[,-15], 
                     control = Weka_control(N = 4, 
                                            init = 0 #random initialization method
                     ))
#View cluster and class number/proportions per cluster
table(predict(model), data$class) 
prop.table(table(predict(model), data$class), 1) #row-wise proportion

# lessThan50K  moreThan50K
# 0         1865         3702
# 1         7677          116
# 2         8060         3099
# 3         7118          924

# lessThan50K  moreThan50K
# 0   0.33500988   0.66499012  --skewed to moreThan50K
# 1   0.98511485   0.01488515  --skewed to lessThan50K
# 2   0.72228694   0.27771306  --skewed to lessThan50K
# 3   0.88510321   0.11489679  --skewed to moreThan50K


#View cluster and characteristics per cluster
model
