library(kernlab)

data(spam)
str(spam[, 1:5])

# create train and test data sets
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1, ]
testSpam <- spam[trainIndicator == 0, ]

head(trainSpam)
table(trainSpam$type)

# plots
plot(trainSpam$capitalAve ~ trainSpam$type)

# using log to reduce skewness
# adding 1 because there are a lot of 0s in the data, and you can't take log of 0
# this is ok in EDA but not for actual analysis
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type) 

# relationships between predictors
plot(log10(trainSpam[, 1:4] + 1))

# clustering
hCluster <- hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

# log transforming predictors and removing last two vars
hClusterUpdated <- hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

# statistical prediction / modeling
trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction <- function(x,y) sum(x != (y > 0.5))
cvError <- rep(NA, 55)
library(boot)
for (i in 1:55) {
        lmFormula <- reformulate(names(trainSpam)[i], response = 'numType')
        glmFit <- glm(lmFormula, family = 'binomial', data = trainSpam)
        cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

# Use the best model from the group
predictionModel <- glm(numType ~ charDollar, family = "binomial", data = trainSpam)

# Get predictions on the test set
predictionTest <- predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])

# Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] <- "spam"

# Classification table
table(predictedSpam, testSpam$type)

# Error rate
(61 + 458) / (1346 + 458 + 61 + 449)

















