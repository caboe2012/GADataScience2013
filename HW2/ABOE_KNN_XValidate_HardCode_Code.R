## KNN - Homework 2: Cross Validation with 5 Hard-Coded Folds ## 

##########################################
### Preprocessing for the KNN Function ###
##########################################

## Call Necessary Libray Packages ###
library(class)
library(ggplot2)

### Establish Variables ###
data<-iris
labels <- data$Species
data$Species <- NULL

#####################################
### 5 Fold Cross-Validation Split ###
#####################################

## Step 1: Randomize the 'Data' dataframe ##
set.seed(01061979)						# Initiate reproducible randomization(in this case with my DOB)

N <- nrow(data)							# N is equal to the total number of rows in data
data.random <- sample(1:N)				# Creates a Vector of random numbers (as determined by seed) from 1 to the number of rows in data


## Step 2:  Split the Randomized 'Data' dataframe into 5-folds ##

train.index1 <- data[-data.random[1:30], ]
test.index1 <- data[data.random[1:30],]

train.index2 <- data[-data.random[31:60], ]
test.index2 <- data[data.random[31:60], ]

train.index3 <- data[-data.random[61:90], ]
test.index3 <- data[data.random[61:90], ]

train.index4 <- data[-data.random[91:120], ]
test.index4 <- data[data.random[91:120], ]

train.index5 <- data[-data.random[121:150], ]
test.index5 <- data[data.random[121:150], ]

## Step 3:  Setup the labels for each newly created Split ##

train.labels1 <- labels[-data.random[1:30]]
test.labels1 <- labels[data.random[1:30]]

train.labels2 <- labels[-data.random[31:60]]
test.labels2 <- labels[data.random[31:60]]

train.labels3 <- labels[-data.random[61:90]]
test.labels3 <- labels[data.random[61:90]]

train.labels4 <- labels[-data.random[91:120]]
test.labels4 <- labels[data.random[91:120]]

train.labels5 <- labels[-data.random[121:150]]
test.labels5 <- labels[data.random[121:150]]

######################################
### Apply Model to each of 5 Folds ###
######################################

## Step 1: Create the 'err.rates' dataframe where the error rates of each Split will be stored ## 
err.rates1 <- data.frame()
err.rates2 <- data.frame()
err.rates3 <- data.frame()
err.rates4 <- data.frame()
err.rates5 <- data.frame()

## Step 2: Establish the number of maximum K-values to be evaluated - in this case 100 ##
max.k <- 100

## Step 3: Create a function to perform cross-validation on numerous each of the Folds, for k 1-100 ##

## Fold 1 ##
for (k in 1:max.k) {
	knn.fit1 <- knn(train.index1, 
	test.index1,
	cl = train.labels1,
	k = k
	)
	cat('\n', 'k = ', k, ', Fold - 1','\n', sep = " ")
	print(table(test.labels1, knn.fit1))

	this.err1 <- sum(test.labels1 != knn.fit1) / length(test.labels1)
	err.rates1 <- rbind(err.rates1, this.err1)
}

## Fold 2 ##
for (k in 1:max.k) {
	knn.fit2 <- knn(train.index2, 
	test.index2,
	cl = train.labels2,
	k = k
	)
	cat('\n', 'k = ', k, ', Fold - 2','\n', sep = " ")
	print(table(test.labels2, knn.fit2))

	this.err2 <- sum(test.labels2 != knn.fit2) / length(test.labels2)
	err.rates2 <- rbind(err.rates2, this.err2)
}

## Fold 3 ##
for (k in 1:max.k) {
	knn.fit3 <- knn(train.index3, 
	test.index3,
	cl = train.labels3,
	k = k
	)
	cat('\n', 'k = ', k, ', Fold - 3','\n', sep = " ")
	print(table(test.labels3, knn.fit3))

	this.err3 <- sum(test.labels3 != knn.fit3) / length(test.labels3)
	err.rates3 <- rbind(err.rates3, this.err3)
}

## Fold 4 ##
for (k in 1:max.k) {
	knn.fit4 <- knn(train.index4, 
	test.index4,
	cl = train.labels4,
	k = k
	)
	cat('\n', 'k = ', k, ', Fold - 4','\n', sep = " ")
	print(table(test.labels4, knn.fit4))

	this.err4 <- sum(test.labels4 != knn.fit4) / length(test.labels4)
	err.rates4 <- rbind(err.rates4, this.err4)
}

## Fold 5 ##
for (k in 1:max.k) {
	knn.fit5 <- knn(train.index5, 
	test.index5,
	cl = train.labels5,
	k = k
	)
	cat('\n', 'k = ', k, ', Fold - 5','\n', sep = " ")
	print(table(test.labels5, knn.fit5))

	this.err5 <- sum(test.labels5 != knn.fit5) / length(test.labels5)
	err.rates5 <- rbind(err.rates5, this.err5)
}

######################################
### Output the results to ggplot2 ####
######################################

## Create a dataframe for each fold containing a list of the K-Values and the corresponding Generalization Error Rates for each K ##
results1 <- data.frame(1:max.k, err.rates1)
results2 <- data.frame(1:max.k, err.rates2)
results3 <- data.frame(1:max.k, err.rates3)
results4 <- data.frame(1:max.k, err.rates4)
results5 <- data.frame(1:max.k, err.rates5)

## Append titles to each of columns in the results dataframe ##
names(results1) <- c('k', 'err.rate1')
names(results2) <- c('k', 'err.rate2')
names(results3) <- c('k', 'err.rate3')
names(results4) <- c('k', 'err.rate4')
names(results5) <- c('k', 'err.rate5')

## Create a title for ggplot of the error rates for each fold ##
title1 <- paste('knn results Fold 1')
title2 <- paste('knn results Fold 2')
title3 <- paste('knn results Fold 3')
title4 <- paste('knn results Fold 4')
title5 <- paste('knn results Fold 5')

## 1st, create a ggplot showing the Generalization Error Rate per K value, with point and a connecting line ##
## 2nd, insert the title of the graph into each plot ##
results.plot1 <- ggplot(results1, aes(x=k, y=err.rate1)) + geom_point() + geom_line()
results.plot1 <- results.plot1 + ggtitle(title1)

results.plot2 <- ggplot(results2, aes(x=k, y=err.rate2)) + geom_point() + geom_line()
results.plot2 <- results.plot2 + ggtitle(title2)

results.plot3 <- ggplot(results3, aes(x=k, y=err.rate3)) + geom_point() + geom_line()
results.plot3 <- results.plot3 + ggtitle(title3)

results.plot4 <- ggplot(results4, aes(x=k, y=err.rate4)) + geom_point() + geom_line()
results.plot4 <- results.plot4 + ggtitle(title4)

results.plot5 <- ggplot(results5, aes(x=k, y=err.rate5)) + geom_point() + geom_line()
results.plot5 <- results.plot5 + ggtitle(title5)

## Print out the results of  Generalization Errors for each Fold in the Cross Validation ##
print(results.plot1)
print(results.plot2)
print(results.plot3)
print(results.plot4)
print(results.plot5)

###############################################
### Average of the 5 Generalization Errors ####
###############################################

## Get the average Generalization Error Rate for each Cross Validation Fold ##
ave1 <- sum(results1$err.rate1) / length(results1$err.rate1)
ave2 <- sum(results2$err.rate2) / length(results2$err.rate2)
ave3 <- sum(results3$err.rate3) / length(results3$err.rate3)
ave4 <- sum(results4$err.rate4) / length(results4$err.rate4)
ave5 <- sum(results5$err.rate5) / length(results5$err.rate5)

## Average the Average Generalization Error Rates to determine the overall Generlaization  Error Rate for my KNN analysis ##
AveGenError <- (ave1 + ave2 + ave3 + ave4 + ave5) / 5
cat('\n', 'The Overal Average Generalization Error for this KNN Analysis is',AveGenError, '\n', sep = ' ')


