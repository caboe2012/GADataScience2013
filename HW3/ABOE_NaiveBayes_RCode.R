### HW 3 - Naive Bayes Classifier with Cross Validation (5-Fold Hard Coded) ###

## Remove all previously loaded objects/variables that may interfere with current analysis ##
rm(list=ls(all=TRUE))

######################
### Pre-processing ###
######################

## load the NaiveBayes package e0171 from the library ##
library("e1071")

## Set the "data" variable to the "iris" dataset that comes preloaded in R ##
data <- iris

################################################
## Create Training/Test Splits on the dataset ##
################################################
## Initiate the randomization by setting the seed ##
set.seed(072413)

N <- nrow(data)

randomize <- sample(1:N)

train.attributes1 <- data[-randomize[1:30],-5]
train.labels1 <- data[-randomize[1:30], 5]
test.attributes1 <- data[randomize[1:30], -5]
test.labels1 <- data[randomize[1:30], 5]

train.attributes2 <- data[-randomize[31:60],-5]
train.labels2 <- data[-randomize[31:60], 5]
test.attributes2 <- data[randomize[31:60], -5]
test.labels2 <- data[randomize[31:60], 5]

train.attributes3 <- data[-randomize[61:90],-5]
train.labels3 <- data[-randomize[61:90], 5]
test.attributes3 <- data[randomize[61:90], -5]
test.labels3 <- data[randomize[61:90], 5]

train.attributes4 <- data[-randomize[91:120],-5]
train.labels4 <- data[-randomize[91:120], 5]
test.attributes4 <- data[randomize[91:120], -5]
test.labels4 <- data[randomize[91:120], 5]

train.attributes5 <- data[-randomize[121:150],-5]
train.labels5 <- data[-randomize[121:150], 5]
test.attributes5 <- data[randomize[121:150], -5]
test.labels5 <- data[randomize[121:150], 5]

##################################
### APPLY THE NAIVEBAYES MODEL ###
##################################
##############
### Fold 1 ###
##############
### Run the NaiveBayes function on the 1st training set to determine the probablity distributions for the four attributes of each flower class ###
classifier1 <- naiveBayes(train.attributes1, train.labels1)
classifier1

### Apply the NaiveBayes Classifier distributions to the 1st training set in order to determine Fold 1 Training Error (total and rate) ###
train.results1 <- table(predict(classifier1, train.attributes1), train.labels1, dnn=list('predicted', 'actual'))
train.results1
train.predict1 <- predict(classifier1, train.attributes1)
train.err.totals1 <- sum(as.numeric(train.predict1 != train.labels1))
train.err.totals1
train.err.rate1 <- train.err.totals1 / length(train.labels1)
train.err.rate1

### Apply the NaiveBayes Classifier distributions to the 1st test set in order to determine Fold 1 Generalization Error (total and rate) ###
test.results1 <- table(predict(classifier1, test.attributes1), test.labels1, dnn=list('predicted', 'actual'))
test.results1
test.predict1 <- predict(classifier1, test.attributes1)
gen.err.totals1 <- sum(as.numeric(test.predict1 != test.labels1))
gen.err.totals1
gen.err.rate1 <- gen.err.totals1 / length(test.labels1)
gen.err.rate1

##############
### Fold 2 ###
##############
### Run the NaiveBayes function on the 2nd training set to determine the probablity distributions for the four attributes of each flower class ###
classifier2 <- naiveBayes(train.attributes2, train.labels2)
classifier2

### Apply the NaiveBayes Classifier distributions to the 2nd training set in order to determine Fold 2 Training Error (total and rate) ###
train.results2 <- table(predict(classifier2, train.attributes2), train.labels2, dnn=list('predicted', 'actual'))
train.results2
train.predict2 <- predict(classifier2, train.attributes2)
train.err.totals2 <- sum(as.numeric(train.predict2 != train.labels2))
train.err.totals2
train.err.rate2 <- train.err.totals2 / length(train.labels2)
train.err.rate2

### Apply the NaiveBayes Classifier distributions to the 2nd test set in order to determine Fold 2 Generalization Error (total and rate) ###
test.results2 <- table(predict(classifier2, test.attributes2), test.labels2, dnn=list('predicted', 'actual'))
test.results2
test.predict2 <- predict(classifier2, test.attributes2)
gen.err.totals2 <- sum(as.numeric(test.predict2 != test.labels2))
gen.err.totals2
gen.err.rate2 <- gen.err.totals2 / length(test.labels2)
gen.err.rate2

##############
### Fold 3 ###
##############
### Run the NaiveBayes function on the 3rd training set to determine the probablity distributions for the four attributes of each flower class ###
classifier3 <- naiveBayes(train.attributes3, train.labels3)
classifier3

### Apply the NaiveBayes Classifier distributions to the 3rd training set in order to determine Fold 3 Training Error (total and rate) ###
train.results3 <- table(predict(classifier3, train.attributes3), train.labels3, dnn=list('predicted', 'actual'))
train.results3
train.predict3 <- predict(classifier3, train.attributes3)
train.err.totals3 <- sum(as.numeric(train.predict3 != train.labels3))
train.err.totals3
train.err.rate3 <- train.err.totals3 / length(train.labels3)
train.err.rate3

### Apply the NaiveBayes Classifier distributions to the 3rd test set in order to determine Fold 3 Generalization Error (total and rate) ###
test.results3 <- table(predict(classifier3, test.attributes3), test.labels3, dnn=list('predicted', 'actual'))
test.results3
test.predict3 <- predict(classifier3, test.attributes3)
gen.err.totals3 <- sum(as.numeric(test.predict3 != test.labels3))
gen.err.totals3
gen.err.rate3 <- gen.err.totals3 / length(test.labels3)
gen.err.rate3

##############
### Fold 4 ###
##############
### Run the NaiveBayes function on the 3rd training set to determine the probablity distributions for the four attributes of each flower class ###
classifier4 <- naiveBayes(train.attributes4, train.labels4)
classifier4

### Apply the NaiveBayes Classifier distributions to the 3rd training set in order to determine Fold 3 Training Error (total and rate) ###
train.results4 <- table(predict(classifier4, train.attributes4), train.labels4, dnn=list('predicted', 'actual'))
train.results4
train.predict4 <- predict(classifier4, train.attributes4)
train.err.totals4 <- sum(as.numeric(train.predict4 != train.labels4))
train.err.totals4
train.err.rate4 <- train.err.totals4 / length(train.labels4)
train.err.rate4

### Apply the NaiveBayes Classifier distributions to the 3rd test set in order to determine Fold 3 Generalization Error (total and rate) ###
test.results4 <- table(predict(classifier4, test.attributes4), test.labels4, dnn=list('predicted', 'actual'))
test.results4
test.predict4 <- predict(classifier4, test.attributes4)
gen.err.totals4 <- sum(as.numeric(test.predict4 != test.labels4))
gen.err.totals4
gen.err.rate4 <- gen.err.totals4 / length(test.labels4)
gen.err.rate4

##############
### Fold 5 ###
##############
### Run the NaiveBayes function on the 3rd training set to determine the probablity distributions for the four attributes of each flower class ###
classifier5 <- naiveBayes(train.attributes5, train.labels5)
classifier5

### Apply the NaiveBayes Classifier distributions to the 3rd training set in order to determine Fold 3 Training Error (total and rate) ###
train.results5 <- table(predict(classifier5, train.attributes5), train.labels5, dnn=list('predicted', 'actual'))
train.results5
train.predict5 <- predict(classifier5, train.attributes5)
train.err.totals5 <- sum(as.numeric(train.predict5 != train.labels5))
train.err.totals5
train.err.rate5 <- train.err.totals5 / length(train.labels5)
train.err.rate5

### Apply the NaiveBayes Classifier distributions to the 3rd test set in order to determine Fold 3 Generalization Error (total and rate) ###
test.results5 <- table(predict(classifier5, test.attributes5), test.labels5, dnn=list('predicted', 'actual'))
test.results5
test.predict5 <- predict(classifier5, test.attributes5)
gen.err.totals5 <- sum(as.numeric(test.predict5 != test.labels5))
gen.err.totals5
gen.err.rate5 <- gen.err.totals5 / length(test.labels5)
gen.err.rate5


#######################################
### OUTPUT THE RESULTS ONTO A GRAPH ###
#######################################
###Fold 1 Graphs for Petal Length Distributin Probabilties ###
classifier1$tables$Petal.Length
## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
plot(function(x) dnorm(x, 1.471053, 0.1722787), 0, 8, col="red", main="Petal length distribution: Fold 1")
## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
curve(dnorm(x, 4.264286, 0.4843006), add=TRUE, col="blue")
## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
curve(dnorm(x, 5.562500, 0.5508443 ), add=TRUE, col = "green")


###Fold 2 Graphs for Petal Length Distributin Probabilties ###
classifier2$tables$Petal.Length
## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
plot(function(x) dnorm(x, 1.453333, 0.1752920), 0, 8, col="red", main="Petal length distribution: Fold 2")
## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
curve(dnorm(x, 4.281579, 0.4625752), add=TRUE, col="blue")
## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
curve(dnorm(x, 5.545946, 0.5674478), add=TRUE, col = "green")


###Fold 3 Graphs for Petal Length Distributin Probabilties ###
classifier3$tables$Petal.Length
## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
plot(function(x) dnorm(x, 1.453846, 0.1699011), 0, 8, col="red", main="Petal length distribution: Fold 3")
## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
curve(dnorm(x, 4.268421, 0.4405802), add=TRUE, col="blue")
## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
curve(dnorm(x, 5.565116, 0.5731632), add=TRUE, col = "green")


###Fold 4 Graphs for Petal Length Distributin Probabilties ###
classifier4$tables$Petal.Length
## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
plot(function(x) dnorm(x, 1.457500, 0.1692934), 0, 8, col="red", main="Petal length distribution : Fold 4")
## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
curve(dnorm(x, 4.273684, 0.5054750), add=TRUE, col="blue")
## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
curve(dnorm(x, 5.504762, 0.5516947), add=TRUE, col = "green")


###Fold 5 Graphs for Petal Length Distributin Probabilties ###
classifier5$tables$Petal.Length
## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
plot(function(x) dnorm(x, 1.476316, 0.1822297), 0, 8, col="red", main="Petal length distribution : Fold 5")
## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
curve(dnorm(x, 4.218182, 0.4576343), add=TRUE, col="blue")
## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
curve(dnorm(x, 5.584211, 0.5159752 ), add=TRUE, col = "green")


###############################################
### Average of the 5 Generalization Errors ####
###############################################
## Average the Average Generalization Error Rates to determine the overall Generlaization  Error Rate for my NaiveBayes analysis ##
AveGenError <- (gen.err.rate1 + gen.err.rate2 + gen.err.rate3 + gen.err.rate4 + gen.err.rate5) / 5
cat('\n', 'The Overal Average Generalization Error Rate for this Naive Bayes Analysis is',AveGenError, '\n', sep = ' ')



##### Remaining Attrbiutes that I can create graphs for if time is really burning a hole in my pocket #####
#classifier1$tables$Petal.Width
#classifier1$tables$Sepal.Length
#classifier1$tables$Sepal.Width

#classifier2$tables$Petal.Width
#classifier2$tables$Sepal.Length
#classifier2$tables$Sepal.Width

#classifier3$tables$Petal.Width
#classifier3$tables$Sepal.Length
#classifier3$tables$Sepal.Width

#classifier4$tables$Petal.Width
#classifier4$tables$Sepal.Length
#classifier4$tables$Sepal.Width

#classifier5$tables$Petal.Width
#classifier5$tables$Sepal.Length
#classifier5$tables$Sepal.Width






