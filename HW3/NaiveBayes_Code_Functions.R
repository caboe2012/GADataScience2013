### HW 3 - Naive Bayes Classifier with Cross Validation (5-Fold Hard Coded) ###

######################
### Pre-processing ###
######################

## load the NaiveBayes package e0171 from the library ##
library("e1071")

## Set the "data" variable to the "iris" dataset that comes preloaded in R ##
data <- iris

cumulative.train.err.totals <- c()
cumulative.train.err.rate <- c()

cumulative.gen.err.totals <- c()
cumulative.gen.err.rate <- c()

################################################
## Create Training/Test Splits on the dataset ##
################################################
## Initiate the randomization by setting the seed ##
NaiveBayes.nfold <- function(seed, n.folds, labels.col) {
	
	set.seed(seed)
	N <- nrow(data)
	randomize <- sample(1:N)

	incrementby <- (N / n.folds)
	
	data.range <- (1:incrementby)

	fold <- 1

	for (each in 1:n.folds) {
		train.attributes <- data[-randomize[data.range],-labels.col]
		train.labels <- data[-randomize[data.range], labels.col]
		test.attributes <- data[randomize[data.range], -labels.col]
		test.labels <- data[randomize[data.range], labels.col]
		
		data.range <- data.range + incrementby
		cat('\n', 'Fold ', fold,' Cross Validation Below', '\n', sep = "")

		classifier <- naiveBayes(train.attributes, train.labels)
		train.results <- table(predict(classifier, train.attributes), train.labels, dnn=list('predicted', 'actual'))
		cat('\n', 'Training Fold ', fold,' Results', '\n', sep = "")
		train.results
		train.predict <- predict(classifier, train.attributes)
		train.err.totals <- sum(as.numeric(train.predict != train.labels))
		cat('\n',  'Fold ', fold,' Total Number of Training Errors', '\n', sep = "")
		train.err.totals
		train.err.rate <- train.err.totals / length(train.labels)
		cat('\n',  'Fold ', fold,' Training Error Rate', '\n', sep = "")
		train.err.rate
		cumulative.train.err.totals <- append(cumulative.train.err.totals, train.err.totals)
		cumulative.train.err.rate <- append(cumulative.train.err.rate, train.err.rate)
		print (cumulative.train.err.totals)
		print (cumulative.train.err.rate)

		test.results <- table(predict(classifier, test.attributes), test.labels, dnn=list('predicted', 'actual'))
		cat('\n',  'Fold ', fold,' Generalization Error Rate', '\n', sep = "")
		print (test.results)
		test.predict <- predict(classifier, test.attributes)
		gen.err.totals <- sum(as.numeric(test.predict != test.labels))
		cat('\n',  'Fold ', fold,' Total Number of Generalization Errors', '\n', sep = "")
		print (gen.err.totals)
		gen.err.rate <- gen.err.totals / length(test.labels)
		cat('\n',  'Fold ', fold,' Generalization Error Rate', '\n', sep = "")
		print (gen.err.rate)
		cumulative.gen.err.totals <- append(cumulative.gen.err.totals, gen.err.totals)
		cumulative.gen.err.rate <- append(cumulative.gen.err.rate, gen.err.rate)
		print (cumulative.gen.err.totals)
		print (cumulative.gen.err.rate)

		###Graphs for Petal Length Distributin Probabilties ###
		print (classifier$tables$Petal.Length)
		## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
		plot(function(x) dnorm(x, classifier$tables$Petal.Length[[1,1]], classifier$tables$Petal.Length[[1,2]]), 0, 8, col="red", main="Petal length Distributions")
		## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
		curve(dnorm(x, classifier$tables$Petal.Length[[2,1]], classifier$tables$Petal.Length[[2,2]]), add=TRUE, col="blue")
		## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
		curve(dnorm(x, classifier$tables$Petal.Length[[3,1]], classifier$tables$Petal.Length[[3,2]]), add=TRUE, col = "green")

		###Graphs for Sepal Length Distributin Probabilties ###
		print (classifier$tables$Sepal.Length)
		## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
		plot(function(x) dnorm(x, classifier$tables$Sepal.Length[[1,1]], classifier$tables$Sepal.Length[[1,2]]), 0, 8, col="red", main="Sepal length Distributions")
		## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
		curve(dnorm(x, classifier$tables$Sepal.Length[[2,1]], classifier$tables$Sepal.Length[[2,2]]), add=TRUE, col="blue")
		## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
		curve(dnorm(x, classifier$tables$Sepal.Length[[3,1]], classifier$tables$Sepal.Length[[3,2]]), add=TRUE, col = "green")

		###Graphs for Sepal Width Distributin Probabilties ###
		print (classifier$tables$Sepal.Width)
		## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
		plot(function(x) dnorm(x, classifier$tables$Sepal.Width[[1,1]], classifier$tables$Sepal.Width[[1,2]]), 0, 8, col="red", main="Sepal Width Distributions")
		## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
		curve(dnorm(x, classifier$tables$Sepal.Width[[2,1]], classifier$tables$Sepal.Width[[2,2]]), add=TRUE, col="blue")
		## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
		curve(dnorm(x, classifier$tables$Sepal.Width[[3,1]], classifier$tables$Sepal.Width[[3,2]]), add=TRUE, col = "green")		

		###Graphs for Petal Width Distributin Probabilties ###
		print (classifier$tables$Petal.Width)
		## 1st, create the plot for the distribution probabilty range for the Petal Length attribute of the Setosa plants ##
		plot(function(x) dnorm(x, classifier$tables$Petal.Width[[1,1]], classifier$tables$Petal.Width[[1,2]]), 0, 8, col="red", main="Petal Width Distributions")
		## Next, create the plot for the distribution probabilty range for the Petal Length attribute of the Versicolor plants ##
		curve(dnorm(x, classifier$tables$Petal.Width[[2,1]], classifier$tables$Petal.Width[[2,2]]), add=TRUE, col="blue")
		## Finally create the plot for the distribution probabilty range for the Petal Length attribute of the Virginica plants ##
		curve(dnorm(x, classifier$tables$Petal.Width[[3,1]], classifier$tables$Petal.Width[[3,2]]), add=TRUE, col = "green")		

		fold <- fold + 1

	}	

	a <- sum(cumulative.gen.err.rate)
	print (a)
	
	AveGenError <- (a / n.folds)
	cat('\n', 'The Overal Average Generalization Error Rate for this Naive Bayes Analysis is',AveGenError, '\n', sep = ' ')
}