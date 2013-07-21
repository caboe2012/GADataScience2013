### Naive Bayes Classifier ###

## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")

## Set variable df to the iris dataset
df <- iris

## Create the variable Classifier to store the result of the naiveBayes classification on df ##
classifier<-naiveBayes(df[,-5], df[,5])

## Create a confustion matrix that shows the results of predicted vs actual flower labels based on NB classifier
a <- table(predict(classifier, df[,-5]), df[,-5], dnn=list('predicted', 'actual'))

######################################################################################
#### Plot the results of the NaiveBayes for each attribute on 4 different graphs #####
######################################################################################
## Display the raw quantities in the df for the three different Classes : Setosa, Versicolor and Virginica ##
classifier$apriori

## Petal Length Graph ##
## Call up the results of the naiveBayes function for the Petal Length attribute to find the values to plug into the graph ##
classifier$tables$Petal.Length

## Create a Normalized Distribution plot for the Petal Length Distribution of the 3 diff species ##

## 1st, create the plot for the Setosa plants ##
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")

## 2nd, add a curve for the Versicolor ##
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")

## Finally, add a curve for the Virginica ##
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

## Petal Width Graph ##
## Call up the results of the naiveBayes function for the Petal Width attribute to find the values to plug into the graph ##
classifier$tables$Petal.Width

## Create a Normalized Distribution plot for the Petal Width Distribution of the 3 diff species ##

## 1st, create the plot for the Setosa plants ##
plot(function(x) dnorm(x, 0.246, 0.1053856), 0, 8, col="red", main="Petal Width distribution for the 3 different species")

## 2nd, add a curve for the Versicolor ##
curve(dnorm(x, 1.326, 0.1977527), add=TRUE, col="blue")

## Finally, add a curve for the Virginica ##
curve(dnorm(x, 2.026, 0.2746501), add=TRUE, col = "green")

## Sepal Length Graph ##
## Call up the results of the naiveBayes function for the Sepal Length attribute to find the values to plug into the graph ##
classifier$tables$Sepal.Length

## Create a Normalized Distribution plot for the Sepal Length Distribution of the 3 diff species ##

## 1st, create the plot for the Setosa plants ##
plot(function(x) dnorm(x, 5.006, 0.3524897), 0, 8, col="red", main="Sepal length distribution for the 3 different species")

## 2nd, add a curve for the Versicolor ##
curve(dnorm(x, 5.936, 0.5161711), add=TRUE, col="blue")

## Finally, add a curve for the Virginica ##
curve(dnorm(x, 6.588, 0.6358796), add=TRUE, col = "green")

## Petal Length Graph ##
## Call up the results of the naiveBayes function for the Sepal width attribute to find the values to plug into the graph ##
classifier$tables$Sepal.Width

## Create a Normalized Distribution plot for the Sepal Width Distribution of the 3 diff species ##

## 1st, create the plot for the Setosa plants ##
plot(function(x) dnorm(x, 3.428, 0.3790644), 0, 8, col="red", main="Sepal Width distribution for the 3 different species")

## 2nd, add a curve for the Versicolor ##
curve(dnorm(x, 2.770, 0.3137983), add=TRUE, col="blue")

## Finally, add a curve for the Virginica ##
curve(dnorm(x, 2.974, 0.3224966), add=TRUE, col = "green")