### HW2 - FOR LOOP Function for Cross-Validation Folds #### 

rm(list=ls(all=TRUE))

library(class)
library(ggplot2)

data<-iris
labels <- data$Species
data$Species <- NULL

results.err.rates <- c()
cumulative <- c()

knn.nfold <- function(seed, n.folds, max.k) {
	set.seed(seed)
	N <- nrow(data)
	randomized <- sample(1:N)

	k.range <- (1:max.k)

	fold = 1
	
	a = (N / n.folds)
	
	b <- (1:a)

	for (each in 1:n.folds) {
		train.data <- data[-randomized[b], ]
		test.data <- data[randomized[b], ]
		
		train.labels <- as.factor(as.matrix(labels)[-randomized[b]])
		test.labels <- as.factor(as.matrix(labels)[randomized[b]])
		
		b <- b + a
		cat('\n', 'Fold ', fold,' Cross Validation Below', '\n', sep = "")
		
		for (k in k.range) {
			knn.fit <- knn(train.data,
			test.data,
			cl = train.labels,
			k = k
			)
			cat('\n', 'k = ', k, 'Fold = ',fold, '\n', sep = " ")
			print(table(test.labels, knn.fit))
			
			this.err <- sum(test.labels != knn.fit) / length(test.labels)
			results.err.rates <- append(results.err.rates, this.err)
				
			print ("the error for this k is:")
			print (this.err)
		}
		results <- data.frame(k.range, results.err.rates)
		names(results) <- c('k', 'err.rate')
		title <- paste('KNN Results Fold', fold)
		print ("And now I WON'T hang!  Wootwoot!")
		results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_point() + geom_line()
		results.plot <- results.plot + ggtitle(title)
		print(results.plot)
		cat('\n', "Done with fold ", fold, " analysis", '\n', sep="")
		print (" ")
		
		print (results)
		print (results.err.rates)
		cumulative <- append(cumulative, results.err.rates)
		results.err.rates <- c()
		print (results.err.rates)
		fold <- fold + 1

	}
	fold <- fold - 1
	print (sum(cumulative))
	ave.out <- sum(cumulative) / (n.folds * max.k)
	cat('\n', "The average Generalization Error Rate for this ", fold, "-Fold kNN analysis is ", ave.out, '\n', sep="")
	return (ave.out)
	
}

knn.nfold(072413, 5, 100)