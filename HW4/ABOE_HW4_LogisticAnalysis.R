#source -- http://www.umass.edu/statdata/statdata/stat-survival.html
library(survival)
library(KMsurv)
library(ggplot2)

data <- read.table("heart_attack_mod1.txt", h=T)

head(data)
results.err.total <- c()
cumulative.err.rates <- c()

## Create the Train/Test Split ##
logit.nfold <- function(seed, n.folds) {
	set.seed(seed)
	N <- nrow(data)
	randomized <- sample(1:N)
	randomized

	fold <- 1
	
	range <- (N / n.folds)
	#if range % 2 != 0 || if range % 3 != 0:
	#range <- round(range)
	increment.by <- (1:range)

	for (each in 1:n.folds) {
		train.data <- data[-randomized[increment.by], ]
		test.data <- data[randomized[increment.by], ]

		print (head(train.data))
		print (head(test.data))

		cat('\n', 'Fold ', fold,' Cross Validation Below', '\n', sep = "")

# Test the Model 
		logit.fit <- glm(fstat ~ age + bmi + chf + miord + lenfol, family='binomial', data=train.data)
		cat('\n', 'Summary of fold ', fold,' Training Analysis Below', '\n', sep = "")
		print (summary(logit.fit))

## Predict out of sample data error using the Generaliztion error as proxy ##
		new.data <- with(test.data, data.frame(age=age, bmi=bmi, chf=chf, miord=miord, lenfol=lenfol))
		cat('\n', 'Fold ', fold,' Test Data Frame', '\n', sep = "")
		print (new.data)

		new.data$mort.prob <- predict(logit.fit, newdata=new.data, type='response')
		new.data$mort.round <- round(new.data$mort.prob)
		new.data$actual <- test.data$fstat
		cat('\n', 'Fold-', fold,' Test CrossValidation Below', '\n', sep = "")
		print(new.data)

		cat('\n', 'Fold-', fold,' Confusion Matrix of Predicted versus Acutal Mortality', '\n', sep = "")
		xtable <- xtabs(~new.data$actual + new.data$mort.round, data=new.data)
		print (xtable)

		gen.err.total <- sum(as.numeric(new.data$actual != new.data$mort.round))
		cat('\n', "The Total number of Generaliztion Errors for fold ",fold,' is:', '\n', sep = "")
		print(gen.err.total)
		results.err.total <- append(results.err.total, gen.err.total)

		gen.err.rate <- gen.err.total / length(new.data$actual)
		cat('\n', "the total Generalization Error Rate for fold ",fold,' is:',  '\n', sep = "")
		print (gen.err.rate)
		cumulative.err.rates <- append(cumulative.err.rates, gen.err.rate)
		cumulative.err.rates

		increment.by <- increment.by + range
		fold <- fold + 1 
		#ggplot(new.data, aes(x=age, y=actual)) + geom_point()
		#ggplot(new.data, aes(x=age, y=a) + geom_line() 		
	}
	cat('\n', "the Total Generaliztion Errors for this analysis is:",  '\n', sep = "")
	total.num.err <- sum(results.err.total)
	print (total.num.err)
	cat('\n', "Below are the Generaliztion Errors for each analysis:",  '\n', sep = "")
	print (cumulative.err.rates)
	cat('\n', "the Overall Generalization Error Rate for this analyis is:",  '\n', sep = "")
	total.err.rate <- sum(cumulative.err.rates) / length(cumulative.err.rates)
	print (total.err.rate)
}

logit.nfold(080513, 10)

logit.fit <- glm(fstat ~ age + gender + bmi + cvd + sho + chf + miord + lenfol, data =x)
summary(logit.fit)

logit.fit2 <- update(logit.fit, .~. -gender)
summary(lin.fit2)

logit.fit3 <- update(logit.fit2, .~. -cvd)
summary(lin.fit3)

logit.fit4 <- update(logit.fit3, .~. -sho)
summary(lin.fit4)

plot(resid(logit.fit4))
qqnorm(resid(logit.fit4))