library(ggplot2)

data <- read.csv("train.csv", h=T)
kaggle <- read.csv("test.csv", h=T)

head(data)
# Create the vectors that wil hold the cumulative Gen Error info ##
results.err.total <- c()
cumulative.err.rates <- c()

## Convert the Pclass and Parch to factors ##
data$Pclass <- as.factor(data$Pclass)
data$Parch <- as.factor(data$Parch)
kaggle$Pclass <- as.factor(kaggle$Pclass)
kaggle$Parch <- as.factor(kaggle$Parch)

kaggle$Parch <- replace(kaggle$Parch, kaggle$Parch == 9, 6)

# Replace NA's with Zeros #
train.data$Age[is.na(train.data$Age)] <- 0
test.data$Age[is.na(test.data$Age)] <- 0

# Obtain average age per train fold and impute for NA's ##
cumulative.train.age <- sum(train.data$Age)
yes.train.age <- sum(train.data$Age != 0)
ave.train.age <- cumulative.train.age / yes.train.age
train.data$Age <- replace(train.data$Age, train.data$Age == 0, ave.train.age)

#Obtain average age per test fold and impute for NA's
cumulative.test.age <- sum(test.data$Age)
yes.test.age <- sum(test.data$Age != 0)
ave.test.age <- cumulative.test.age / yes.test.age
test.data$Age <- replace(test.data$Age, test.data$Age == 0, ave.test.age)

#Obtain average age for the Kaggle OOS csv and  impute for NA
kaggle$Age[is.na(kaggle$Age)] <- 0
cumulative.kaggle.age <- sum(kaggle$Age)
yes.kaggle.age <- sum(kaggle$Age != 0)
ave.kaggle.age <- cumulative.kaggle.age / yes.kaggle.age
tereplace(kaggle$Age, kaggle$Age == 0, ave.kaggle.age)

## Create the Train/Test Split ##
logit.nfold <- function(seed, n.folds) {
	# Set the randomization of the training data #
	set.seed(seed)
	N <- nrow(data)
	randomized <- sample(1:N)
	randomized
	# Determine the 
	fold <- 1
	range <- (N / n.folds)
	increment.by <- (1:range)

	for (each in 1:n.folds) {
		# Create the Train/Test Split ##
		train.data <- data[-randomized[increment.by], ]
		test.data <- data[randomized[increment.by], ]
		
		## Inform th user that the Cross Validation is commencing ##
		cat('\n', 'Fold ', fold,' Cross Validation Below', '\n', sep = "")

# Train the Model ##
		logit.fit <- glm(Survived ~ Sex + Pclass + Parch, family='binomial', data=train.data)
		cat('\n', 'Summary of fold ', fold,' Training Analysis Below', '\n', sep = "")
		print (summary(logit.fit))

## Determine the Generaliztion error for my model by applying the Test Fold ##
		new.data <- with(test.data, data.frame(Sex=Sex, Pclass=Pclass, Parch=Parch))
		new.data$Parch
		cat('\n', 'Fold ', fold,' Test Data Frame', '\n', sep = "")
		print (new.data)

		new.data$death.predict.prob <- predict(logit.fit, newdata=new.data, type='response')
		new.data$death.predict <- round(new.data$death.predict.prob)
		new.data$death.actual <- test.data$Survived
		cat('\n', 'Fold-', fold,' Test CrossValidation Below', '\n', sep = "")
		print(new.data)

		cat('\n', 'Fold-', fold,' Confusion Matrix of Predicted versus Acutal Mortality', '\n', sep = "")
		xtable <- xtabs(~new.data$death.actual + new.data$death.predict,  data=new.data)
		print (xtable)

		gen.err.total <- sum(as.numeric(new.data$death.actual != new.data$death.predict))
		cat('\n', "The Total number of Generaliztion Errors for fold ",fold,' is:', '\n', sep = "")
		print(gen.err.total)
		results.err.total <- append(results.err.total, gen.err.total)

		gen.err.rate <- gen.err.total / length(new.data$death.actual)
		cat('\n', "the total Generalization Error Rate for fold ",fold,' is:',  '\n', sep = "")
		print (gen.err.rate)
		cumulative.err.rates <- append(cumulative.err.rates, gen.err.rate)
		cumulative.err.rates

		## Test my model against the TEST CSV file ##
		kaggle.data <- with(kaggle, data.frame(Sex=Sex, Pclass=Pclass, Parch=Parch))
		cat('\n', 'Fold ', fold,' Kaggle Predictions', '\n', sep = "")
		result <- predict(logit.fit, newdata=kaggle.data, type='response')

		kaggle.submit <- with(kaggle, data.frame(PassengerId = PassengerId))
		kaggle.submit$Survived <- round(result)
		print (kaggle.submit)
		write.csv(kaggle.submit, file ="C:\\Caboe_Sex_PClass_Parch.csv", row.names=FALSE)
		increment.by <- increment.by + range
		fold <- fold + 1 
			
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

logit.nfold(080813, 9)
