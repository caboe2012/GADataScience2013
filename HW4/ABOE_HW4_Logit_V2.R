#source -- http://www.umass.edu/statdata/statdata/stat-survival.html ; data = whas500.dat

## Load the necessary libraries in order to plot the Survival Analysis charts ##
library(survival)
library(KMsurv)
# Load the modified data set as described in analysis outline #
data <- read.table("heart_attack_mod1.txt", h=T)

# Display the first 6 rows in the data set in order to verfiy properly imported ##
head(data)
# Create two emptey vectors that will be used to store the overall analysis resutls for both Total Errors
# and the Overall Error Rate this correpsonds to
results.err.total <- c()
cumulative.err.rates <- c()

## Create the Train/Test Split ##
logit.nfold <- function(seed, n.folds) {
	set.seed(seed) #Allow the user to set the seed 
	N <- nrow(data) # Create a variable to hold the number of rows in the dataset, in this case 460
	randomized <- sample(1:N) # Create a randomized range of numbers equal to the number of rows in the dataset
	randomized #Verify randomized range properly created and in order to verify seed properly set

	fold <- 1 # Create the variable to be used in tracking the fold analyses
	range <- (N / n.folds)  # Set the range equal to the number of dataset rows divivded by the number of folds
	increment.by <- (1:range) # Increment the folds by the range in the test splits

	time.train0 <- rep(0, N - range) # Create a vector of 0s equal to the length of the train split data set
	time.test0 <- rep(0, range) # Create a vector of 0s equal to the length of the test split data set
								# These will be used in the function to plot the survival probabilty over time
	# Iterate over the Train/Test splits for the Logistic Analysis 
	for (each in 1:n.folds) {
		train.data <- data[-randomized[increment.by], ]
		test.data <- data[randomized[increment.by], ]

		print (head(train.data))
		print (head(test.data))

		cat('\n', 'Fold ', fold,' Cross Validation Below', '\n', sep = "")

# Create a model where survival is a function of age, body mass index, congestive heart failure,
# wether this was the patients first heart attack (MIORD) where 0 = first, 1 = not first, and length of follow-up
# since the time of the most recent heart attack 
		logit.fit <- glm(fstat ~ age + bmi + chf + miord + lenfol, family='binomial', data=train.data)
		cat('\n', 'Summary of fold ', fold,' Training Analysis Below', '\n', sep = "")
# Pass the Survival Fit function the start time of 0, the duration equal to the patient's Length of Follow-Up, and set 
# the event as equal to the patients mortality  (0=life, 1=death)
		my.train.fit <- survfit(Surv(time.train0, train.data$lenfol, train.data$fstat) ~ 1)
		print (summary(logit.fit))
		xaxis <- "Length of Follow-Up (in days)"
		yaxis <-  "Surival Probability"
# PLot the Survival Probabiliyt of the patients as a function of time 		
		train.plot <- plot(my.train.fit, xlab = xaxis , ylab= yaxis, main="Train Fold Survival Analysis")
		train.plot

## Predict out of sample data error using the Generaliztion error as proxy ##
## Create a new data frame comprised of the test data columns listed below ##
		new.data <- with(test.data, data.frame(age=age, bmi=bmi, chf=chf, miord=miord, lenfol=lenfol))
		cat('\n', 'Fold ', fold,' Test Data Frame', '\n', sep = "")
		print (new.data)
## Run the Trained Model against the new test data and add column to the new.data frame
		new.data$mort.prob <- predict(logit.fit, newdata=new.data, type='response')
## Round the predictions to the nearest whole 0,1 and add to the new.data frame
		new.data$mort.round <- round(new.data$mort.prob)
# Juxtapose the test data sets known survival statistics in order to compare to the model's predictions
		new.data$actual <- test.data$fstat
		cat('\n', 'Fold ', fold,' Test Data Set Cross Validation Below', '\n', sep = "")
		print(new.data)
		cat('\n', 'Fold ', fold,' Confusion Matrix of Predicted versus Acutal Mortality', '\n', sep = "")
# Create a confusion matrix that more effeciently displays the (in)accuracy of the model's predictions
		xtable <- xtabs(~new.data$actual + new.data$mort.round, data=new.data)
		print (xtable)
# Pass the Survival Fit function the start time of 0, the duration equal to the patient's Length of Follow-Up, and set 
# the event as equal to the patients mortality  (0=life, 1=death)
		my.test.fit <- survfit(Surv(time.test0, new.data$lenfol, new.data$mort.round) ~ 1)
		print (summary(logit.fit))
		xaxis <- "Length of Follow-Up (in days)"
		yaxis <-  "Surival Probability"
# PLot the Survival Probabiliyt of the patients as a function of time 		
		test.plot <- plot(my.test.fit, xlab = xaxis , ylab= yaxis, main="Test Fold Survival Analysis")
		test.plot
# Calculate the Total Numer of Errors for the Test Fold ##
		gen.err.total <- sum(as.numeric(new.data$actual != new.data$mort.round))
		cat('\n', "The Total number of Generaliztion Errors for fold ",fold,' is:', '\n', sep = "")
		print(gen.err.total)
# Store this data in a vector for further analysis once the function has completed
		results.err.total <- append(results.err.total, gen.err.total)
# Convert the Number of Errors to the Error Rate for the Test Fold 
		gen.err.rate <- gen.err.total / length(new.data$actual)
		cat('\n', "the total Generalization Error Rate for fold ",fold,' is:',  '\n', sep = "")
		print (gen.err.rate)
# Store this data in a vector for further analysis once the function has completed
		cumulative.err.rates <- append(cumulative.err.rates, gen.err.rate)
		cumulative.err.rates
# Increment the range in order to complete the following TRAIN/TEST analysis
		increment.by <- increment.by + range
		fold <- fold + 1 
		#ggplot(new.data, aes(x=age, y=actual)) + geom_point()
		#ggplot(new.data, aes(x=age, y=a) + geom_line() 		
	}
# Print out the Generalization Error Totals, Rates and Average Generalization Error Rate ##
	cat('\n', "the Total Number of Generalization Errors for this analysis is:",  '\n', sep = "")
	total.num.err <- sum(results.err.total)
	print (total.num.err)
	cat('\n', "Below are the Generaliztion Errors for each analysis:",  '\n', sep = "")
	print (cumulative.err.rates)
	cat('\n', "the Overall Generalization Error Rate for this analyis is:",  '\n', sep = "")
	total.err.rate <- sum(cumulative.err.rates) / length(cumulative.err.rates)
	print (total.err.rate)
}

logit.nfold(08112013, 10)

