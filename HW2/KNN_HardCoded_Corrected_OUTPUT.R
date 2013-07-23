Last login: Mon Jul 22 23:48:58 on ttys002
Chads-MacBook-Air:~ caboe$ cd r_work
Chads-MacBook-Air:r_work caboe$ r

R version 3.0.1 (2013-05-16) -- "Good Sport"
Copyright (C) 2013 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin10.8.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> ## KNN - Homework 2: Cross Validation with 5 Hard-Coded Folds ## 
> 
> ##########################################
> ### Preprocessing for the KNN Function ###
> ##########################################
> 
> ## Call Necessary Libray Packages ###
> library(class)
> library(ggplot2)
> 
> ### Establish Variables ###
> data<-iris
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
test.index1 <- data[data.random[1:30] ,]

train.index2 <- data[-data.random[31:60], ]
test.index2 <- data[data.random[31:60], ]

train.index3 <- data[-data.random[61:90], ]
test.index3 <- data[data.random[61:90], ]

train.index4 <- data[-data.random[91:120], ]
test.index4 <- data[data.random[91:120], ]

train.index5 <- data[-data.random[121:150], ]
test.index5 <- data[data.random[121:150> labels <- data$Species
> data$Species <- NULL
> 
> #####################################
> ### 5 Fold Cross-Validation Split ###
> #####################################
> 
> ## Step 1: Randomize the 'Data' dataframe ##
> set.seed(01061979)# Initiate reproducible randomization(in this case with my DOB)
> 
> N <- nrow(data)# N is equal to the total number of rows in data
> data.random <- sample(1:N)# Creates a Vector of random numbers (as determined by seed) from 1 to the number of rows in data
> 
> 
> ## Step 2:  Split the Randomized 'Data' dataframe into 5-folds ##
> 
> train.index1 <- data[-data.random[1:30], ]
> test.index1 <- data[data.random[1:30] ,]
> 
> train.index2 <- data[-data.random[31:60], ]
> test.index2 <- data[data.random[31:60], ]
> 
> train.index3 <- data[-data.random[61:90], ]
> test.index3 <- data[data.random[61:90], ]
> 
> train.index4 <- data[-data.random[91:120], ]
> test.index4 <- data[data.random[91:120], ]
> 
> train.index5 <- data[-data.random[121:150], ]
> test.index5 <- data[data.random[121:150], ]
> 
> ## Step 3:  Setup the labels for each newly created Split ##
> 
> train.labels1 <- labels[-data.random[1:30]]
> test.labels1 <- labels[data.random[1:30]]
> 
> train.labels2 <- labels[-data.random[31:60]]
> test.labels2 <- labels[data.random[31:60]]
> 
> train.labels3 <- labels[-data.random[61:90]]
> test.labels3 <- labels[data.random[61:90]]
> 
> train.labels4 <- labels[-data.random[91:120]]
> test.labels4 <- labels[data.random[91:120]]
> 
> train.labels5 <- labels[-data.random[121:150]]
> test.labels5 <- labels[data.random[121:150]]
> 
> ######################################
> ### Apply Model to each of 5 Folds ###
> ######################################
> 
> ## Step 1: Create the 'err.rates' dataframe where the error rates of each Split will be stored ## 
> err.rates1 <- data.frame()
> err.rates2 <- data.frame()
> err.rates3 <- data.frame()
> err.rates4 <- data.frame()
> err.rates5 <- data.frame()
> 
> ## Step 2: Establish the number of maximum K-values to be evaluated - in this case 100 ##
> max.k <- 100
> 
> ## Step 3: Create a function to perform cross-validation on numerous each of the Folds, for k 1-100 ##
> 
> ## Fold 1 ##
> for (k in 1:max.k) {
+ knn.fit1 <- knn(train.index1, 
+ test.index1,
+ cl = train.labels1,
+ k = k
+ )
+ cat('\n', 'k = ', k, ', Fold - 1','\n', sep = " ")
+ print(table(test.labels1, knn.fit1))
+ 
+ this.err1 <- sum(test.labels1 != knn.fit1) / length(test.labels1)
+ err.rates1 <- rbind(err.rates1, this.err1)
+ }

 k =  1 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  2 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  3 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  4 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  5 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  6 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  7 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  8 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  9 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  10 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  11 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  12 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  13 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  14 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  15 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  16 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  17 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  18 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  19 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  20 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  21 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  22 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  23 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  24 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  25 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  26 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  27 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  28 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  29 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

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
for (k in 1:max.k)
 k =  30 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  31 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  32 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  33 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  34 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  35 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  36 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  37 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  38 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          0         9

 k =  39 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  40 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  41 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  42 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  43 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  44 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  45 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  46 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  47 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  48 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  49 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  50 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          0         9

 k =  51 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  52 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  53 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  54 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  55 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  56 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  57 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  58 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  59 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  60 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  61 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  62 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  63 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  64 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  65 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  66 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  67 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          1         8

 k =  68 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  69 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  70 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          1         8

 k =  71 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  72 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         0
  virginica       0          1         8

 k =  73 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  74 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  75 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  76 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  77 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  78 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  79 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa         11          1         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  80 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          5          7         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  81 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          2         10         0
  versicolor      0          8         1
  virginica       0          1         8

 k =  82 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          1         11         0
  versicolor      0          6         3
  virginica       0          4         5

 k =  83 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          7         2

 k =  84 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          1         11         0
  versicolor      0          7         2
  virginica       0          6         3

 k =  85 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          4         5

 k =  86 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          1         11         0
  versicolor      0          7         2
  virginica       0          5         4

 k =  87 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          5         4

 k =  88 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          6         3

 k =  89 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          4         5

 k =  90 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          5         4

 k =  91 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          5         4

 k =  92 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          4         5

 k =  93 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          9         0
  virginica       0          5         4

 k =  94 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          3         6

 k =  95 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          5         4

 k =  96 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          4         5

 k =  97 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          8         1
  virginica       0          4         5

 k =  98 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          6         3
  virginica       0          3         6

 k =  99 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          6         3

 k =  100 , Fold - 1 
            knn.fit1
test.labels1 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          7         2
  virginica       0          6         3
> 
> ## Fold 2 ##
> for (k in 1:max.k) {
+ knn.fit2 <- knn(train.index2, 
+ test.index2,
+ cl = train.labels2,
+ k = k
+ )
+ cat('\n', 'k = ', k, ', Fold - 2','\n', sep = " ")
+ print(table(test.labels2, knn.fit2))
+ 
+ this.err2 <- sum(test.labels2 != knn.fit2) / length(test.labels2)
+ err.rates2 <- rbind(err.rates2, this.err2)
+ }

 k =  1 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  2 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  3 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  4 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  5 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  6 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  7 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  8 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  9 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  10 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  11 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  12 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  13 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  14 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  15 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  16 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  17 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  18 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  19 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  20 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  21 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  22 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  23 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  24 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  25 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  26 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  27 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  28 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  29 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  30 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  31 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  32 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  33 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  34 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  35 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  36 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  37 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  38 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

 k =  39 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         15         0
  virginica       0          1         4

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

################################
 k =  40 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  41 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  42 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  43 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  44 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  45 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  46 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  47 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  48 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  49 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  50 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  51 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  52 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  53 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  54 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  55 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  56 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  57 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  58 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  59 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  60 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  61 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  62 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  63 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         14         1
  virginica       0          1         4

 k =  64 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         12         3
  virginica       0          1         4

 k =  65 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         10         5
  virginica       0          1         4

 k =  66 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         10         5
  virginica       0          1         4

 k =  67 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         10         5
  virginica       0          1         4

 k =  68 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         10         5
  virginica       0          1         4

 k =  69 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0         10         5
  virginica       0          1         4

 k =  70 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          7         8
  virginica       0          1         4

 k =  71 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          2        13
  virginica       0          0         5

 k =  72 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          2        13
  virginica       0          0         5

 k =  73 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          2        13
  virginica       0          0         5

 k =  74 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  75 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  76 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  77 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  78 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  79 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  80 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  81 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  82 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  83 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  84 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  85 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  86 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  87 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  88 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  89 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  90 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  91 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  92 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  93 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  94 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  95 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  96 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  97 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  98 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  99 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      1          1        13
  virginica       0          0         5

 k =  100 , Fold - 2 
            knn.fit2
test.labels2 setosa versicolor virginica
  setosa         10          0         0
  versicolor      2          0        13
  virginica       0          0         5
> 
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

##################################> ## Fold 3 ##
> for (k in 1:max.k) {
+ knn.fit3 <- knn(train.index3, 
+ test.index3,
+ cl = train.labels3,
+ k = k
+ )
+ cat('\n', 'k = ', k, ', Fold - 3','\n', sep = " ")
+ print(table(test.labels3, knn.fit3))
+ 
+ this.err3 <- sum(test.labels3 != knn.fit3) / length(test.labels3)
+ err.rates3 <- rbind(err.rates3, this.err3)
+ }

 k =  1 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         10         2
  virginica       0          0        12

 k =  2 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         10         2
  virginica       0          0        12

 k =  3 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         10         2
  virginica       0          0        12

 k =  4 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         10         2
  virginica       0          1        11

 k =  5 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          1        11

 k =  6 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          1        11

 k =  7 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          1        11

 k =  8 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          0        12

 k =  9 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          1        11

 k =  10 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          1        11

 k =  11 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          2        10

 k =  12 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  13 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          2        10

 k =  14 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          2        10

 k =  15 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  16 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  17 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  18 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  19 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  20 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  21 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  22 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  23 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  24 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          3         9

 k =  25 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  26 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  27 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  28 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  29 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  30 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  31 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  32 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  33 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  34 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  35 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          3         9

 k =  36 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  37 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  38 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  39 , Fold - 3 

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
results3 <- data            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  40 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  41 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  42 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  43 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  44 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          4         8

 k =  45 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  46 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          5         7

 k =  47 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          5         7

 k =  48 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          5         7

 k =  49 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          5         7

 k =  50 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          5         7

 k =  51 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          5         7

 k =  52 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  53 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          4         8

 k =  54 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         12         0
  virginica       0          5         7

 k =  55 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      0         11         1
  virginica       0          5         7

 k =  56 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  57 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  58 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  59 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  60 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  61 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  62 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  63 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  64 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  65 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  66 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  67 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  68 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  69 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          5         7

 k =  70 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  71 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  72 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         11         0
  virginica       0          6         6

 k =  73 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         10         1
  virginica       0          5         7

 k =  74 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         10         1
  virginica       0          5         7

 k =  75 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1         10         1
  virginica       0          5         7

 k =  76 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          9         2
  virginica       0          2        10

 k =  77 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          7         4
  virginica       0          8         4

 k =  78 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          8         3
  virginica       0          5         7

 k =  79 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          8         3
  virginica       0          5         7

 k =  80 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          8         3
  virginica       0          5         7

 k =  81 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          7         4
  virginica       0          5         7

 k =  82 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          3         9

 k =  83 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          5         7

 k =  84 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          5         5
  virginica       0          5         7

 k =  85 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          9         1
  virginica       0          3         9

 k =  86 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      1          7         4
  virginica       0          6         6

 k =  87 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          7         3
  virginica       0          6         6

 k =  88 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          8         2
  virginica       0          4         8

 k =  89 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          5         5
  virginica       0          6         6

 k =  90 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          7         3
  virginica       0          4         8

 k =  91 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          7         3
  virginica       0          7         5

 k =  92 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          5         7

 k =  93 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          6         6

 k =  94 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          6         6

 k =  95 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          7         3
  virginica       0          5         7

 k =  96 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          6         6

 k =  97 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          7         3
  virginica       0          9         3

 k =  98 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          9         1
  virginica       0          5         7

 k =  99 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          8         2
  virginica       0          6         6

 k =  100 , Fold - 3 
            knn.fit3
test.labels3 setosa versicolor virginica
  setosa          6          0         0
  versicolor      2          6         4
  virginica       0          5         7
> 
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
results3 <- data.f> ## Fold 4 ##
> for (k in 1:max.k) {
+ knn.fit4 <- knn(train.index4, 
+ test.index4,
+ cl = train.labels4,
+ k = k
+ )
+ cat('\n', 'k = ', k, ', Fold - 4','\n', sep = " ")
+ print(table(test.labels4, knn.fit4))
+ 
+ this.err4 <- sum(test.labels4 != knn.fit4) / length(test.labels4)
+ err.rates4 <- rbind(err.rates4, this.err4)
+ }

 k =  1 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          1        15

 k =  2 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          1        15

 k =  3 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          1        15

 k =  4 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          1        15

 k =  5 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          1        15

 k =  6 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          0        16

 k =  7 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          0        16

 k =  8 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          0        16

 k =  9 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          0        16

 k =  10 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          0        16

 k =  11 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          0        16

 k =  12 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          1        15

 k =  13 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          2        14

 k =  14 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          2        14

 k =  15 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          2        14

 k =  16 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          2        14

 k =  17 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          2        14

 k =  18 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          2        14

 k =  19 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  20 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          2        14

 k =  21 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          3        13

 k =  22 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          3        13

 k =  23 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  24 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  25 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  26 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  27 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  28 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  29 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  30 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          3        13

 k =  31 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  32 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          3        13

 k =  33 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  34 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          4        12

 k =  35 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          4        12

 k =  36 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  37 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

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
names(results4) <- c('k'
 k =  38 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          3        13

 k =  39 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          4        12

 k =  40 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          4        12

 k =  41 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  42 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  43 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  44 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          4        12

 k =  45 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  46 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  47 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  48 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  49 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          4        12

 k =  50 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          5        11

 k =  51 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          5        11

 k =  52 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          3         1
  virginica       0          5        11

 k =  53 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          5        11

 k =  54 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          5        11

 k =  55 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          5        11

 k =  56 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          6        10

 k =  57 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          6        10

 k =  58 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          6        10

 k =  59 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          7         9

 k =  60 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          8         8

 k =  61 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          8         8

 k =  62 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          8         8

 k =  63 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          8         8

 k =  64 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          8         8

 k =  65 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0          9         7

 k =  66 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         13         3

 k =  67 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         13         3

 k =  68 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         13         3

 k =  69 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  70 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  71 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  72 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  73 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  74 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  75 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  76 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  77 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  78 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  79 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa         10          0         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  80 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          9          1         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  81 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          3          7         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  82 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          4          6         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  83 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          1          9         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  84 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  85 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  86 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  87 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  88 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  89 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  90 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  91 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  92 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  93 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  94 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  95 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  96 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  97 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  98 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  99 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0

 k =  100 , Fold - 4 
            knn.fit4
test.labels4 setosa versicolor virginica
  setosa          0         10         0
  versicolor      0          4         0
  virginica       0         16         0
> 
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
names(results4) <- c('k', > ## Fold 5 ##
> for (k in 1:max.k) {
+ knn.fit5 <- knn(train.index5, 
+ test.index5,
+ cl = train.labels5,
+ k = k
+ )
+ cat('\n', 'k = ', k, ', Fold - 5','\n', sep = " ")
+ print(table(test.labels5, knn.fit5))
+ 
+ this.err5 <- sum(test.labels5 != knn.fit5) / length(test.labels5)
+ err.rates5 <- rbind(err.rates5, this.err5)
+ }

 k =  1 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  2 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  3 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  4 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  5 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  6 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  7 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  8 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  9 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  10 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  11 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  12 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  13 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  14 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  15 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  16 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  17 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  18 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0         10         0
  virginica       0          0         8

 k =  19 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  20 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  21 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  22 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  23 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  24 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  25 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  26 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  27 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  28 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  29 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  30 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  31 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  32 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  33 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  34 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  35 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0         10         0
  virginica       0          0         8

 k =  36 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  37 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  38 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  39 , Fold - 5 

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

## 1st, c            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  40 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  41 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  42 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  43 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  44 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  45 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  46 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  47 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  48 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  49 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  50 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  51 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  52 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  53 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  54 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  55 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  56 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  57 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  58 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  59 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  60 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  61 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  62 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  63 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  64 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  65 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  66 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  67 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  68 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  69 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  70 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  71 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          8         2
  virginica       0          0         8

 k =  72 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  73 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          9         1
  virginica       0          0         8

 k =  74 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          6         4
  virginica       0          0         8

 k =  75 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          5         5
  virginica       0          0         8

 k =  76 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          5         5
  virginica       0          0         8

 k =  77 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          5         5
  virginica       0          0         8

 k =  78 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          6         4
  virginica       0          0         8

 k =  79 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          7         3
  virginica       0          0         8

 k =  80 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         12          0         0
  versicolor      0          7         3
  virginica       0          0         8

 k =  81 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         11          1         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  82 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa         10          2         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  83 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          5          7         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  84 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          7          5         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  85 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          4          8         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  86 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          4          8         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  87 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          4          8         0
  versicolor      0          1         9
  virginica       0          0         8

 k =  88 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          2         10         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  89 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          1         9
  virginica       0          0         8

 k =  90 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  91 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  92 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  93 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  94 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  95 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  96 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  97 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  98 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          3         7
  virginica       0          0         8

 k =  99 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8

 k =  100 , Fold - 5 
            knn.fit5
test.labels5 setosa versicolor virginica
  setosa          0         12         0
  versicolor      0          2         8
  virginica       0          0         8
> 
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

## 1st, cre> ######################################
> ### Output the results to ggplot2 ####
> ######################################
> 
> ## Create a dataframe for each fold containing a list of the K-Values and the corresponding Generalization Error Rates for each K ##
> results1 <- data.frame(1:max.k, err.rates1)
> results2 <- data.frame(1:max.k, err.rates2)
> results3 <- data.frame(1:max.k, err.rates3)
> results4 <- data.frame(1:max.k, err.rates4)
> results5 <- data.frame(1:max.k, err.rates5)
> 
> ## Append titles to each of columns in the results dataframe ##
> names(results1) <- c('k', 'err.rate1')
> names(results2) <- c('k', 'err.rate2')
> names(results3) <- c('k', 'err.rate3')
> names(results4) <- c('k', 'err.rate4')
> names(results5) <- c('k', 'err.rate5')
> 
> ## Create a title for ggplot of the error rates for each fold ##
> title1 <- paste('knn results Fold 1')
> title2 <- paste('knn results Fold 2')
> title3 <- paste('knn results Fold 3')
> title4 <- paste('knn results Fold 4')
> title5 <- paste('knn results Fold 5')
> 
> ## 1st, create a ggplot showing the Generalization Error Rate per K value, with point and a connecting line ##
> ## 2nd, insert the title of the graph into each plot ##
> results.plot1 <- ggplot(results1, aes(x=k, y=err.rate1)) + geom_point() + geom_line()
> results.plot1 <- results.plot1 + ggtitle(title1)
> 
> results.plot2 <- ggplot(results2, aes(x=k, y=err.rate2)) + geom_point() + geom_line()
> results.plot2 <- results.plot2 + ggtitle(title2)
> 
> results.plot3 <- ggplot(results3, aes(x=k, y=err.rate3)) + geom_point() + geom_line()
> results.plot3 <- results.plot3 + ggtitle(title3)
> 
> results.plot4 <- ggplot(results4, aes(x=k, y=err.rate4)) + geom_point() + geom_line()
> results.plot4 <- results.plot4 + ggtitle(title4)
> 
> results.plot5 <- ggplot(results5, aes(x=k, y=err.rate5)) + geom_point() + geom_line()
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
cat('\n', 'The Overal Average Generalization > results.plot5 <- results.plot5 + ggtitle(title5)
> 
> ## Print out the results of  Generalization Errors for each Fold in the Cross Validation ##
> print(results.plot1)
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


> print(results.plot2)
> print(results.plot3)
> print(results.plot4)
> print(results.plot5)
> 
> ###############################################
> ### Average of the 5 Generalization Errors ####
> ###############################################
> 
> ## Get the average Generalization Error Rate for each Cross Validation Fold ##> ave1 <- sum(results1$err.rate1) / length(results1$err.rate1)
> ave2 <- sum(results2$err.rate2) / length(results2$err.rate2)
> ave3 <- sum(results3$err.rate3) / length(results3$err.rate3)
> ave4 <- sum(results4$err.rate4) / length(results4$err.rate4)
> ave5 <- sum(results5$err.rate5) / length(results5$err.rate5)
> 
> ## Average the Average Generalization Error Rates to determine the overall Generlaization  Error Rate for my KNN analysis ##
> AveGenError <- (ave1 + ave2 + ave3 + ave4 + ave5) / 5
> cat('\n', 'The Overal Average Generalization Error for this KNN Analysis is',AveGenError, '\n', sep = ' ')

 The Overal Average Generalization Error for this KNN Analysis is 0.2019333 
> 
> 
> 
