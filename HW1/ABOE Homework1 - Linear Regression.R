Last login: Thu Jul  4 10:30:26 on ttys001
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

> train <- mtcars
> summary(train)
      mpg             cyl             disp             hp       
 Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
 1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
 Median :19.20   Median :6.000   Median :196.3   Median :123.0  
 Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
 3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
 Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
      drat             wt             qsec             vs        
 Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
 1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
 Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
 Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
 3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
 Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
       am              gear            carb      
 Min.   :0.0000   Min.   :3.000   Min.   :1.000  
 1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
 Median :0.0000   Median :4.000   Median :2.000  
 Mean   :0.4062   Mean   :3.688   Mean   :2.812  
 3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
 Max.   :1.0000   Max.   :5.000   Max.   :8.000  
> head(train)
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
> model.1<- lm(mpg ~ 0 + cyl + hp + wt + gear, data=train)
> summary(model.1)

Call:
lm(formula = mpg ~ 0 + cyl + hp + wt + gear, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.7086 -3.0381  0.1477  2.9264  8.2698 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
cyl   1.79256    0.77569   2.311   0.0284 *  
hp   -0.08862    0.01728  -5.129 1.95e-05 ***
wt   -0.35297    1.11321  -0.317   0.7535    
gear  6.21707    0.46242  13.445 9.74e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.88 on 28 degrees of freedom
Multiple R-squared:   0.97,	Adjusted R-squared:  0.9657 
F-statistic: 226.1 on 4 and 28 DF,  p-value: < 2.2e-16

> model.2<-update(model.1, .~. -wt)
> summary(model.2)

Call:
lm(formula = mpg ~ cyl + hp + gear - 1, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.8202 -3.0056 -0.1566  2.9708  8.3768 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
cyl   1.60876    0.50739   3.171  0.00358 ** 
hp   -0.08829    0.01698  -5.200 1.45e-05 ***
gear  6.20668    0.45405  13.670 3.59e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.82 on 29 degrees of freedom
Multiple R-squared:  0.9699,	Adjusted R-squared:  0.9668 
F-statistic: 311.1 on 3 and 29 DF,  p-value: < 2.2e-16

> model.2

Call:
lm(formula = mpg ~ cyl + hp + gear - 1, data = train)

Coefficients:
     cyl        hp      gear  
 1.60876  -0.08829   6.20668  

> model.simplified<-lm(mpg ~ 0 + gear, data=train)
> summary(model.simplified)

Call:
lm(formula = mpg ~ 0 + gear, data = train)

Residuals:
     Min       1Q   Median       3Q      Max 
-11.9569  -1.9970  -0.3655   2.8824  12.3345 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
gear    5.391      0.254   21.23   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.4 on 31 degrees of freedom
Multiple R-squared:  0.9356,	Adjusted R-squared:  0.9335 
F-statistic: 450.5 on 1 and 31 DF,  p-value: < 2.2e-16

> model.simplified.2<-update(model.simplified, .~. +hp)
> summary(model.simplified.2)

Call:
lm(formula = mpg ~ gear + hp - 1, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-7.9936 -2.0516  0.7182  3.7526  8.6747 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
gear  6.99437    0.43363  16.130  2.5e-16 ***
hp   -0.04234    0.01009  -4.195 0.000223 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.358 on 30 degrees of freedom
Multiple R-squared:  0.9594,	Adjusted R-squared:  0.9567 
F-statistic: 354.7 on 2 and 30 DF,  p-value: < 2.2e-16

> model.simplified.3<-update(model.simplified.2, .~. +vs)
> summary(model.simplified.3)

Call:
lm(formula = mpg ~ gear + hp + vs - 1, data = train)

Residuals:
   Min     1Q Median     3Q    Max 
-6.833 -2.108  0.994  2.870  7.906 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
gear  5.73071    0.66674   8.595 1.82e-09 ***
hp   -0.02347    0.01229  -1.910   0.0661 .  
vs    4.59717    1.93128   2.380   0.0241 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.054 on 29 degrees of freedom
Multiple R-squared:  0.9661,	Adjusted R-squared:  0.9625 
F-statistic: 275.1 on 3 and 29 DF,  p-value: < 2.2e-16

> model.simplified.3<-update(model.simplified.2, .~. +wt)
> model.simplified.3<-update(model.simplified.2, .~. +vs)
> summary(model.simplified.3)

Call:
lm(formula = mpg ~ gear + hp + vs - 1, data = train)

Residuals:
   Min     1Q Median     3Q    Max 
-6.833 -2.108  0.994  2.870  7.906 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
gear  5.73071    0.66674   8.595 1.82e-09 ***
hp   -0.02347    0.01229  -1.910   0.0661 .  
vs    4.59717    1.93128   2.380   0.0241 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.054 on 29 degrees of freedom
Multiple R-squared:  0.9661,	Adjusted R-squared:  0.9625 
F-statistic: 275.1 on 3 and 29 DF,  p-value: < 2.2e-16

> model.simplified.4<-update(model.simplified.3, .~. +wt)
> summary(model.simplified.4)

Call:
lm(formula = mpg ~ gear + hp + vs + wt - 1, data = train)

Residuals:
   Min     1Q Median     3Q    Max 
-7.140 -3.007  0.905  2.502  8.277 

Coefficients:
     Estimate Std. Error t value Pr(>|t|)    
gear  5.65137    0.65903   8.575 2.55e-09 ***
hp   -0.04237    0.01826  -2.320   0.0278 *  
vs    3.74163    1.99995   1.871   0.0718 .  
wt    1.10592    0.80035   1.382   0.1780    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.992 on 28 degrees of freedom
Multiple R-squared:  0.9682,	Adjusted R-squared:  0.9637 
F-statistic: 213.3 on 4 and 28 DF,  p-value: < 2.2e-16

> model.simplified.5<-update(model.simplified.4, .~. +qsec)
> summary(model.simplified.5)

Call:
lm(formula = mpg ~ gear + hp + vs + wt + qsec - 1, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.3551 -1.7063 -0.4372  1.0386  5.5018 

Coefficients:
      Estimate Std. Error t value Pr(>|t|)    
gear  1.979214   0.765193   2.587 0.015406 *  
hp   -0.007471   0.013651  -0.547 0.588710    
vs   -1.759733   1.639092  -1.074 0.292503    
wt   -4.031034   1.025093  -3.932 0.000529 ***
qsec  1.546089   0.262415   5.892 2.82e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.689 on 27 degrees of freedom
Multiple R-squared:  0.9861,	Adjusted R-squared:  0.9835 
F-statistic:   383 on 5 and 27 DF,  p-value: < 2.2e-16

> model.simplified.6<-update(model.simplified.5, .~. +drat)
> summary(model.simplified.6)

Call:
lm(formula = mpg ~ gear + hp + vs + wt + qsec + drat - 1, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.6517 -1.4757 -0.5241  1.0983  5.3700 

Coefficients:
      Estimate Std. Error t value Pr(>|t|)    
gear  1.080175   1.018812   1.060 0.298790    
hp   -0.008926   0.013516  -0.660 0.514825    
vs   -1.465794   1.632826  -0.898 0.377582    
wt   -3.666511   1.048878  -3.496 0.001715 ** 
qsec  1.299916   0.319578   4.068 0.000392 ***
drat  1.843418   1.402446   1.314 0.200179    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.654 on 26 degrees of freedom
Multiple R-squared:  0.987,	Adjusted R-squared:  0.984 
F-statistic: 328.1 on 6 and 26 DF,  p-value: < 2.2e-16

> 
