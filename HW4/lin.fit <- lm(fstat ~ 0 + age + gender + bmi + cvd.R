data <- read.table("heart_attack_mod1.txt", h=T)

lin.fit <- lm(fstat ~ 0 + age + gender + bmi + cvd + sho + chf + av3 + miord + lenfol, data=data)
summary(lin.fit)
# Rsq of .4116 is too low.  Look to improve by removing those variables with abs t-values < 1.
# start with the lowest T-value, in this case, Gender with 0.123

lin.fit2 <- update(lin.fit, .~. -gender)
summary(lin.fit2)

#still have two values with an abs t-value less than 1: sho and cdf (cardiogenic shock and congestive heart failure)
#remove the cvd becuase it's t-value of -0.232 is less signifcant than cdf.
lin.fit3 <- update(lin.fit2, .~. -cvd)
summary(lin.fit3)

#remove the sho variable becuase it's t-value of 0.285 is less than 1.
lin.fit4 <- update(lin.fit3, .~. -sho)
summary(lin.fit4)

lin.fit5 <- update(lin.fit4, .~. -av3)
summary(lin.fit5)

plot(resid(lin.fit5))
qqnorm(resid(lin.fit5))