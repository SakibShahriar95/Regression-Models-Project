#load mtcars data set
data(mtcars)

#conver to factor variables
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))
mtcars$carb <- factor(mtcars$carb)

#mpg vs auto/manual plot
boxplot(mpg~am, data = mtcars, xlab = "Transmission Type", ylab = "Miles per Gallon -MPG",
        main = "MPG by Transmission")

#From the boxplot, MPG values generated from manual transmission is much better when compared to automatic
#So that is our hypothesis, we can use T  test now: 
tRes <-t.test(mtcars$mpg~mtcars$am)

#print(tRes)

#Therefore, assuming all variables remain constant, a p-value of 0.001374 which 
#happens to be less than 5%, we can reject our null hypothesis and agree with the hypothesis
#that indeed manual transmission yields a far better mpg.


#Quantification of the MPG difference

#perform a linear regression against all other variables
regr <- lm(mpg~., mtcars)
print(summary(regr))

compare <- step(regr, direction = "both")
print(compare)

#Residual plot
par(mfrow = c(2,2))
plot(compare)
