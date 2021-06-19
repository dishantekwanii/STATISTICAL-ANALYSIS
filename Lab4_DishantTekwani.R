OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  # We calculate the test statistic below.
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    # Get the p-value for this test statistic below.
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    # Define df.
    df <- n-1  
    # Get the p-value of this test statistic.
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  # Check if it is significant.
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )
  # Return the list.
  return( ret )
}



P <- rnorm(100,4,5) #making a normal destribution
OneSampTest("z", "left" ,0.05, 0, 100, 4, 5)
#doing a z test with true population sd using alpha 0.05

# The Second task!
Psample <-sample(P,10) #taking a random sample of size 10
sampleSD<- sd(Psample) #standard deviation of the sample
OneSampTest("t", "right" ,0.025, 4.2, 10, 4, sampleSD)
# Do a t test with sample sd and alpha 0.025
# By analyzing the result we fail to reject the hypotheses since p_value > 0.5



#Part C

#1)
#Load the csv file survey.csv in R. 
Lab4<- read.csv(file='survey.csv', header = TRUE)
#Creates contingency table for the variable Smoke and Ex 
table1 <- table(survey$Smoke, survey$Exer)
#run chi square test
chisq.test(table1)
#run fisher's test in order to test hypothesis because expected count is not high enough for chisquared 
result <- fisher.test(table1)

#2
#The code gave an error message because the chi-squared statistic only approximately follows the
#distribution of chi-squared, if there were more observations the test would be more accurate
#the expected counts must be less than 5 for the error message to appear.
#The null hypothesis being tested is that there is no correlation between the two variables Smoke and Exer, 
#they are independent, thus there smoking is independent with exercise.
#The alternative hypothesis is that there is a correlation between smoking and exercising.
#The p-value resulting from the test is 0.414, using a significance level of .05 and 11 degrees of freedom
# since the critical value, 19.68 is much larger than the value resulting from the test, we can affirm the null hypothesis


