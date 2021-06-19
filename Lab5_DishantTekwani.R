#inspecting pre-loaded pressure dataset.
head(pressure)


#Task 1

#a:	Checking the correlation which exists between the variables.
result1 <-cor(pressure$temperature,pressure$pressure)

#b:	Creating a scatterplot of the data which shows both the points and also a smoothed line of the points.
scatter.smooth(x=pressure$temperature, y=pressure$pressure, main="Pressure ~ Temp") # scatter plot

#c:	Creating a side-by-side box-plot of every variable, including the outliers, and then printing observation number of any outliers in console.
par(mfrow=c(1, 2))  # dividing graph area into 2 columns.
boxplot(pressure$temperature, main="Temp") # making box plot for 'Temp'.
boxplot(pressure$pressure, main="Pressure")  # making box plot for 'pressure'.
#printing observation number of any outliers in console.
print('Outliers for temperature are: ')
print(boxplot.stats(pressure$temperature)$out)
#displaying the outliers.
print("Outliers for pressure are: ")
print(boxplot.stats(pressure$pressure)$out)

#d:	Creating a side-by-side graph of the densities of variables.
plot( density(pressure$temperature), main="Density Plot:Temp") #making the density plot for Temperature.
plot(density(pressure$pressure), main="Density Plot:Pressure") #making the density plot for pressure.


#Task 2

#a:	Fitting a simple linear model that predicts the pressure using the temperature. 
pressure.lm <- lm(pressure ~ temperature, data=pressure)  #build linear regression model on full data.
print(pressure.lm)
summary(pressure.lm) #inspect the results.

#b:	Visualizing the resulting regression line on the scatter plot of the data.
par(mfrow=c(1, 1))  #back to 1 plot.
plot(pressure$temperature, pressure$pressure)
abline(pressure.lm)

#c:	Plotting the residual densities.
pressure.res <- resid(pressure.lm) #computing the residuals.

#inspecting the residual density.
plot(density(pressure.res), main="Density Plot: residuals") 

#d:	Using the plot function to generate 4 graphs of the residuals vs. the fitted values, etc. 
plot(pressure.lm) #plotting each individually.
par(mfrow=c(1, 1))  #back to 1 plot.


#Task 3

#a:	Write an equation that describes the linear model you have fitted
#Pressure = -147.899 +  1.512  (temp.).

#b:	Explain why the p-values for the variable temperature and the overall F test are so similar for this model.
#The p-values for the variable temperature and the overall F-test are identical because of the density of variable temperature which
#follows that of bell curve as most correlation tests are designed to produce.

#c:	Comment on what you saw from the previous parts. Is the model appropriate?
# I would say that the model is pretty fair, however an exponential regression model might fit 
#better than the linear omodel which is based on the relationship between the variables.  
#the outliers of the data also seem to have a strong pull over the linear maps of the data.