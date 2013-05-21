# Multiple linear regression
data(stackloss)
str(stackloss)
# Estimated Multiple Regression Equation
# Apply the multiple linear regression model for the data set stackloss, and predict the stack loss if the air flow is 72, water temperature is 20 and acid concentration is 85.
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,data=stackloss)
# We also wrap the parameters inside a new data frame named newdata.
newdata = data.frame(Air.Flow=72,Water.Temp=20,Acid.Conc.=85)
predict(stackloss.lm, newdata)
# Based on the multiple linear regression model and the given parameters, the predicted stack loss is 24.582
# Multiple Coefficient of Determination
# The coefficient of determination of a multiple linear regression model is the quotient of the variances of the fitted values and observed values of the dependent variable
# Find the coefficient of determination for the multiple linear regression model of the data set stackloss.
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,data=stackloss)
summary(stackloss.lm)$r.squared
# The coefficient of determination of the multiple linear regression model for the data set stackloss is 0.91358.
# Significance Test for MLR
# Decide which of the independent variables in the multiple linear regression model of the data set stackloss are statistically significant at .05 significance level.
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,data=stackloss)
summary(stackloss.lm)
# As the p-values of Air.Flow and Water.Temp are less than 0.05, they are both statistically significant in the multiple linear regression model of stackloss.
help(summary.lm)
# Confidence Interval for MLR
# For a given set of values of xk (k = 1, 2, ..., p), the interval estimate for the mean of the dependent variable,  , is called the confidence interval.
# In data set stackloss, develop a 95% confidence interval of the stack loss if the air flow is 72, water temperature is 20 and acid concentration is 85.
attach(stackloss)
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.)
newdata = data.frame(Air.Flow=72,Water.Temp=20,Acid.Conc.=85)
predict(stackloss.lm, newdata, interval="confidence")
detach(stackloss)
# The 95% confidence interval of the stack loss with the given parameters is between 20.218 and 28.945.
# Prediction Interval for MLR
# Assume that the error term ϵ in the multiple linear regression (MLR) model is independent of xk (k = 1, 2, ..., p), and is normally distributed, with zero mean and constant variance. For a given set of values of xk (k = 1, 2, ..., p), the interval estimate of the dependent variable y is called the prediction interval.
# In data set stackloss, develop a 95% prediction interval of the stack loss if the air flow is 72, water temperature is 20 and acid concentration is 85.
attach(stackloss)
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.)
newdata = data.frame(Air.Flow=72,Water.Temp=20,Acid.Conc.=85)
predict(stackloss.lm, newdata, interval="predict")
detach(stackloss)
# The 95% confidence interval of the stack loss with the given parameters is between 16.466 and 32.697.
