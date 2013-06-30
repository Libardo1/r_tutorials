# Linear Regression
age <- 18:29
height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5)
length(age)
length(height)
plot(age,height)
# Note that the data in Figure 1 is approximately linear.
# will calculate and plot the Line of Best Fit, or the Least Squares Regression Line.
res=lm(height~age)
res
# These coefficients are the intercept and slope of the line of best fit. Essentially, we are being told that the equation of the line of best fit is:
# height = 0.635 age + 64.928
# The command abline will use the data in res to draw the "Line of Best Fit"
abline(res)
# Now that we have the equation of the line of best fit, we can use the equation
# we wished to estimate the average height of a child at age 27.5 months.
# height = 0.635 age + 64.928 = 0.635(27.5) + 64.928 = 82.3905
0.635*27.5+64.928
# Thus, the average height at age 27.5 months is 82.3905 centimeters.


