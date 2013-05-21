# tabplot: A tableplot is a visualisation of a (large) dataset with a dozen of variables, both numeric and categorical.
# Each column represents a variable and each row bin is an aggregate of a certain number of records. 
# Numeric variables are visualized as bar charts, and categorical variables as stacked bar charts.
# Missing values are taken into account.
# Main Functions: tableplot, itableplot, tablePrepare, tablePalettes, tableSave, tableChange
library(tabplot)
require(ggplot2)
data(diamonds)
tableplot(diamonds)
# Transform a date-time vector from class POSIXt or Date to a factor.
d <- as.Date("2012-12-21") + sample.int(500, 1000, replace=TRUE)
d2 <- datetime2fac(d)
levels(d2)
# itableplot
data(diamonds)
data(iris)
data(cars)
itableplot()
# Transform a numerical vector from class POSIXt or Date to a factor.
library(classInt)
data(diamonds)
diamonds$price2 <- num2fac(diamonds$price)
tableplot(diamonds)
# plot.tabplot: load diamonds dataset from ggplot2
data(diamonds)
tab <- tableplot(diamonds)
plot(tab, title="Shine on you Crazy Diamond!!!", fontsize=12, legend.lines=7, fontsize.title=16)
# Getting started with the tableplot function
data(diamonds)
## Add some NA's
is.na(diamonds$price) <- diamonds$cut == "Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds)) > 0.8)
tableplot(diamonds)
tableplot(diamonds, select = c(carat, price,cut, color, clarity), sortCol = price)
# Zooming and filtering
tableplot(diamonds, select = c(carat, price,cut, color, clarity), sortCol = price,from = 0, to = 5)
tableplot(diamonds, subset = price < 5000 & cut == "Premium")
# Color palletes
tablePalettes()
tableplot(diamonds, pals = list(cut = "Set1(6)",color = "Set5", clarity = rainbow(8)))
# High cardinality data
diamonds$carat_class <- num2fac(diamonds$carat, n = 20)
diamonds$price_class <- num2fac(diamonds$price,n = 100)
tableplot(diamonds, select = c(carat, price,carat_class, price_class))
# The tabplot object
tab <- tableplot(diamonds, plot = FALSE)
summary(tab)

