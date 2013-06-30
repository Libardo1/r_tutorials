#### Data Analysis with R (mainly ggplot2) ####
### R Basics ###
# Installing a package
install.packages(c("ggplot2", "gcookbook"))
# Loading the package
library(ggplot2)
library(gcookbook)
library(MASS)
library(plyr)
library(xlsx)
library(hexbin)
# Loading a Delimited Text Data File
data <- read.csv("a1.csv", header=TRUE)
# Head
head(data)
# str of data.frame
str(data)
# Convert to factor
data$X <- factor(data$X)
str(data)
# Loading Data from an Excel File
install.packages("xlsx")
library(xlsx)
data <- read.xlsx("espout.xls", 2)
str(data)
# Loading Data from an SPSS File
install.packages("foreign")
data <- read.spss("datafile.sav")
# Other formats supported by package "foreign"
read.octave()
read.systat()
read.xport()
read.dta()
### Exploring Data ###
## Scatter plot##
plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
library(gcookbook)
# List the two columns we'll use
heightweight[, c("ageYear", "heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=21)
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(size=2.5,shape=21)
# Grouping Data Points by a Variable Using Shape or Color
heightweight[, c("sex", "ageYear", "heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) + geom_point() + scale_shape_manual(values=c(1,2)) + scale_colour_brewer(palette="Set1")
# Using Different Point Shapes
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=3)
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + geom_point(size=3) + scale_shape_manual(values=c(1, 4))
# Mapping a Continuous Variable to Color or Size
heightweight[, c("sex", "ageYear", "heightIn", "weightLb")]
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()
ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) + geom_point(shape=21, size=2.5) + scale_fill_gradient(low="black", high="white")
ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) + geom_point(shape=21, size=2.5) + scale_fill_gradient(low="black", high="white", breaks=12:17,guide=guide_legend())
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) + geom_point(alpha=.5) + scale_size_area() + scale_colour_brewer(palette="Set1")
library(hexbin)
sp + stat_binhex() + scale_fill_gradient(low="lightblue", high="red",limits=c(0, 8000))
# Dealing with Overplotting
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + geom_point()
sp + geom_point(alpha=.1)
sp + geom_point(alpha=.01)
# Bin points
sp + stat_bin2d()
sp + stat_bin2d(bins=50) + scale_fill_gradient(low="lightblue", high="red", limits=c(0, 6000))
# Jitter
sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point()
sp1 + geom_point(position="jitter")
sp1 + geom_point(position=position_jitter(width=.5, height=0))
# Adding Fitted Regression Model Lines
library(gcookbook)
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp + geom_point() + stat_smooth(method=lm)
# 99% confidence region
sp + geom_point() + stat_smooth(method=lm, level=0.99)
# No confidence region
sp + geom_point() + stat_smooth(method=lm, se=FALSE)
sp + geom_point(colour="grey60") + stat_smooth(method=lm, se=FALSE, colour="black")
sp + geom_point(colour="grey60") + stat_smooth()
sp + geom_point(colour="grey60") + stat_smooth(method=loess)
# Adding Marginal Rugs to a Scatter Plot
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug()
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug(position="jitter", size=.2)
# Labeling Points in a Scatter Plot
library(gcookbook)
subset(countries, Year==2009 & healthexp>2000)
sp <- ggplot(subset(countries, Year==2009 & healthexp>2000),aes(x=healthexp, y=infmortality)) + geom_point()
sp + annotate("text", x=4350, y=5.4, label="Canada") + annotate("text", x=7400, y=6.8, label="USA")
sp + geom_text(aes(label=Name), size=4)
sp + geom_text(aes(label=Name), size=4, vjust=0)
sp + geom_text(aes(y=infmortality+.1, label=Name), size=4, vjust=0)
# Label just some of the points 19/2/2013
cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States","New Zealand", "Iceland", "Japan", "Luxembourg","Netherlands", "Switzerland")
idx
cdat$Name1[!idx] <- NA
cdat
ggplot(cdat, aes(x=healthexp, y=infmortality)) + geom_point() + geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) + xlim(2000, 10000)
# Making a Scatter Plot Matrix
library(gcookbook)
c2009 <- subset(countries, Year==2009, select=c(Name, GDP, laborrate, healthexp, infmortality))
c2009
pairs(c2009[,2:5])
## Line Graphs ##
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="red")
qplot(pressure$temperature, pressure$pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()
qplot(temperature, pressure, data=pressure, geom=c("line", "point"))
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD1 <- BOD # Make a copy of the data
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
# Adding Points to a Line Graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
library(gcookbook)
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
# Same with a log y-axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10()
# Making a Line Graph with Multiple Lines
library(plyr)
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
# Map supp to colour
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
# Map supp to linetype
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() + geom_point(size=4, shape=21)
# Changing the Appearance of Lines
ggplot(BOD, aes(x=Time, y=demand)) + geom_line(linetype="dashed", size=1, colour="blue")
# Changing the Appearance of Points
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point(size=4, shape=22, colour="darkred", fill="pink")
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point(size=4, shape=21, fill="white")
library(plyr)
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
pd <- position_dodge(0.2)
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line(position=pd) + geom_point(shape=21, size=3, position=pd) + scale_fill_manual(values=c("black","white"))
# Making a Graph with a Shaded Area
sunspotyear <- data.frame(Year = as.numeric(time(sunspot.year)),Sunspots = as.numeric(sunspot.year))
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area(colour="black", fill="blue", alpha=.2)
# Making a Stacked Area Graph
library(gcookbook)
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area(colour="black", size=.2, alpha=.4) + scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))
# Adding a Confidence Region
clim <- subset(climate, Source == "Berkeley",select=c("Year", "Anomaly10y", "Unc10y"))
ggplot(clim, aes(x=Year, y=Anomaly10y)) + geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y),alpha=0.2) + geom_line()
# With a dotted line for upper and lower bounds
ggplot(clim, aes(x=Year, y=Anomaly10y)) + geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dotted") + geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") + geom_line()
## Bar Graphs ##
barplot(BOD$demand, names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(BOD$Time, BOD$demand, geom="bar", stat="identity")
qplot(factor(BOD$Time), BOD$demand, geom="bar", stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
# There's no entry for Time == 6
BOD
# Time is numeric (continuous)
str(BOD)
# Convert Time to a discrete (categorical) variable with factor()
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")
library(gcookbook)
cabbage_exp
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position="dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position="dodge", colour="black") + scale_fill_brewer(palette="Pastel1")
# Using colors
upc <- subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) + geom_bar(stat="identity", colour="black") + scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("State")
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity")
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity", colour="black", size=0.25) + scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)
# Adjusting Bar Width and Spacing
library(gcookbook)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=1)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", width=0.5, position="dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))
# Making a Stacked Bar Graph
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity") + guides(fill=guide_legend(reverse=TRUE))
# Adding Labels to a Bar Graph
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=1.5, colour="white")
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=-0.2)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", position="dodge") + geom_text(aes(label=Weight), vjust=1.5, colour="white", position=position_dodge(.9), size=3)
# Adding Labels to a Stacked Bar Graph
library(plyr)
# Sort by the day and sex columns
ce <- arrange(cabbage_exp, Date, Cultivar)
# Get the cumulative sum
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight))
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity") + geom_text(aes(y=label_y, label=Weight), vjust=1.5, colour="white")
# Making a Cleveland Dot Plot
library(gcookbook)
tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
## Histograms ##
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)
qplot(mtcars$mpg)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(faithful, aes(x=waiting)) + geom_histogram()
h <- ggplot(faithful, aes(x=waiting))
h + geom_histogram(binwidth=8, fill="white", colour="black", origin=31)
h + geom_histogram(binwidth=8, fill="white", colour="black", origin=35)
# Making Multiple Histograms from Grouped Data
library(MASS)
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") + facet_grid(smoke ~ .)
# Making a Density Curve
ggplot(faithful, aes(x=waiting)) + geom_density()
ggplot(faithful, aes(x=waiting)) + geom_line(stat="density") + expand_limits(y=0)
# Making Multiple Density Curves from Grouped Data
# Make a copy of the data 
birthwt1 <- birthwt
# Convert smoke to a factor
birthwt1$smoke <- factor(birthwt1$smoke)
# Map smoke to colour
ggplot(birthwt1, aes(x=bwt, colour=smoke)) + geom_density()
# Facet density
ggplot(birthwt1, aes(x=bwt)) + geom_density() + facet_grid(smoke ~ .)
## Box Plot ##
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
# Making a Violin Plot
p <- ggplot(heightweight, aes(x=sex, y=heightIn))
p + geom_violin()
p + geom_violin() + geom_boxplot(width=.1, fill="black", outlier.colour=NA) + stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
p + geom_violin(trim=FALSE)
# More smoothing
p + geom_violin(adjust=2)
# Less smoothing
p + geom_violin(adjust=.5)
# Making a Dot Plot
countries2009 <- subset(countries, Year==2009 & healthexp>2000)
p <- ggplot(countries2009, aes(x=infmortality))
p + geom_dotplot()
p + geom_dotplot(binwidth=.25) + geom_rug() + scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
p + geom_dotplot(method="histodot", binwidth=.25) + geom_rug() + scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
# Making Multiple Dot Plots for Grouped Data
ggplot(heightweight, aes(x=sex, y=heightIn)) + geom_dotplot(binaxis="y", binwidth=.5, stackdir="center")
ggplot(heightweight, aes(x=sex, y=heightIn)) + geom_boxplot(outlier.colour=NA, width=.4) + geom_dotplot(binaxis="y", binwidth=.5, stackdir="center", fill=NA)
# Making a Density Plot of Two-Dimensional Data
p <- ggplot(faithful, aes(x=eruptions, y=waiting))
p + geom_point() + stat_density2d()
p + stat_density2d(aes(colour=..level..))
p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
p + geom_point() + stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE) + theme_bw()
p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE, h=c(.5,5))
## Annotations ##
# Adding Lines 
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
p + geom_hline(yintercept=60) + geom_vline(xintercept=14)
p + geom_abline(intercept=37.4, slope=1.75)
# Adding a Shaded Rectangle
p <- ggplot(subset(climate, Source=="Berkeley"), aes(x=Year, y=Anomaly10y)) + geom_line()
p + annotate("rect", xmin=1950, xmax=1980, ymin=-1, ymax=1, alpha=.1,fill="blue")
## Axes ##
# Swapping X- and Y-Axes
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip()
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip() +scale_x_discrete(limits=rev(levels(PlantGrowth$group)))
# Limit Axes
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p
p + ylim(0, max(PlantGrowth$weight))
# Setting the Scaling Ratio of the X- and Y-Axes
sp <- ggplot(marathon, aes(x=Half,y=Full)) + geom_point()
sp + coord_fixed()
sp + coord_fixed() + scale_y_continuous(breaks=seq(0, 420, 30)) + scale_x_continuous(breaks=seq(0, 420, 30))
sp + coord_fixed(ratio=1/2) + scale_y_continuous(breaks=seq(0, 420, 30)) + scale_x_continuous(breaks=seq(0, 420, 15))
# Setting the Positions of Tick Marks
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6, 8))
# Removing Tick Marks and Labels
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p + theme(axis.text.y = element_blank())
p + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p + scale_y_continuous(breaks=NULL)
# Changing the Text of Axis Labels
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
hwp
hwp + xlab("Age in years") + ylab("Height in inches")
hwp + theme(axis.title.x=element_blank())
# Using a Logarithmic Axis
p <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) + geom_text(size=3)
p
p + scale_x_log10() + scale_y_log10()
# Making a Circular Graph
ggplot(wind, aes(x=DirCat, fill=SpeedCat)) + geom_histogram(binwidth=15, origin=-7.5) + coord_polar() + scale_x_continuous(limits=c(0,360))
# Using Dates on an Axis
str(economics)
ggplot(economics, aes(x=date, y=psavert)) + geom_line()
econ <- subset(economics, date >= as.Date("1992-05-01") & date <  as.Date("1993-06-01"))
p <- ggplot(econ, aes(x=date, y=psavert)) + geom_line()
p
datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by="2 month")
p + scale_x_date(breaks=datebreaks) + theme(axis.text.x = element_text(angle=30, hjust=1))
library(scales)
p + scale_x_date(breaks=datebreaks, labels=date_format("%Y %b")) + theme(axis.text.x = element_text(angle=30, hjust=1))

# Ordering points in ggplot
ggplot(order2, aes(x=Romney, y=reorder(Term, Romney))) + geom_point(size=3) 


