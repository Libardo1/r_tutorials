# Association Rules: arules provides basic infrastructure for creating and manipulating
# input data sets and for analyzing the resulting itemssets and rules
# arules includes apriori and eclat algorithms for mining frequent itemsets, maximal frequent
# itemsets, closed frequent itemsets and association rules
# Examples available at: http://cran.r-project.org/web/packages/arules/vignettes/arules.pdf

library(arules)

##### Epub example #####

data("Epub")
Epub
summary(Epub)

# extracting elements using indexes
id <- transactionInfo(Epub)[["transactionID"]]
str(id)

# timestamp conversion and frequency table
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)
Epub2003 <- Epub[year == "2003"]
length(Epub2003)
image(Epub2003)

# size function
transactionInfo(Epub2003[size(Epub2003) > 20])

# inspect transactions
inspect(Epub2003[1:5])

# convert transactions into a list
as(Epub2003[1:5], "list")

# conversion of transaction data: from horizontal to vertical layout
EpubTidLists <- as(Epub, "tidLists")
EpubTidLists
as(EpubTidLists[1:3], "list")


##### Preparing and mining a questionnaire Example #####

data("AdultUCI")
dim(AdultUCI)
class(AdultUCI)

# remove some variables
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

# mapping metric attributes to ordinal attributes
AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)),labels=c("Young","Middle-aged","Senior","Old"))
AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]], c(0,25,40,60,168)),labels=c("Part-Time","Full-Time","Over-Time","Workaholic"))
AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),labels=c("None","Low","High"))
AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]], c(-Inf,0,median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),labels=c("none","low","high"))

str(AdultUCI)

# converting data to binary incidence matrix by coercing data set to transactions
Adult <- as(AdultUCI, "transactions")
Adult
summary(Adult)
itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)

# call apriori to find rules
rules <- apriori(Adult,parameter = list(support = 0.01, confidence = 0.6))
rules
summary(rules)

# subseting and exporting data
rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)

inspect(head(sort(rulesIncomeSmall, by = "confidence"), n = 3))
inspect(head(sort(rulesIncomeLarge, by = "confidence"), n = 3))

write(rulesIncomeSmall, file = "data.csv", sep = ",", col.names = NA)
write.PMML(rulesIncomeSmall, file = "data.xml")

##### Arules Viz #####

library(arulesViz)
data("Groceries")
summary(Groceries)

# Mining association rules using the Apriori algorithm
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules

# Top three rules with respect to the lift measure 
inspect(head(sort(rules, by ="lift"),3))

# Plotting rules
plot(x, method = NULL, measure = "support", shading = "lift", interactive = FALSE, data)
plot(rules)
plot(rules, measure=c("support", "lift"), shading="confidence")
plot(rules, shading="order", control=list(main = "Two-key plot"))

# Interactive plotting
sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

# Matrix based visualizations
subrules <- rules[quality(rules)$confidence > 0.8]
subrules
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

# Grouped matrix based visualizations
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=50))
sel <- plot(rules, method="grouped", interactive=TRUE)

# Graph based visualizations
subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")
plot(subrules2, method="graph",control=list(type="items"))

# Export graph as graphml
saveAsGraph(head(sort(rules, by="lift"),1000), file="rules.graphml")

# Parallel coordinates
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

# Double decker plot
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = Groceries)
