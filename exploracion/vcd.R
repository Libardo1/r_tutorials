# VCD: A package for analyzing categorical data
library(vcd)
rseed <- 1071
data("Arthritis", package = "vcd")
(art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female"))
mosaic(art)
assoc(art)
data("alzheimer", package = "coin")
alz <- xtabs(~ smoking + disease + gender, data = alzheimer)
alz
data("SexualFun")
agreementplot(t(SexualFun))
data("HairEyeColor")
str(HairEyeColor)
(x <- margin.table(HairEyeColor, c(1, 2)))
assoc(x)
assoc(x, main = "Relation between hair and eye color", shade = TRUE)
(x <- margin.table(HairEyeColor, c(1, 3)))
chisq.test(x)
assoc(x, main = "Relation between hair color and sex", shade = TRUE)
assocstats(x)

